open! Core
open! Async
open! Openrouter_api

let parse_key_value ~flag s =
  match String.lsplit2 s ~on:'=' with
  | Some (key, value) when not (String.is_empty key) -> Ok (key, value)
  | _ -> Or_error.errorf "%s expects KEY=VALUE" flag
;;

let parse_key_json ~flag s =
  let%map.Or_error key, value = parse_key_value ~flag s in
  key, Or_error.ok (Jsonaf.parse value) |> Option.value ~default:(`String value)
;;

let parse_key_int ~flag s =
  let%bind.Or_error key, value = parse_key_value ~flag s in
  let%map.Or_error value =
    Or_error.tag_arg
      (Or_error.try_with (fun () -> Int.of_string value))
      "invalid integer value"
      value
      [%sexp_of: string]
  in
  key, value
;;

let key_values_or_error ~flag values =
  Or_error.combine_errors (List.map values ~f:(parse_key_value ~flag))
;;

let key_json_values_or_error ~flag values =
  Or_error.combine_errors (List.map values ~f:(parse_key_json ~flag))
;;

let key_int_values_or_error ~flag values =
  Or_error.combine_errors (List.map values ~f:(parse_key_int ~flag))
;;

module Message_input = struct
  type t =
    { message : string option
    ; files : string list
    ; cache_system_prompt : string option
    }
  [@@deriving sexp]

  let param =
    let%map_open.Command files =
      flag "file" (listed string) ~doc:"PATH attach a file (PDF or image)"
    and cache_system_prompt =
      flag
        "cache-system-prompt"
        (optional string)
        ~doc:
          "TEXT prepend a cached system-prompt content part with cache_control = \
           ephemeral (Anthropic-style prompt caching)"
    and message = anon (maybe ("MESSAGE" %: string)) in
    { message; files; cache_system_prompt }
  ;;

  let mime_type_of_extension ext =
    match String.lowercase ext with
    | ".pdf" -> Some "application/pdf"
    | ".png" -> Some "image/png"
    | ".jpg" | ".jpeg" -> Some "image/jpeg"
    | ".gif" -> Some "image/gif"
    | ".webp" -> Some "image/webp"
    | ".mp3" -> Some "audio/mpeg"
    | ".wav" -> Some "audio/wav"
    | ".ogg" -> Some "audio/ogg"
    | ".flac" -> Some "audio/flac"
    | ".m4a" -> Some "audio/mp4"
    | ".mp4" -> Some "video/mp4"
    | ".mov" -> Some "video/quicktime"
    | ".webm" -> Some "video/webm"
    | _ -> None
  ;;

  let read_file_as_data_url path =
    let ext = Filename.split_extension path |> snd |> Option.value ~default:"" in
    let ext = if String.is_prefix ext ~prefix:"." then ext else "." ^ ext in
    match mime_type_of_extension ext with
    | None -> Deferred.Or_error.errorf "Unsupported file type: %s" ext
    | Some mime_type ->
      let%bind content = Reader.file_contents path in
      let base64 = Base64.encode_exn content in
      let data_url = sprintf "data:%s;base64,%s" mime_type base64 in
      Deferred.Or_error.return (mime_type, data_url)
  ;;

  let message_text t =
    match t.message with
    | Some message -> return message
    | None -> Reader.contents (force Reader.stdin)
  ;;

  let to_request_message t ~message =
    let cache_system_part =
      Option.map t.cache_system_prompt ~f:(fun text ->
        Completions.Request.Message.Content_part.text
          ~cache_control:
            (Completions.Request.Message.Content_part.Cache_control.ephemeral ())
          text)
    in
    match t.files, cache_system_part with
    | [], None -> Deferred.Or_error.return (Completions.Request.Message.user message)
    | files, cache_system_part ->
      let%bind.Deferred.Or_error file_parts =
        Deferred.Or_error.List.map files ~how:`Sequential ~f:(fun path ->
          let%bind.Deferred.Or_error mime_type, data_url = read_file_as_data_url path in
          let filename = Filename.basename path in
          let base64_data () =
            String.chop_prefix_exn data_url ~prefix:(sprintf "data:%s;base64," mime_type)
          in
          match String.lsplit2 mime_type ~on:'/' with
          | Some ("image", _) ->
            Deferred.Or_error.return
              (Completions.Request.Message.Content_part.image_base64
                 ~mime_type
                 ~data:(base64_data ())
                 ())
          | Some ("audio", subtype) ->
            let format =
              match subtype with
              | "mpeg" -> "mp3"
              | "mp4" -> "m4a"
              | s -> s
            in
            Deferred.Or_error.return
              (Completions.Request.Message.Content_part.audio
                 ~format
                 ~data:(base64_data ())
                 ())
          | Some ("video", _) ->
            Deferred.Or_error.return
              (Completions.Request.Message.Content_part.video_base64
                 ~mime_type
                 ~data:(base64_data ())
                 ())
          | _ ->
            Deferred.Or_error.return
              (Completions.Request.Message.Content_part.file
                 ~filename
                 ~file_data:data_url
                 ()))
      in
      let text_part = Completions.Request.Message.Content_part.text message in
      let all_parts = Option.to_list cache_system_part @ (text_part :: file_parts) in
      Deferred.Or_error.return (Completions.Request.Message.user_multipart all_parts)
  ;;
end

module Output_format = struct
  type t =
    | Json_object
    | Json_schema_file of string
  [@@deriving sexp]

  let param =
    let%map_open.Command json =
      flag "json" no_arg ~doc:" force JSON object output (response_format=json_object)"
    and json_schema_file =
      flag
        "json-schema"
        (optional Filename_unix.arg_type)
        ~doc:"FILE force output matching the JSON schema in FILE"
    in
    match json, json_schema_file with
    | true, Some _ -> Or_error.error_string "cannot pass both -json and -json-schema"
    | true, None -> Ok (Some Json_object)
    | false, Some path -> Ok (Some (Json_schema_file path))
    | false, None -> Ok None
  ;;

  let resolve = function
    | None -> Deferred.Or_error.return None
    | Some Json_object ->
      Deferred.Or_error.return (Some Completions.Request.Response_format.Json_object)
    | Some (Json_schema_file path) ->
      let%map contents = Reader.file_contents path in
      let%map.Or_error schema = Jsonaf.parse contents in
      let name = Filename.basename path |> Filename.chop_extension in
      Some (Completions.Request.Response_format.json_schema ~name ~strict:true ~schema ())
  ;;
end

module Reasoning = struct
  type t = Completions.Request.Reasoning.t option [@@deriving sexp]

  let param =
    let%map_open.Command reasoning_enabled =
      flag "reasoning-enabled" no_arg ~doc:" enable reasoning with provider defaults"
    and reasoning_tokens =
      flag
        "reasoning"
        (optional int)
        ~doc:"TOKENS enable reasoning with max tokens budget"
    and reasoning_effort =
      flag
        "reasoning-effort"
        (optional Completions.Request.Reasoning.Effort.arg_type)
        ~doc:
          "EFFORT reasoning effort (xhigh|high|medium|low|minimal|none); mutually \
           exclusive with -reasoning"
    and reasoning_exclude =
      flag "reasoning-exclude" no_arg ~doc:" hide reasoning from response output"
    and reasoning_summary =
      flag
        "reasoning-summary"
        (optional string)
        ~doc:"VERBOSITY reasoning summary verbosity (auto|concise|detailed)"
    in
    match
      ( reasoning_enabled
      , reasoning_tokens
      , reasoning_effort
      , reasoning_exclude
      , reasoning_summary )
    with
    | false, None, None, false, None -> Ok None
    | enabled, max_tokens, effort, exclude, summary ->
      let enabled = Option.some_if enabled true in
      let exclude = Option.some_if exclude true in
      let%map.Or_error reasoning =
        Completions.Request.Reasoning.create
          ?effort
          ?max_tokens
          ?exclude
          ?enabled
          ?summary
          ()
      in
      Some reasoning
  ;;
end

module Tool_choice = struct
  type t = Completions.Tool_choice.t option [@@deriving sexp]

  let param =
    Command.Param.flag
      "tool-choice"
      (Command.Param.optional Completions.Tool_choice.arg_type)
      ~doc:"CHOICE tool choice: auto, none, required, or function:NAME"
  ;;
end

module Logit_bias = struct
  type t = Completions.Request.Logit_bias.t option [@@deriving sexp]

  let param =
    let%map_open.Command logit_bias =
      flag
        "logit-bias"
        (listed string)
        ~doc:"TOKEN_ID=BIAS token logit bias in [-100,100] (may be repeated)"
    in
    match logit_bias with
    | [] -> Ok None
    | _ :: _ ->
      let%map.Or_error logit_bias =
        key_int_values_or_error ~flag:"-logit-bias" logit_bias
      in
      Some logit_bias
  ;;
end

module Plugins = struct
  type t = Completions.Plugin.t list [@@deriving sexp]

  let param =
    let%map_open.Command web_search =
      flag "web-search" no_arg ~doc:" enable web search plugin"
    and web_search_max_results =
      flag
        "web-search-max-results"
        (optional int)
        ~doc:"N maximum web-search plugin results"
    and auto_router = flag "auto-router" no_arg ~doc:" enable auto-router plugin"
    and auto_router_allowed_models =
      flag
        "auto-router-allowed-model"
        (listed string)
        ~doc:"PATTERN restrict auto-router plugin to a model pattern"
    and moderation = flag "moderation" no_arg ~doc:" enable moderation plugin"
    and response_healing =
      flag "response-healing" no_arg ~doc:" enable response-healing plugin"
    and context_compression =
      flag "context-compression" no_arg ~doc:" enable context-compression plugin"
    and file_parser_pdf_engine =
      flag
        "file-parser-pdf-engine"
        (optional Completions.Plugin.Pdf_engine.arg_type)
        ~doc:"ENGINE enable file-parser plugin with PDF engine"
    and pareto_min_coding_score =
      flag
        "pareto-min-coding-score"
        (optional float)
        ~doc:"FLOAT enable pareto-router plugin with minimum coding score"
    in
    List.filter_opt
      [ (match auto_router, auto_router_allowed_models with
         | false, [] -> None
         | enabled, allowed_models ->
           Some (Completions.Plugin.auto_router ~enabled ~allowed_models ()))
      ; (match moderation with
         | true -> Some Completions.Plugin.moderation
         | false -> None)
      ; (match web_search, web_search_max_results with
         | false, None -> None
         | _, max_results -> Some (Completions.Plugin.web ~enabled:true ?max_results ()))
      ; Option.map file_parser_pdf_engine ~f:(fun pdf_engine ->
          Completions.Plugin.file_parser ~pdf_engine ())
      ; (match response_healing with
         | true -> Some Completions.Plugin.response_healing
         | false -> None)
      ; (match context_compression with
         | true -> Some Completions.Plugin.context_compression
         | false -> None)
      ; Option.map pareto_min_coding_score ~f:(fun min_coding_score ->
          Completions.Plugin.pareto_router ~min_coding_score ())
      ]
  ;;
end

module Server_tools = struct
  type t = Completions.Tool.t list [@@deriving sexp]

  let param =
    let%map_open.Command server_web_search =
      flag
        "server-web-search"
        no_arg
        ~doc:" enable server-side openrouter:web_search tool (model decides when to call)"
    and server_web_search_engine =
      flag "server-web-search-engine" (optional string) ~doc:"ENGINE web search engine"
    and server_web_search_max_results =
      flag
        "server-web-search-max-results"
        (optional int)
        ~doc:"N maximum results per web search call"
    and server_web_search_max_total_results =
      flag
        "server-web-search-max-total-results"
        (optional int)
        ~doc:"N maximum total web search results"
    and server_web_search_context_size =
      flag
        "server-web-search-context-size"
        (optional string)
        ~doc:"SIZE web search context size (low|medium|high)"
    and server_web_search_allowed_domain =
      flag
        "server-web-search-allowed-domain"
        (listed string)
        ~doc:"DOMAIN restrict server web search results to domain"
    and server_web_search_blocked_domain =
      flag
        "server-web-search-blocked-domain"
        (listed string)
        ~doc:"DOMAIN exclude domain from server web search results"
    and server_web_fetch =
      flag "server-web-fetch" no_arg ~doc:" enable server-side openrouter:web_fetch tool"
    and server_web_fetch_engine =
      flag "server-web-fetch-engine" (optional string) ~doc:"ENGINE web fetch engine"
    and server_web_fetch_max_uses =
      flag "server-web-fetch-max-uses" (optional int) ~doc:"N maximum web fetch uses"
    and server_web_fetch_max_content_tokens =
      flag
        "server-web-fetch-max-content-tokens"
        (optional int)
        ~doc:"N maximum fetched content tokens"
    and server_web_fetch_allowed_domain =
      flag
        "server-web-fetch-allowed-domain"
        (listed string)
        ~doc:"DOMAIN restrict server web fetch URLs to domain"
    and server_web_fetch_blocked_domain =
      flag
        "server-web-fetch-blocked-domain"
        (listed string)
        ~doc:"DOMAIN exclude server web fetch URLs from domain"
    and server_datetime =
      flag "server-datetime" no_arg ~doc:" enable server-side openrouter:datetime tool"
    and server_image_generation =
      flag
        "server-image-generation"
        no_arg
        ~doc:" enable server-side openrouter:image_generation tool"
    and server_image_generation_model =
      flag
        "server-image-generation-model"
        (optional string)
        ~doc:"MODEL image model for server-side image generation"
    and server_image_generation_prompt =
      flag
        "server-image-generation-prompt"
        (optional string)
        ~doc:"PROMPT prompt to pass to the server-side image generation tool"
    and server_image_generation_param =
      flag
        "server-image-generation-param"
        (listed string)
        ~doc:"KEY=JSON provider-specific image generation parameter"
    and server_search_models =
      flag
        "server-search-models"
        no_arg
        ~doc:" enable experimental server-side model-search tool"
    and server_search_models_max_results =
      flag
        "server-search-models-max-results"
        (optional int)
        ~doc:"N maximum search-models tool results"
    and demo_function_tool =
      flag "demo-function-tool" no_arg ~doc:" add a simple strict-capable function tool"
    and demo_function_strict =
      flag "demo-function-strict" no_arg ~doc:" set strict=true on -demo-function-tool"
    in
    let%map.Or_error server_image_generation_parameters =
      key_json_values_or_error
        ~flag:"-server-image-generation-param"
        server_image_generation_param
    in
    List.filter_opt
      [ (match server_web_search with
         | true ->
           Some
             (Completions.Tool.web_search
                ?engine:server_web_search_engine
                ?max_results:server_web_search_max_results
                ?max_total_results:server_web_search_max_total_results
                ?search_context_size:server_web_search_context_size
                ~allowed_domains:server_web_search_allowed_domain
                ~blocked_domains:server_web_search_blocked_domain
                ())
         | false -> None)
      ; (match server_web_fetch with
         | true ->
           Some
             (Completions.Tool.web_fetch
                ?engine:server_web_fetch_engine
                ?max_uses:server_web_fetch_max_uses
                ?max_content_tokens:server_web_fetch_max_content_tokens
                ~allowed_domains:server_web_fetch_allowed_domain
                ~blocked_domains:server_web_fetch_blocked_domain
                ())
         | false -> None)
      ; (match server_datetime with
         | true -> Some Completions.Tool.datetime
         | false -> None)
      ; (match server_image_generation with
         | true ->
           Some
             (Completions.Tool.image_generation
                ?model:server_image_generation_model
                ?prompt:server_image_generation_prompt
                ~parameters:server_image_generation_parameters
                ())
         | false -> None)
      ; (match server_search_models with
         | true ->
           Some
             (Completions.Tool.search_models
                ?max_results:server_search_models_max_results
                ())
         | false -> None)
      ; (match demo_function_tool with
         | false -> None
         | true ->
           Some
             (Completions.Tool.function_
                ~name:"lookup_code"
                ~description:"Look up a short code in a local table"
                ~parameters:
                  (`Object
                      [ "type", `String "object"
                      ; ( "properties"
                        , `Object
                            [ ( "code"
                              , `Object
                                  [ "type", `String "string"
                                  ; "description", `String "Code to look up"
                                  ] )
                            ] )
                      ; "required", `Array [ `String "code" ]
                      ; "additionalProperties", `False
                      ])
                ~strict:demo_function_strict
                ()))
      ]
  ;;
end

module Provider = struct
  type t = Completions.Request.Provider.t option [@@deriving sexp]

  let param =
    let%map_open.Command provider_only =
      flag
        "provider-only"
        (listed string)
        ~doc:"NAME restrict routing to this provider (may be repeated)"
    and provider_ignore =
      flag
        "provider-ignore"
        (listed string)
        ~doc:"NAME exclude this provider from routing (may be repeated)"
    and provider_quantization =
      flag
        "provider-quantization"
        (listed string)
        ~doc:"NAME restrict routing to this quantization (may be repeated)"
    and provider_order =
      flag
        "provider-order"
        (listed string)
        ~doc:"NAME try providers in this order (may be repeated)"
    and provider_sort =
      flag
        "provider-sort"
        (optional Completions.Request.Provider.Sort.arg_type)
        ~doc:"SORT route preference (price|throughput|latency)"
    and provider_data_collection =
      flag
        "provider-data-collection"
        (optional Completions.Request.Provider.Data_collection.arg_type)
        ~doc:"MODE allow|deny — restrict providers by data-collection policy"
    and provider_zdr =
      flag "provider-zdr" no_arg ~doc:" restrict to Zero Data Retention endpoints"
    and provider_allow_fallbacks =
      flag
        "provider-no-fallbacks"
        no_arg
        ~doc:" disable provider fallbacks (sets allow_fallbacks=false)"
    and provider_require_parameters =
      flag
        "provider-require-parameters"
        no_arg
        ~doc:" require providers that support all request parameters"
    and provider_enforce_distillable_text =
      flag
        "provider-enforce-distillable-text"
        no_arg
        ~doc:" restrict routing to providers/models that allow text distillation"
    and provider_preferred_min_throughput =
      flag
        "provider-preferred-min-throughput"
        (optional float)
        ~doc:"FLOAT preferred minimum provider throughput"
    and provider_preferred_max_latency =
      flag
        "provider-preferred-max-latency"
        (optional float)
        ~doc:"FLOAT preferred maximum provider latency"
    and provider_max_price_prompt =
      flag
        "provider-max-price-prompt"
        (optional float)
        ~doc:"FLOAT max provider prompt price"
    and provider_max_price_completion =
      flag
        "provider-max-price-completion"
        (optional float)
        ~doc:"FLOAT max provider completion price"
    and provider_max_price_request =
      flag
        "provider-max-price-request"
        (optional float)
        ~doc:"FLOAT max provider request price"
    and provider_max_price_image =
      flag
        "provider-max-price-image"
        (optional float)
        ~doc:"FLOAT max provider image price"
    and provider_max_price_audio =
      flag
        "provider-max-price-audio"
        (optional float)
        ~doc:"FLOAT max provider audio price"
    in
    let max_price =
      match
        List.exists
          [ provider_max_price_prompt
          ; provider_max_price_completion
          ; provider_max_price_request
          ; provider_max_price_image
          ; provider_max_price_audio
          ]
          ~f:Option.is_some
      with
      | false -> None
      | true ->
        Some
          { Completions.Request.Provider.Max_price.prompt = provider_max_price_prompt
          ; completion = provider_max_price_completion
          ; request = provider_max_price_request
          ; image = provider_max_price_image
          ; audio = provider_max_price_audio
          }
    in
    let provider =
      { Completions.Request.Provider.order =
          Option.some_if (not (List.is_empty provider_order)) provider_order
      ; allow_fallbacks = Option.some_if provider_allow_fallbacks false
      ; require_parameters = Option.some_if provider_require_parameters true
      ; data_collection = provider_data_collection
      ; zdr = Option.some_if provider_zdr true
      ; only = Option.some_if (not (List.is_empty provider_only)) provider_only
      ; ignore = Option.some_if (not (List.is_empty provider_ignore)) provider_ignore
      ; quantizations =
          Option.some_if (not (List.is_empty provider_quantization)) provider_quantization
      ; sort = provider_sort
      ; max_price
      ; preferred_min_throughput = provider_preferred_min_throughput
      ; preferred_max_latency = provider_preferred_max_latency
      ; enforce_distillable_text = Option.some_if provider_enforce_distillable_text true
      }
    in
    Option.some_if (not (Completions.Request.Provider.is_empty provider)) provider
  ;;
end

module Request_options = struct
  type t =
    { temperature : float option
    ; top_p : float option
    ; top_k : int option
    ; min_p : float option
    ; top_a : float option
    ; max_tokens : int option
    ; max_completion_tokens : int option
    ; seed : int option
    ; stop : string list
    ; frequency_penalty : float option
    ; presence_penalty : float option
    ; repetition_penalty : float option
    ; logit_bias : Logit_bias.t
    ; logprobs : bool
    ; top_logprobs : int option
    ; verbosity : Completions.Request.Verbosity.t option
    ; parallel_tool_calls : bool option
    ; modalities : string list
    ; structured_outputs : bool
    ; cache_control : Completions.Request.Cache_control.t option
    ; service_tier : string option
    ; models : string list
    ; transforms : string list
    ; metadata : Completions.Request.Metadata.t
    ; user : string option
    ; session_id : string option
    ; route : string option
    ; trace : Completions.Request.Trace.t
    ; image_config : Completions.Request.Image_config.t
    }
  [@@deriving sexp]

  let param =
    let%map_open.Command temperature =
      flag "temperature" (optional float) ~doc:"FLOAT sampling temperature"
    and top_p = flag "top-p" (optional float) ~doc:"FLOAT nucleus sampling cutoff"
    and top_k = flag "top-k" (optional int) ~doc:"N top-k sampling cutoff"
    and min_p = flag "min-p" (optional float) ~doc:"FLOAT min-p sampling cutoff"
    and top_a = flag "top-a" (optional float) ~doc:"FLOAT top-a sampling cutoff"
    and max_tokens = flag "max-tokens" (optional int) ~doc:"N max tokens in completion"
    and max_completion_tokens =
      flag
        "max-completion-tokens"
        (optional int)
        ~doc:"N max completion tokens (modern alias for -max-tokens)"
    and seed = flag "seed" (optional int) ~doc:"N RNG seed for reproducibility"
    and stop =
      flag "stop" (listed string) ~doc:"STR stop sequence (may be repeated, up to 4)"
    and frequency_penalty =
      flag "frequency-penalty" (optional float) ~doc:"FLOAT frequency penalty"
    and presence_penalty =
      flag "presence-penalty" (optional float) ~doc:"FLOAT presence penalty"
    and repetition_penalty =
      flag "repetition-penalty" (optional float) ~doc:"FLOAT repetition penalty"
    and logit_bias = Logit_bias.param
    and logprobs = flag "logprobs" no_arg ~doc:" return token logprobs"
    and top_logprobs =
      flag "top-logprobs" (optional int) ~doc:"N return top-N logprobs per token"
    and verbosity =
      flag
        "verbosity"
        (optional Completions.Request.Verbosity.arg_type)
        ~doc:"LEVEL completion verbosity (low|medium|high|xhigh|max)"
    and parallel_tool_calls =
      flag "parallel-tool-calls" (optional bool) ~doc:"BOOL allow parallel tool calls"
    and modalities =
      flag
        "modality"
        (listed string)
        ~doc:"NAME enable response modality (e.g. text, image; may be repeated)"
    and structured_outputs =
      flag
        "structured-outputs"
        no_arg
        ~doc:
          " set structured_outputs=true (strict-mode flag complementary to -json-schema)"
    and cache_control_ttl =
      flag
        "cache-control-ttl"
        (optional string)
        ~doc:"TTL enable top-level automatic prompt caching with ttl (e.g. 5m, 1h)"
    and service_tier =
      flag
        "service-tier"
        (optional string)
        ~doc:"TIER service tier (e.g. auto, default, flex, priority)"
    and models =
      flag
        "fallback-model"
        (listed string)
        ~doc:"MODEL fallback model id, in priority order (may be repeated)"
    and transforms =
      flag
        "transform"
        (listed string)
        ~doc:"NAME message transform (e.g. middle-out; may be repeated)"
    and metadata =
      flag
        "metadata"
        (listed string)
        ~doc:"KEY=VALUE request metadata pair (may be repeated)"
    and user = flag "user" (optional string) ~doc:"ID unique user identifier"
    and session_id =
      flag "session-id" (optional string) ~doc:"ID observability session id"
    and route = flag "route" (optional string) ~doc:"ROUTE deprecated route value"
    and trace =
      flag "trace" (listed string) ~doc:"KEY=JSON trace metadata pair (may be repeated)"
    and image_config =
      flag "image-config" (listed string) ~doc:"KEY=JSON top-level image_config parameter"
    in
    let%map.Or_error metadata = key_values_or_error ~flag:"-metadata" metadata
    and trace = key_json_values_or_error ~flag:"-trace" trace
    and logit_bias
    and image_config =
      image_config
      |> key_json_values_or_error ~flag:"-image-config"
      |> Or_error.map ~f:(fun kvs -> `Object kvs)
    in
    { temperature
    ; top_p
    ; top_k
    ; min_p
    ; top_a
    ; max_tokens
    ; max_completion_tokens
    ; seed
    ; stop
    ; frequency_penalty
    ; presence_penalty
    ; repetition_penalty
    ; logit_bias
    ; logprobs
    ; top_logprobs
    ; verbosity
    ; parallel_tool_calls
    ; modalities
    ; structured_outputs
    ; cache_control =
        Option.map cache_control_ttl ~f:(fun ttl ->
          Completions.Request.Cache_control.ephemeral ~ttl ())
    ; service_tier
    ; models
    ; transforms
    ; metadata
    ; user
    ; session_id
    ; route
    ; trace
    ; image_config
    }
  ;;
end

module Streaming = struct
  type t =
    { include_usage : bool
    ; echo_upstream_body : bool
    ; audio : Completions.Request.Audio.t option
    }
  [@@deriving sexp]

  let is_empty t =
    (not t.include_usage) && (not t.echo_upstream_body) && Option.is_none t.audio
  ;;

  let param =
    let%map_open.Command include_usage =
      flag "stream-usage" no_arg ~doc:" include final usage chunk in streaming responses"
    and debug_echo_upstream_body =
      flag
        "debug-echo-upstream-body"
        no_arg
        ~doc:" include transformed upstream request body in first stream debug chunk"
    and audio_voice =
      flag "audio-voice" (optional string) ~doc:"VOICE request audio output voice"
    and audio_format =
      flag "audio-format" (optional string) ~doc:"FORMAT request audio output format"
    in
    let%map.Or_error audio =
      match audio_voice, audio_format with
      | None, None -> Ok None
      | Some voice, Some format ->
        Ok (Some (Completions.Request.Audio.create ~voice ~format))
      | _ ->
        Or_error.error_string "-audio-voice and -audio-format must be passed together"
    in
    { include_usage; echo_upstream_body = debug_echo_upstream_body; audio }
  ;;
end

module App_info = struct
  type t = Http.App_info.t

  let param =
    let%map_open.Command app_referer =
      flag
        "app-referer"
        (optional string)
        ~doc:"URL value for HTTP-Referer header (app attribution)"
    and app_title =
      flag
        "app-title"
        (optional string)
        ~doc:"NAME value for X-Title header (app attribution)"
    and app_cache =
      flag
        "app-cache"
        no_arg
        ~doc:" send X-OpenRouter-Cache: true to opt into response caching"
    and app_experimental_metadata =
      flag
        "app-experimental-metadata"
        no_arg
        ~doc:" send X-OpenRouter-Experimental-Metadata: enabled"
    in
    Http.App_info.create
      ?http_referer:app_referer
      ?x_title:app_title
      ~cache:app_cache
      ~experimental_metadata:app_experimental_metadata
      ()
  ;;
end

module Logging = struct
  type t =
    { response_to : string option
    ; request_to : string option
    ; stream_to : string option
    }
  [@@deriving sexp]

  let param =
    let%map_open.Command response_to =
      flag
        "log-response-to"
        (optional Filename_unix.arg_type)
        ~doc:
          "PATH write the raw JSON response body to PATH (non-streaming only); useful \
           for capturing test fixtures"
    and request_to =
      flag
        "log-request-to"
        (optional Filename_unix.arg_type)
        ~doc:"PATH write the JSON request body before sending"
    and stream_to =
      flag
        "log-stream-to"
        (optional Filename_unix.arg_type)
        ~doc:
          "PATH write each raw stream chunk JSON to PATH as one line (streaming only); \
           useful for capturing test fixtures"
    in
    { response_to; request_to; stream_to }
  ;;
end

module Common = struct
  type t =
    { model : string
    ; message_input : Message_input.t
    ; output_format : Output_format.t option
    ; reasoning : Reasoning.t
    ; tool_choice : Tool_choice.t
    ; plugins : Plugins.t
    ; server_tools : Server_tools.t
    ; provider : Provider.t
    ; request_options : Request_options.t
    }

  let param ~default_model =
    let%map_open.Command model =
      flag
        "model"
        (optional_with_default default_model string)
        ~doc:[%string "MODEL model to use (default: %{default_model})"]
    and message_input = Message_input.param
    and reasoning = Reasoning.param
    and tool_choice = Tool_choice.param
    and plugins = Plugins.param
    and server_tools = Server_tools.param
    and provider = Provider.param
    and output_format = Output_format.param
    and request_options = Request_options.param in
    let%map.Or_error reasoning
    and server_tools
    and output_format
    and request_options in
    { model
    ; message_input
    ; output_format
    ; reasoning
    ; tool_choice
    ; plugins
    ; server_tools
    ; provider
    ; request_options
    }
  ;;

  let resolve
        { model
        ; message_input
        ; output_format
        ; reasoning
        ; tool_choice
        ; plugins
        ; server_tools
        ; provider
        ; request_options =
            { temperature
            ; top_p
            ; top_k
            ; min_p
            ; top_a
            ; max_tokens
            ; max_completion_tokens
            ; seed
            ; stop
            ; frequency_penalty
            ; presence_penalty
            ; repetition_penalty
            ; logit_bias
            ; logprobs
            ; top_logprobs
            ; verbosity
            ; parallel_tool_calls
            ; modalities
            ; structured_outputs
            ; cache_control
            ; service_tier
            ; models
            ; transforms
            ; metadata
            ; user
            ; session_id
            ; route
            ; trace
            ; image_config
            }
        }
    =
    let%map.Deferred.Or_error response_format = Output_format.resolve output_format
    and user_message =
      let%bind message = Message_input.message_text message_input in
      Message_input.to_request_message message_input ~message
    in
    let optional_list xs = Option.some_if (not (List.is_empty xs)) xs in
    let optional_object (json : Jsonaf.t) =
      match json with
      | `Object [] -> None
      | _ -> Some json
    in
    fun ~create ->
      create
        ?cache_control
        ?reasoning
        ?tools:(optional_list server_tools)
        ?tool_choice
        ?parallel_tool_calls
        ?plugins:(optional_list plugins)
        ?metadata:(optional_list metadata)
        ?user
        ?session_id
        ?route
        ?trace:(optional_list trace)
        ?temperature
        ?top_p
        ?top_k
        ?min_p
        ?top_a
        ?max_tokens
        ?max_completion_tokens
        ?seed
        ?stop:(optional_list stop)
        ?frequency_penalty
        ?presence_penalty
        ?repetition_penalty
        ?logit_bias
        ?logprobs:(Option.some_if logprobs true)
        ?top_logprobs
        ?verbosity
        ?response_format
        ?structured_outputs:(Option.some_if structured_outputs true)
        ?modalities:(optional_list modalities)
        ?image_config:(optional_object image_config)
        ?service_tier
        ?models:(optional_list models)
        ?transforms:(optional_list transforms)
        ?provider
        ~model
        ~messages:[ user_message ]
        ()
  ;;
end

module Request = struct
  type t =
    | Streaming of Common.t * Streaming.t
    | Non_streaming of Common.t

  module Resolved = struct
    type t =
      | Streaming of [ `Streaming ] Completions.Request.t
      | Non_streaming of [ `Non_streaming ] Completions.Request.t
  end

  let validate_streaming ~no_stream streaming =
    match no_stream, Streaming.is_empty streaming with
    | false, _ | true, true -> Ok ()
    | true, false ->
      Or_error.error_string
        "-debug-echo-upstream-body, -audio-voice/-audio-format, and -stream-usage \
         require streaming"
  ;;

  let param ~default_model =
    let%map_open.Command no_stream =
      flag "no-stream" no_arg ~doc:" disable streaming (use non-streaming API)"
    and common = Common.param ~default_model
    and streaming = Streaming.param in
    let%bind.Or_error common
    and streaming in
    let%map.Or_error () = validate_streaming ~no_stream streaming in
    match no_stream with
    | true -> Non_streaming common
    | false -> Streaming (common, streaming)
  ;;

  let resolve = function
    | Streaming (common, { Streaming.include_usage; echo_upstream_body; audio }) ->
      let%map.Deferred.Or_error create_request = Common.resolve common in
      let stream_options = Completions.Request.Stream_options.create ~include_usage () in
      let debug = Completions.Request.Debug.create ~echo_upstream_body () in
      let request =
        create_request
          ~create:(Completions.Request.create_streaming ~debug ?audio ~stream_options)
      in
      Resolved.Streaming request
    | Non_streaming common ->
      let%map.Deferred.Or_error create_request = Common.resolve common in
      let request = create_request ~create:Completions.Request.create in
      Resolved.Non_streaming request
  ;;
end

type t =
  { request : Request.t
  ; app_info : App_info.t
  ; logging : Logging.t
  }

let param ~default_model =
  let%map_open.Command request = Request.param ~default_model
  and app_info = App_info.param
  and logging = Logging.param in
  let%map.Or_error request in
  { request; app_info; logging }
;;

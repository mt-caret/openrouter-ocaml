open! Core
open! Async
open! Openrouter_api

(** Detect MIME type from file extension *)
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

(** Read a file and return its content as a base64-encoded data URL *)
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

let api_key_from_env () =
  Sys.getenv "OPENROUTER_API_KEY"
  |> Result.of_option ~error:(Error.of_string "OPENROUTER_API_KEY not set")
  |> Deferred.return
;;

let default_model = "anthropic/claude-opus-4.7"

let parse_key_value ~flag s =
  match String.lsplit2 s ~on:'=' with
  | Some (key, value) when not (String.is_empty key) -> Ok (key, value)
  | _ -> Or_error.errorf "%s expects KEY=VALUE" flag
;;

let parse_key_json ~flag s =
  let%map.Or_error key, value = parse_key_value ~flag s in
  key, Or_error.ok (Jsonaf.parse value) |> Option.value ~default:(`String value)
;;

let key_values_or_error ~flag values =
  Or_error.combine_errors (List.map values ~f:(parse_key_value ~flag))
;;

let key_json_values_or_error ~flag values =
  Or_error.combine_errors (List.map values ~f:(parse_key_json ~flag))
;;

let chat_command =
  Command.async_or_error
    ~summary:"Send a chat completion request"
    (let%map_open.Command model =
       flag
         "model"
         (optional_with_default default_model string)
         ~doc:[%string "MODEL model to use (default: %{default_model})"]
     and no_stream =
       flag "no-stream" no_arg ~doc:" disable streaming (use non-streaming API)"
     and reasoning_tokens =
       flag
         "reasoning"
         (optional int)
         ~doc:"TOKENS enable reasoning with max tokens budget"
     and reasoning_effort =
       flag
         "reasoning-effort"
         (optional string)
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
     and web_search = flag "web-search" no_arg ~doc:" enable web search plugin"
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
     and pareto_min_coding_score =
       flag
         "pareto-min-coding-score"
         (optional float)
         ~doc:"FLOAT enable pareto-router plugin with minimum coding score"
     and server_web_search =
       flag
         "server-web-search"
         no_arg
         ~doc:
           " enable server-side openrouter:web_search tool (model decides when to call)"
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
     and files = flag "file" (listed string) ~doc:"PATH attach a file (PDF or image)"
     and temperature =
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
     and logprobs = flag "logprobs" no_arg ~doc:" return token logprobs"
     and top_logprobs =
       flag "top-logprobs" (optional int) ~doc:"N return top-N logprobs per token"
     and verbosity =
       flag
         "verbosity"
         (optional string)
         ~doc:"LEVEL completion verbosity (low|medium|high|xhigh|max)"
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
     and cache_system_prompt =
       flag
         "cache-system-prompt"
         (optional string)
         ~doc:
           "TEXT prepend a cached system-prompt content part with cache_control = \
            ephemeral (Anthropic-style prompt caching)"
     and cache_control_ttl =
       flag
         "cache-control-ttl"
         (optional string)
         ~doc:"TTL enable top-level automatic prompt caching with ttl (e.g. 5m, 1h)"
     and include_usage =
       flag "stream-usage" no_arg ~doc:" include final usage chunk in streaming responses"
     and debug_echo_upstream_body =
       flag
         "debug-echo-upstream-body"
         no_arg
         ~doc:" include transformed upstream request body in first stream debug chunk"
     and service_tier =
       flag
         "service-tier"
         (optional string)
         ~doc:"TIER service tier (e.g. auto, default, flex, priority)"
     and fallback_models =
       flag
         "fallback-model"
         (listed string)
         ~doc:"MODEL fallback model id, in priority order (may be repeated)"
     and transforms =
       flag
         "transform"
         (listed string)
         ~doc:"NAME message transform (e.g. middle-out; may be repeated)"
     and provider_only =
       flag
         "provider-only"
         (listed string)
         ~doc:"NAME restrict routing to this provider (may be repeated)"
     and provider_ignore =
       flag
         "provider-ignore"
         (listed string)
         ~doc:"NAME exclude this provider from routing (may be repeated)"
     and provider_order =
       flag
         "provider-order"
         (listed string)
         ~doc:"NAME try providers in this order (may be repeated)"
     and provider_sort =
       flag
         "provider-sort"
         (optional string)
         ~doc:"SORT route preference (price|throughput|latency)"
     and provider_data_collection =
       flag
         "provider-data-collection"
         (optional string)
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
     and provider_max_price_audio =
       flag
         "provider-max-price-audio"
         (optional float)
         ~doc:"FLOAT max provider audio price"
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
     and audio_voice =
       flag "audio-voice" (optional string) ~doc:"VOICE request audio output voice"
     and audio_format =
       flag "audio-format" (optional string) ~doc:"FORMAT request audio output format"
     and image_config =
       flag
         "image-config"
         (listed string)
         ~doc:"KEY=JSON top-level image_config parameter"
     and json =
       flag "json" no_arg ~doc:" force JSON object output (response_format=json_object)"
     and json_schema_file =
       flag
         "json-schema"
         (optional Filename_unix.arg_type)
         ~doc:"FILE force output matching the JSON schema in FILE"
     and app_referer =
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
     and log_response_to =
       flag
         "log-response-to"
         (optional Filename_unix.arg_type)
         ~doc:
           "PATH write the raw JSON response body to PATH (non-streaming only); useful \
            for capturing test fixtures"
     and log_request_to =
       flag
         "log-request-to"
         (optional Filename_unix.arg_type)
         ~doc:"PATH write the JSON request body before sending"
     and log_stream_to =
       flag
         "log-stream-to"
         (optional Filename_unix.arg_type)
         ~doc:
           "PATH write each raw stream chunk JSON to PATH as one line (streaming only); \
            useful for capturing test fixtures"
     and message = anon (maybe ("MESSAGE" %: string))
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind.Deferred.Or_error api_key = api_key_from_env () in
       let%bind message =
         match message with
         | Some message -> return message
         | None -> Reader.contents (force Reader.stdin)
       in
       let%bind.Deferred.Or_error response_format =
         match json, json_schema_file with
         | true, Some _ ->
           Deferred.Or_error.error_string "cannot pass both -json and -json-schema"
         | true, None ->
           Deferred.Or_error.return (Some Completions.Request.Response_format.Json_object)
         | false, Some path ->
           let%map contents = Reader.file_contents path in
           let%map.Or_error schema = Jsonaf.parse contents in
           let name = Filename.basename path |> Filename.chop_extension in
           Some
             (Completions.Request.Response_format.json_schema
                ~name
                ~strict:true
                ~schema
                ())
         | false, None -> Deferred.Or_error.return None
       in
       let stream = not no_stream in
       let parse_string_variant of_string ~flag s =
         Or_error.try_with (fun () -> of_string s)
         |> Or_error.tag_s
              ~tag:[%message "invalid flag value" (flag : string) (s : string)]
       in
       let%bind.Deferred.Or_error reasoning =
         Deferred.return
           (match
              reasoning_tokens, reasoning_effort, reasoning_exclude, reasoning_summary
            with
            | None, None, false, None -> Ok None
            | max_tokens, effort, exclude, summary ->
              let%bind.Or_error effort =
                match effort with
                | None -> Ok None
                | Some s ->
                  parse_string_variant
                    Completions.Request.Reasoning.Effort.of_string
                    ~flag:"-reasoning-effort"
                    s
                  |> Or_error.map ~f:Option.some
              in
              let exclude = if exclude then Some true else None in
              let%map.Or_error reasoning =
                Completions.Request.Reasoning.create
                  ?effort
                  ?max_tokens
                  ?exclude
                  ?summary
                  ()
              in
              Some reasoning)
       in
       let%bind.Deferred.Or_error metadata =
         Deferred.return (metadata |> key_values_or_error ~flag:"-metadata")
       in
       let%bind.Deferred.Or_error trace =
         Deferred.return (trace |> key_json_values_or_error ~flag:"-trace")
       in
       let%bind.Deferred.Or_error image_config =
         Deferred.return
           (image_config
            |> key_json_values_or_error ~flag:"-image-config"
            |> Or_error.map ~f:(fun kvs -> `Object kvs))
       in
       let%bind.Deferred.Or_error server_image_generation_parameters =
         Deferred.return
           (server_image_generation_param
            |> key_json_values_or_error ~flag:"-server-image-generation-param")
       in
       let%bind.Deferred.Or_error audio =
         Deferred.return
           (match audio_voice, audio_format with
            | None, None -> Ok None
            | Some voice, Some format ->
              Ok (Some (Completions.Request.Audio.create ~voice ~format))
            | _ ->
              Or_error.error_string
                "-audio-voice and -audio-format must be passed together")
       in
       let%bind.Deferred.Or_error verbosity =
         Deferred.return
           (match verbosity with
            | None -> Ok None
            | Some s ->
              parse_string_variant
                Completions.Request.Verbosity.of_string
                ~flag:"-verbosity"
                s
              |> Or_error.map ~f:Option.some)
       in
       let%bind.Deferred.Or_error provider =
         Deferred.return
           (let%bind.Or_error sort =
              match provider_sort with
              | None -> Ok None
              | Some s ->
                parse_string_variant
                  Completions.Request.Provider.Sort.of_string
                  ~flag:"-provider-sort"
                  s
                |> Or_error.map ~f:Option.some
            in
            let%map.Or_error data_collection =
              match provider_data_collection with
              | None -> Ok None
              | Some s ->
                parse_string_variant
                  Completions.Request.Provider.Data_collection.of_string
                  ~flag:"-provider-data-collection"
                  s
                |> Or_error.map ~f:Option.some
            in
            let provider =
              let max_price =
                Option.map provider_max_price_audio ~f:(fun audio ->
                  { Completions.Request.Provider.Max_price.prompt = None
                  ; completion = None
                  ; request = None
                  ; image = None
                  ; audio = Some audio
                  })
              in
              { Completions.Request.Provider.empty with
                order =
                  (match provider_order with
                   | [] -> None
                   | _ :: _ -> Some provider_order)
              ; allow_fallbacks =
                  (match provider_allow_fallbacks with
                   | true -> Some false
                   | false -> None)
              ; require_parameters =
                  (match provider_require_parameters with
                   | true -> Some true
                   | false -> None)
              ; data_collection
              ; zdr =
                  (match provider_zdr with
                   | true -> Some true
                   | false -> None)
              ; only =
                  (match provider_only with
                   | [] -> None
                   | _ :: _ -> Some provider_only)
              ; ignore =
                  (match provider_ignore with
                   | [] -> None
                   | _ :: _ -> Some provider_ignore)
              ; sort
              ; max_price
              ; enforce_distillable_text =
                  (match provider_enforce_distillable_text with
                   | true -> Some true
                   | false -> None)
              }
            in
            match Completions.Request.Provider.is_empty provider with
            | true -> None
            | false -> Some provider)
       in
       let stream_options =
         match include_usage with
         | true -> Some (Completions.Request.Stream_options.create ~include_usage:true ())
         | false -> None
       in
       let plugins =
         List.filter_opt
           [ (match auto_router, auto_router_allowed_models with
              | false, [] -> None
              | enabled, allowed_models ->
                Some (Completions.Plugin.auto_router ~enabled ~allowed_models ()))
           ; (match moderation with
              | true -> Some Completions.Plugin.moderation
              | false -> None)
           ; (match web_search with
              | true -> Some (Completions.Plugin.web ())
              | false -> None)
           ; (match response_healing with
              | true -> Some Completions.Plugin.response_healing
              | false -> None)
           ; (match context_compression with
              | true -> Some Completions.Plugin.context_compression
              | false -> None)
           ; Option.map pareto_min_coding_score ~f:(fun min_coding_score ->
               Completions.Plugin.pareto_router ~min_coding_score ())
           ]
       in
       let server_tools =
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
       in
       let cache_control =
         Option.map cache_control_ttl ~f:(fun ttl ->
           Completions.Request.Cache_control.ephemeral ~ttl ())
       in
       let debug =
         match debug_echo_upstream_body with
         | true -> Some (Completions.Request.Debug.create ~echo_upstream_body:true ())
         | false -> None
       in
       let app_info =
         Http.App_info.create
           ?http_referer:app_referer
           ?x_title:app_title
           ~cache:app_cache
           ~experimental_metadata:app_experimental_metadata
           ()
       in
       (* Build message - use multipart if files are attached or a cached
          system prompt is supplied. *)
       let cache_system_part =
         Option.map cache_system_prompt ~f:(fun text ->
           Completions.Request.Message.Content_part.text
             ~cache_control:
               (Completions.Request.Message.Content_part.Cache_control.ephemeral ())
             text)
       in
       let%bind.Deferred.Or_error user_message =
         match files, cache_system_part with
         | [], None -> Deferred.Or_error.return (Completions.Request.Message.user message)
         | files, cache_system_part ->
           (* Read all files and build content parts *)
           let%bind.Deferred.Or_error file_parts =
             Deferred.Or_error.List.map files ~how:`Sequential ~f:(fun path ->
               let%bind.Deferred.Or_error mime_type, data_url =
                 read_file_as_data_url path
               in
               let filename = Filename.basename path in
               let base64_data () =
                 String.chop_prefix_exn
                   data_url
                   ~prefix:(sprintf "data:%s;base64," mime_type)
               in
               match String.lsplit2 mime_type ~on:'/' with
               | Some ("image", _) ->
                 Deferred.Or_error.return
                   (Completions.Request.Message.Content_part.image_base64
                      ~mime_type
                      ~data:(base64_data ())
                      ())
               | Some ("audio", subtype) ->
                 (* OpenRouter expects [format] like "wav"/"mp3"/"flac"; mp4 ↔ m4a. *)
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
       in
       let bool_flag b = if b then Some true else None in
       (* Bundle the args that don't depend on the streaming variant. *)
       let logprobs = bool_flag logprobs in
       let structured_outputs = bool_flag structured_outputs in
       let%bind.Deferred.Or_error () =
         Deferred.return
           (match stream, debug, audio, stream_options with
            | true, _, _, _ | false, None, None, None -> Ok ()
            | false, _, _, _ ->
              Or_error.error_string
                "-debug-echo-upstream-body, -audio-voice/-audio-format, and \
                 -stream-usage require streaming")
       in
       match stream with
       | true ->
         let request =
           Completions.Request.create_streaming
             ~model
             ~messages:[ user_message ]
             ?cache_control
             ?debug
             ?reasoning
             ~tools:server_tools
             ~plugins
             ~metadata
             ?user
             ?session_id
             ?route
             ~trace
             ?temperature
             ?top_p
             ?top_k
             ?min_p
             ?top_a
             ?max_tokens
             ?max_completion_tokens
             ?seed
             ~stop
             ?frequency_penalty
             ?presence_penalty
             ?repetition_penalty
             ?logprobs
             ?top_logprobs
             ?verbosity
             ?response_format
             ?structured_outputs
             ~modalities
             ?audio
             ~image_config
             ?stream_options
             ?service_tier
             ~models:fallback_models
             ~transforms
             ?provider
             ()
         in
         let%bind () =
           match log_request_to with
           | None -> return ()
           | Some path ->
             Writer.save
               path
               ~contents:
                 ([%jsonaf_of: [ `Streaming ] Completions.Request.t] request
                  |> Jsonaf.to_string_hum)
         in
         let%bind log_writer =
           match log_stream_to with
           | None -> return None
           | Some path ->
             let%map w = Writer.open_file path in
             Some w
         in
         let on_stream_chunk =
           Option.map log_writer ~f:(fun w chunk ->
             Writer.write_line w chunk;
             Writer.flushed w)
         in
         let%bind () =
           Completions.create_stream ~api_key ~app_info ?on_stream_chunk request
           >>= Pipe.iter ~f:(fun chunk_result ->
             match chunk_result with
             | Error err ->
               eprintf "Stream error: %s\n" (Error.to_string_hum err);
               return ()
             | Ok (chunk : Completions.Stream_chunk.t) ->
               Deferred.List.iter chunk.choices ~how:`Sequential ~f:(fun choice ->
                 (* Print text content *)
                 let%bind () =
                   match choice.delta.content with
                   | None -> return ()
                   | Some content ->
                     print_string content;
                     Writer.flushed (force Writer.stdout)
                 in
                 (* Print image placeholders *)
                 let%bind () =
                   Deferred.List.iter choice.delta.images ~how:`Sequential ~f:(fun img ->
                     printf "\n<image: %d bytes>\n" (String.length img.image_url.url);
                     Writer.flushed (force Writer.stdout))
                 in
                 (* Print citations from web search *)
                 let citations =
                   List.filter_map choice.delta.annotations ~f:(fun ann ->
                     Completions.Citation.of_annotation_jsonaf ann)
                 in
                 Deferred.List.iter citations ~how:`Sequential ~f:(fun citation ->
                   let title = Option.value citation.title ~default:citation.url in
                   printf "\n📎 [%s](%s)\n" title citation.url;
                   Writer.flushed (force Writer.stdout))))
         in
         let%bind () =
           match log_writer with
           | None -> return ()
           | Some w -> Writer.close w
         in
         print_endline "";
         Deferred.Or_error.return ()
       | false ->
         let request =
           Completions.Request.create
             ~model
             ~messages:[ user_message ]
             ?cache_control
             ?reasoning
             ~tools:server_tools
             ~plugins
             ~metadata
             ?user
             ?session_id
             ?route
             ~trace
             ?temperature
             ?top_p
             ?top_k
             ?min_p
             ?top_a
             ?max_tokens
             ?max_completion_tokens
             ?seed
             ~stop
             ?frequency_penalty
             ?presence_penalty
             ?repetition_penalty
             ?logprobs
             ?top_logprobs
             ?verbosity
             ?response_format
             ?structured_outputs
             ~modalities
             ~image_config
             ?service_tier
             ~models:fallback_models
             ~transforms
             ?provider
             ()
         in
         let%bind () =
           match log_request_to with
           | None -> return ()
           | Some path ->
             Writer.save
               path
               ~contents:
                 ([%jsonaf_of: [ `Non_streaming ] Completions.Request.t] request
                  |> Jsonaf.to_string_hum)
         in
         let on_response_body =
           Option.map log_response_to ~f:(fun path body ->
             Writer.save path ~contents:body)
         in
         let%bind response =
           Completions.create ~api_key ~app_info ?on_response_body request
         in
         [%sexp_of: Completions.Response.Elide_image.t Or_error.t] response
         |> Sexp.to_string_hum
         |> print_endline;
         Deferred.Or_error.return ())
;;

let list_models_command =
  Command.async_or_error
    ~summary:"List all available models"
    (let%map_open.Command sexp = flag "sexp" no_arg ~doc:"print raw sexp output"
     and log_response_to =
       flag
         "log-response-to"
         (optional Filename_unix.arg_type)
         ~doc:"PATH write the raw JSON response body to PATH"
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind.Deferred.Or_error api_key = api_key_from_env () in
       let on_response_body =
         Option.map log_response_to ~f:(fun path body -> Writer.save path ~contents:body)
       in
       let%map.Deferred.Or_error { data = models } =
         Models.list ~api_key ?on_response_body ()
         |> Deferred.Or_error.tag_s_lazy ~tag:(lazy [%message "Error fetching models"])
       in
       match sexp with
       | true -> print_s [%sexp (models : Models.Model_info.t list)]
       | false ->
         let open Models.Model_info in
         (* Sort by model name, then by prompt price *)
         let models =
           List.sort
             models
             ~compare:
               (Comparable.lexicographic
                  [ Comparable.lift String.compare ~f:id
                  ; Comparable.lift Float.compare ~f:(Fn.compose Pricing.prompt pricing)
                  ])
         in
         let columns =
           let open Ascii_table.Column in
           [ create ~min_width:50 "Model ID" id
           ; create ~min_width:9 "Context" (fun m ->
               match context_length m with
               | Some len -> sprintf "%dk" (len / 1000)
               | None -> "-")
           ; create ~min_width:12 "Max Output" (fun m ->
               match Top_provider.max_completion_tokens (top_provider m) with
               | Some tokens -> sprintf "%dk" (tokens / 1000)
               | None -> "-")
           ; create ~min_width:18 "Modality" (fun m ->
               Option.value (Architecture.modality (architecture m)) ~default:"-")
           ; create ~min_width:12 "Prompt $/M" (fun m ->
               let price = Pricing.prompt (pricing m) in
               if Float.( = ) price 0.
               then "free"
               else sprintf "%.2f" (price *. 1_000_000.))
           ; create ~min_width:16 "Completion $/M" (fun m ->
               let price = Pricing.completion (pricing m) in
               if Float.( = ) price 0.
               then "free"
               else sprintf "%.2f" (price *. 1_000_000.))
           ]
         in
         print_string (Ascii_table.to_string ~limit_width_to:180 columns models))
;;

let embeddings_command =
  Command.async_or_error
    ~summary:"Get embeddings for one or more inputs (one per line on stdin if no -input)"
    (let%map_open.Command model =
       flag
         "model"
         (required string)
         ~doc:"MODEL embedding model (e.g. openai/text-embedding-3-small)"
     and inputs =
       flag
         "input"
         (listed string)
         ~doc:"STR text to embed (may be repeated; otherwise read from stdin)"
     and dimensions =
       flag "dimensions" (optional int) ~doc:"N truncate embeddings to N dimensions"
     and sexp = flag "sexp" no_arg ~doc:" print raw sexp output"
     and log_response_to =
       flag
         "log-response-to"
         (optional Filename_unix.arg_type)
         ~doc:"PATH write the raw JSON response body to PATH"
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind.Deferred.Or_error api_key = api_key_from_env () in
       let%bind inputs =
         match inputs with
         | _ :: _ -> return inputs
         | [] ->
           let%map contents = Reader.contents (force Reader.stdin) in
           String.split_lines contents
           |> List.filter ~f:(fun line -> not (String.is_empty (String.strip line)))
       in
       let input =
         match inputs with
         | [ s ] -> Embeddings.Request.Input.Single s
         | xs -> Multi xs
       in
       let on_response_body =
         Option.map log_response_to ~f:(fun path body -> Writer.save path ~contents:body)
       in
       let%map.Deferred.Or_error
           ({ object_ = _; model; data; usage; provider = _; id = _ } as response)
         =
         Embeddings.create ~api_key ?on_response_body { model; input; dimensions }
       in
       match sexp with
       | true -> print_s [%sexp (response : Embeddings.Response.t)]
       | false ->
         print_s
           [%message
             ""
               (model : string)
               ~prompt_tokens:(usage.prompt_tokens : int)
               ~total_tokens:(usage.total_tokens : int)
               ~cost:(usage.cost : float option)];
         List.iter data ~f:(fun { object_ = _; index; embedding } ->
           let dims = List.length embedding in
           let preview =
             List.take embedding 3
             |> List.map ~f:(fun x -> [%string "%{x#Float}"])
             |> String.concat ~sep:", "
           in
           print_string [%string "[%{index#Int}] (%{dims#Int} dims) [%{preview}, ...]\n"]))
;;

let generation_command =
  Command.async_or_error
    ~summary:"Fetch generation stats for a previous request"
    (let%map_open.Command id =
       flag "id" (required string) ~doc:"GEN_ID generation id (returned on completion)"
     and retries =
       flag
         "retries"
         (optional_with_default 5 int)
         ~doc:"N polling attempts while waiting for the generation to materialize"
     and delay_seconds =
       flag
         "delay"
         (optional_with_default 2.0 float)
         ~doc:"S seconds between retry attempts"
     and sexp = flag "sexp" no_arg ~doc:" print raw sexp output"
     and log_response_to =
       flag
         "log-response-to"
         (optional Filename_unix.arg_type)
         ~doc:
           "PATH write the raw JSON response body of the successful poll attempt to PATH"
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind.Deferred.Or_error api_key = api_key_from_env () in
       let on_response_body =
         Option.map log_response_to ~f:(fun path body ->
           (* Only the successful poll attempt has a [data] field; skip the 404s. *)
           match String.is_substring body ~substring:"\"data\"" with
           | true -> Writer.save path ~contents:body
           | false -> return ())
       in
       let rec poll attempts_left =
         match%bind.Deferred.Or_error
           Generation.get ~api_key ?on_response_body ~id ()
         with
         | Some stats -> Deferred.Or_error.return stats
         | None when attempts_left <= 0 ->
           Deferred.Or_error.errorf "generation %s not found after %d attempts" id retries
         | None ->
           let%bind () = Clock.after (Time_float.Span.of_sec delay_seconds) in
           poll (attempts_left - 1)
       in
       let%map.Deferred.Or_error stats = poll retries in
       match sexp with
       | true -> print_s [%sexp (stats : Generation.Stats.t)]
       | false ->
         let str = Fn.id in
         let str_opt = Option.value ~default:"-" in
         let int_opt = Option.value_map ~default:"-" ~f:Int.to_string in
         let bool_opt = Option.value_map ~default:"-" ~f:Bool.to_string in
         let float_opt =
           Option.value_map ~default:"-" ~f:(fun x -> [%string "%{x#Float}"])
         in
         let jsonaf_opt = Option.value_map ~default:"-" ~f:Jsonaf.to_string in
         let providers ps =
           match ps with
           | [] -> "-"
           | ps -> [%string "%{List.length ps#Int} provider response(s)"]
         in
         (* Use Fields.to_list so the table tracks any field added to
            Generation.Stats.t — adding a field becomes a compile error here. *)
         let row fmt field = Field.name field, fmt (Field.get field stats) in
         let rows =
           Generation.Stats.Fields.to_list
             ~id:(row str)
             ~model:(row str)
             ~provider_name:(row str_opt)
             ~created_at:(row str)
             ~api_type:(row str_opt)
             ~origin:(row str_opt)
             ~user_agent:(row str_opt)
             ~http_referer:(row str_opt)
             ~session_id:(row str_opt)
             ~request_id:(row str_opt)
             ~upstream_id:(row str_opt)
             ~app_id:(row jsonaf_opt)
             ~external_user:(row jsonaf_opt)
             ~router:(row jsonaf_opt)
             ~streamed:(row bool_opt)
             ~cancelled:(row bool_opt)
             ~is_byok:(row bool_opt)
             ~finish_reason:(row str_opt)
             ~native_finish_reason:(row str_opt)
             ~service_tier:(row str_opt)
             ~latency:(row int_opt)
             ~moderation_latency:(row int_opt)
             ~generation_time:(row int_opt)
             ~tokens_prompt:(row int_opt)
             ~tokens_completion:(row int_opt)
             ~native_tokens_prompt:(row int_opt)
             ~native_tokens_completion:(row int_opt)
             ~native_tokens_completion_images:(row int_opt)
             ~native_tokens_reasoning:(row int_opt)
             ~native_tokens_cached:(row int_opt)
             ~num_media_prompt:(row int_opt)
             ~num_input_audio_prompt:(row int_opt)
             ~num_media_completion:(row int_opt)
             ~num_search_results:(row int_opt)
             ~num_fetches:(row int_opt)
             ~web_search_engine:(row str_opt)
             ~usage:(row float_opt)
             ~total_cost:(row float_opt)
             ~upstream_inference_cost:(row float_opt)
             ~cache_discount:(row float_opt)
             ~response_cache_source_id:(row str_opt)
             ~provider_responses:(row providers)
         in
         let columns =
           let open Ascii_table.Column in
           [ create ~min_width:24 "Field" fst; create ~min_width:24 "Value" snd ]
         in
         print_string (Ascii_table.to_string ~limit_width_to:120 columns rows))
;;

let command =
  Command.group
    ~summary:"OpenRouter API example client"
    [ "chat", chat_command
    ; "list-models", list_models_command
    ; "embeddings", embeddings_command
    ; "generation", generation_command
    ]
;;

let () = Command_unix.run command

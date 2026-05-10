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
     and web_search = flag "web-search" no_arg ~doc:" enable web search plugin"
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
     and include_usage =
       flag "stream-usage" no_arg ~doc:" include final usage chunk in streaming responses"
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
           (match reasoning_tokens, reasoning_effort, reasoning_exclude with
            | None, None, false -> Ok None
            | Some _, Some _, _ ->
              Or_error.error_string "cannot pass both -reasoning and -reasoning-effort"
            | max_tokens, effort, exclude ->
              let%map.Or_error effort =
                match effort with
                | None -> Ok None
                | Some s ->
                  parse_string_variant
                    Completions.Request.Reasoning.Effort.of_string
                    ~flag:"-reasoning-effort"
                    s
                  |> Or_error.map ~f:Option.some
              in
              Some
                { Completions.Request.Reasoning.effort
                ; max_tokens
                ; exclude = (if exclude then Some true else None)
                ; enabled = None
                })
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
       let stream_options =
         match include_usage with
         | true -> Some { Completions.Request.Stream_options.include_usage = Some true }
         | false -> None
       in
       let plugins =
         match web_search with
         | true -> [ Completions.Plugin.web () ]
         | false -> []
       in
       let app_info =
         Http.App_info.create
           ?http_referer:app_referer
           ?x_title:app_title
           ~cache:app_cache
           ~experimental_metadata:app_experimental_metadata
           ()
       in
       (* Build message - use multipart if files are attached *)
       let%bind.Deferred.Or_error user_message =
         match files with
         | [] -> Deferred.Or_error.return (Completions.Request.Message.user message)
         | _ ->
           (* Read all files and build content parts *)
           let%bind.Deferred.Or_error file_parts =
             Deferred.Or_error.List.map files ~how:`Sequential ~f:(fun path ->
               let%bind.Deferred.Or_error mime_type, data_url =
                 read_file_as_data_url path
               in
               let filename = Filename.basename path in
               (* Use file content part for PDFs, image for images *)
               if String.is_prefix mime_type ~prefix:"image/"
               then (
                 (* Extract base64 data from data URL for images *)
                 let data =
                   String.chop_prefix_exn
                     data_url
                     ~prefix:(sprintf "data:%s;base64," mime_type)
                 in
                 Deferred.Or_error.return
                   (Completions.Request.Message.Content_part.image_base64
                      ~mime_type
                      ~data))
               else
                 Deferred.Or_error.return
                   (Completions.Request.Message.Content_part.file
                      ~filename
                      ~file_data:data_url))
           in
           let text_part = Completions.Request.Message.Content_part.text message in
           let all_parts = text_part :: file_parts in
           Deferred.Or_error.return (Completions.Request.Message.user_multipart all_parts)
       in
       let request : Completions.Request.t =
         { model
         ; messages = [ user_message ]
         ; stream
         ; reasoning
         ; tools = []
         ; tool_choice = None
         ; parallel_tool_calls = None
         ; plugins
         ; temperature
         ; top_p
         ; top_k
         ; min_p
         ; top_a
         ; max_tokens
         ; max_completion_tokens
         ; seed
         ; stop = (if List.is_empty stop then None else Some stop)
         ; frequency_penalty
         ; presence_penalty
         ; repetition_penalty
         ; logit_bias = None
         ; logprobs = (if logprobs then Some true else None)
         ; top_logprobs
         ; verbosity
         ; response_format
         ; modalities = (if List.is_empty modalities then None else Some modalities)
         ; stream_options
         ; service_tier
         ; models = fallback_models
         ; transforms
         }
       in
       match stream with
       | true ->
         let%bind () =
           Completions.create_stream ~api_key ~app_info request
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
         print_endline "";
         Deferred.Or_error.return ()
       | false ->
         let%bind response = Completions.create ~api_key ~app_info request in
         [%sexp_of: Completions.Response.Elide_image.t Or_error.t] response
         |> Sexp.to_string_hum
         |> print_endline;
         Deferred.Or_error.return ())
;;

let list_models_command =
  Command.async_or_error
    ~summary:"List all available models"
    (let%map_open.Command sexp = flag "sexp" no_arg ~doc:"print raw sexp output"
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind.Deferred.Or_error api_key = api_key_from_env () in
       let%map.Deferred.Or_error { data = models } =
         Models.list ~api_key ()
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
       let%map.Deferred.Or_error
           ({ object_ = _; model; data; usage; provider = _; id = _ } as response)
         =
         Embeddings.create ~api_key { model; input; dimensions }
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
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind.Deferred.Or_error api_key = api_key_from_env () in
       let rec poll attempts_left =
         match%bind.Deferred.Or_error Generation.get ~api_key ~id () with
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

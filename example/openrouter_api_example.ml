open! Core
open! Async
open! Openrouter_api

let api_key_from_env () =
  Sys.getenv "OPENROUTER_API_KEY"
  |> Result.of_option ~error:(Error.of_string "OPENROUTER_API_KEY not set")
  |> Deferred.return
;;

let default_model = "anthropic/claude-opus-4.7"

let chat_command =
  Command.async_or_error
    ~summary:"Send a chat completion request"
    (let%map_open.Command config = Chat_options.param ~default_model
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind.Deferred.Or_error api_key = api_key_from_env ()
       and { request; app_info; logging = { request_to; stream_to; response_to } } =
         Deferred.return config
       in
       match%bind.Deferred.Or_error Chat_options.Request.resolve request with
       | Streaming request ->
         let%bind () =
           match request_to with
           | None -> return ()
           | Some path ->
             Writer.save
               path
               ~contents:
                 ([%jsonaf_of: [ `Streaming ] Completions.Request.t] request
                  |> Jsonaf.to_string_hum)
         and log_writer =
           match stream_to with
           | None -> return None
           | Some path ->
             let%map w = Writer.open_file path in
             Some w
         in
         let%bind () =
           Completions.create_stream
             ~api_key
             ~app_info
             ?on_stream_chunk:
               (Option.map log_writer ~f:(fun w chunk ->
                  Writer.write_line w chunk;
                  Writer.flushed w))
             request
           >>= Pipe.iter ~f:(fun chunk_result ->
             match chunk_result with
             | Error err ->
               eprintf "Stream error: %s\n" (Error.to_string_hum err);
               return ()
             | Ok (chunk : Completions.Stream_chunk.t) ->
               Deferred.List.iter chunk.choices ~how:`Sequential ~f:(fun choice ->
                 let%bind () =
                   match choice.delta.content with
                   | None -> return ()
                   | Some content ->
                     print_string content;
                     Writer.flushed (force Writer.stdout)
                 in
                 let%bind () =
                   Deferred.List.iter choice.delta.images ~how:`Sequential ~f:(fun img ->
                     printf "\n<image: %d bytes>\n" (String.length img.image_url.url);
                     Writer.flushed (force Writer.stdout))
                 in
                 Deferred.List.iter
                   (List.filter_map choice.delta.annotations ~f:(fun ann ->
                      Completions.Citation.of_annotation_jsonaf ann))
                   ~how:`Sequential
                   ~f:(fun citation ->
                     printf
                       "\n📎 [%s](%s)\n"
                       (Option.value citation.title ~default:citation.url)
                       citation.url;
                     Writer.flushed (force Writer.stdout))))
         in
         let%bind () =
           match log_writer with
           | None -> return ()
           | Some w -> Writer.close w
         in
         print_endline "";
         Deferred.Or_error.return ()
       | Non_streaming request ->
         let%bind () =
           match request_to with
           | None -> return ()
           | Some path ->
             Writer.save
               path
               ~contents:
                 ([%jsonaf_of: [ `Non_streaming ] Completions.Request.t] request
                  |> Jsonaf.to_string_hum)
         in
         let%bind response =
           Completions.create
             ~api_key
             ~app_info
             ?on_response_body:
               (Option.map response_to ~f:(fun path body ->
                  Writer.save path ~contents:body))
             request
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
       let%map.Deferred.Or_error { data = models } =
         Models.list
           ~api_key
           ?on_response_body:
             (Option.map log_response_to ~f:(fun path body ->
                Writer.save path ~contents:body))
           ()
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
       let%bind.Deferred.Or_error api_key = api_key_from_env ()
       and inputs =
         (match inputs with
          | _ :: _ -> return inputs
          | [] ->
            let%map contents = Reader.contents (force Reader.stdin) in
            String.split_lines contents
            |> List.filter ~f:(fun line -> not (String.is_empty (String.strip line))))
         >>| Or_error.return
       in
       let%map.Deferred.Or_error
           ({ object_ = _; model; data; usage; provider = _; id = _ } as response)
         =
         Embeddings.create
           ~api_key
           ?on_response_body:
             (Option.map log_response_to ~f:(fun path body ->
                Writer.save path ~contents:body))
           { model
           ; input =
               (match inputs with
                | [ s ] -> Embeddings.Request.Input.Single s
                | xs -> Multi xs)
           ; dimensions
           }
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

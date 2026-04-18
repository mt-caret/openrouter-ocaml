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
     and web_search = flag "web-search" no_arg ~doc:" enable web search plugin"
     and files = flag "file" (listed string) ~doc:"PATH attach a file (PDF or image)"
     and temperature =
       flag "temperature" (optional float) ~doc:"FLOAT sampling temperature"
     and top_p = flag "top-p" (optional float) ~doc:"FLOAT nucleus sampling cutoff"
     and max_tokens = flag "max-tokens" (optional int) ~doc:"N max tokens in completion"
     and seed = flag "seed" (optional int) ~doc:"N RNG seed for reproducibility"
     and stop =
       flag "stop" (listed string) ~doc:"STR stop sequence (may be repeated, up to 4)"
     and frequency_penalty =
       flag "frequency-penalty" (optional float) ~doc:"FLOAT frequency penalty"
     and presence_penalty =
       flag "presence-penalty" (optional float) ~doc:"FLOAT presence penalty"
     and repetition_penalty =
       flag "repetition-penalty" (optional float) ~doc:"FLOAT repetition penalty"
     and json =
       flag "json" no_arg ~doc:" force JSON object output (response_format=json_object)"
     and json_schema_file =
       flag
         "json-schema"
         (optional Filename_unix.arg_type)
         ~doc:"FILE force output matching the JSON schema in FILE"
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
       let reasoning =
         Option.map reasoning_tokens ~f:(fun max_tokens ->
           { Completions.Request.Reasoning.max_tokens })
       in
       let plugins =
         match web_search with
         | true -> [ Completions.Plugin.web () ]
         | false -> []
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
         ; max_tokens
         ; seed
         ; stop = (if List.is_empty stop then None else Some stop)
         ; frequency_penalty
         ; presence_penalty
         ; repetition_penalty
         ; response_format
         }
       in
       match stream with
       | true ->
         let%bind () =
           Completions.create_stream ~api_key request
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
         let%bind response = Completions.create ~api_key request in
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
         Models.list ~api_key
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

let command =
  Command.group
    ~summary:"OpenRouter API example client"
    [ "chat", chat_command
    ; "list-models", list_models_command
    ; "embeddings", embeddings_command
    ]
;;

let () = Command_unix.run command

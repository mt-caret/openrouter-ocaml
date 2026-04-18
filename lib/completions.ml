open! Core
open! Async
open Jsonaf.Export

let endpoint_url = Uri.of_string "https://openrouter.ai/api/v1/chat/completions"

(* Reasoning details from models with extended thinking.
   Different providers use different formats:
   - Claude: has `text` and `signature` fields
   - Google Gemini: has `text` or `data` (for encrypted reasoning) fields
   In streaming mode, intermediate chunks have `text` but no `signature`.
   The final chunk has `signature` but may have empty `text`. *)
module Reasoning_detail = struct
  type t =
    { format : string
    ; index : int
    ; type_ : string [@key "type"]
    ; text : string option [@default None] [@jsonaf_drop_default.equal]
    ; signature : string option [@default None] [@jsonaf_drop_default.equal]
    ; data : string option [@default None] [@jsonaf_drop_default.equal]
      (* Google's encrypted reasoning *)
    }
  [@@deriving equal, jsonaf, sexp_of]
end

(* Image in a message response *)
module Image = struct
  module Image_url = struct
    type t = { url : string } [@@deriving equal, jsonaf, sexp_of]
  end

  type t =
    { type_ : string [@key "type"]
    ; image_url : Image_url.t
    ; index : int
    }
  [@@deriving equal, jsonaf, sexp_of]

  module Elide_data = struct
    type nonrec t = t =
      { type_ : string
      ; image_url : (Image_url.t[@sexp.opaque])
      ; index : int
      }
    [@@deriving sexp_of]
  end
end

(* Tool calling types per OpenRouter API spec:
   https://openrouter.ai/docs/guides/features/tool-calling *)

module Tool = struct
  (* JSON value with equality (Jsonaf.t has exactly_equal but not equal). *)
  module Json_schema = struct
    type t = Jsonaf.t [@@deriving jsonaf, sexp_of]

    let equal = Jsonaf.exactly_equal
  end

  module Function = struct
    type t =
      { name : string
      ; description : string option [@jsonaf.option]
      ; parameters : Json_schema.t option [@jsonaf.option]
      }
    [@@deriving equal, jsonaf, sexp_of]
  end

  type t =
    { type_ : string [@key "type"] (* Always "function" *)
    ; function_ : Function.t [@key "function"]
    }
  [@@deriving equal, jsonaf, sexp_of]

  let create ~name ?description ?parameters () =
    { type_ = "function"; function_ = { name; description; parameters } }
  ;;
end

module Tool_choice = struct
  module Function_choice = struct
    type t = { name : string } [@@deriving jsonaf, sexp_of]
  end

  module Specific = struct
    type t =
      { type_ : string [@key "type"]
      ; function_ : Function_choice.t [@key "function"]
      }
    [@@deriving jsonaf, sexp_of]
  end

  type t =
    | Auto
    | None_
    | Required
    | Specific of Specific.t
  [@@deriving sexp_of]

  let t_of_jsonaf json =
    match json with
    | `String "auto" -> Auto
    | `String "none" -> None_
    | `String "required" -> Required
    | `Object _ -> Specific (Specific.t_of_jsonaf json)
    | _ -> Jsonaf_kernel.Conv.of_jsonaf_error "Invalid tool_choice" json
  ;;

  let jsonaf_of_t = function
    | Auto -> `String "auto"
    | None_ -> `String "none"
    | Required -> `String "required"
    | Specific s -> Specific.jsonaf_of_t s
  ;;

  let auto = Auto
  let none = None_
  let required = Required
  let force_function name = Specific { type_ = "function"; function_ = { name } }
end

module Tool_call = struct
  module Function_call = struct
    type t =
      { name : string
      ; arguments : string (* JSON-encoded string *)
      }
    [@@deriving equal, jsonaf, sexp_of]
  end

  type t =
    { id : string
    ; type_ : string [@key "type"]
    ; function_ : Function_call.t [@key "function"]
    }
  [@@deriving equal, jsonaf, sexp_of]
end

module Plugin = struct
  module Web = struct
    type t =
      { id : string
      ; enabled : bool option [@jsonaf.option]
      ; max_results : int option [@jsonaf.option]
      }
    [@@deriving equal, jsonaf, sexp_of]

    let default = { id = "web"; enabled = None; max_results = None }
  end

  module Pdf_engine = struct
    type t =
      | Pdf_text
      | Mistral_ocr
      | Native
    [@@deriving equal, sexp_of]

    let jsonaf_of_t = function
      | Pdf_text -> `String "pdf-text"
      | Mistral_ocr -> `String "mistral-ocr"
      | Native -> `String "native"
    ;;

    let t_of_jsonaf = function
      | `String "pdf-text" -> Pdf_text
      | `String "mistral-ocr" -> Mistral_ocr
      | `String "native" -> Native
      | json -> Jsonaf_kernel.Conv.of_jsonaf_error "Unknown PDF engine" json
    ;;
  end

  module File_parser = struct
    module Pdf_config = struct
      type t = { engine : Pdf_engine.t } [@@deriving equal, jsonaf, sexp_of]
    end

    type t =
      { id : string
      ; pdf : Pdf_config.t option [@jsonaf.option]
      }
    [@@deriving equal, jsonaf, sexp_of]

    let default = { id = "file-parser"; pdf = None }
  end

  type t =
    | Web of Web.t
    | File_parser of File_parser.t
  [@@deriving equal, sexp_of]

  let jsonaf_of_t = function
    | Web w -> Web.jsonaf_of_t w
    | File_parser f -> File_parser.jsonaf_of_t f
  ;;

  let t_of_jsonaf json =
    match Jsonaf.member "id" json with
    | Some (`String "web") -> Web (Web.t_of_jsonaf json)
    | Some (`String "file-parser") -> File_parser (File_parser.t_of_jsonaf json)
    | _ -> Jsonaf_kernel.Conv.of_jsonaf_error "Unknown plugin id" json
  ;;

  let web ?enabled ?max_results () = Web { id = "web"; enabled; max_results }

  let file_parser ?pdf_engine () =
    let pdf =
      Option.map pdf_engine ~f:(fun engine -> { File_parser.Pdf_config.engine })
    in
    File_parser { id = "file-parser"; pdf }
  ;;
end

module Citation = struct
  type t =
    { url : string
    ; title : string option [@jsonaf.option]
    ; content : string option [@jsonaf.option]
    ; start_index : int option [@jsonaf.option]
    ; end_index : int option [@jsonaf.option]
    }
  [@@deriving equal, of_jsonaf, sexp_of]

  let of_annotation_jsonaf json =
    match Jsonaf.member "type" json with
    | Some (`String "url_citation") ->
      (match Jsonaf.member "url_citation" json with
       | Some citation_json -> Some (t_of_jsonaf citation_json)
       | None -> None)
    | _ -> None
  ;;
end

module Message = struct
  type t =
    { role : string
    ; content : string option
          [@default None] (* content can be null when assistant makes tool calls *)
    ; refusal : string option [@default None]
    ; reasoning : string option [@default None]
    ; reasoning_details : Reasoning_detail.t list
          [@default []] [@jsonaf_drop_default.equal]
    ; images : Image.t list [@default []] [@jsonaf_drop_default.equal]
    ; annotations : Jsonaf.t list [@default []]
    ; tool_calls : Tool_call.t list
          [@default []] [@jsonaf_drop_default.equal] (* Tool calls made by assistant *)
    ; tool_call_id : string option [@default None] [@jsonaf_drop_default.equal]
      (* For tool role messages: ID of the tool call being responded to *)
    }
  [@@deriving jsonaf, sexp_of]

  module Elide_image = struct
    type nonrec t = t =
      { role : string
      ; content : string option
      ; refusal : string option
      ; reasoning : string option
      ; reasoning_details : Reasoning_detail.t list
      ; images : (Image.Elide_data.t list[@sexp.list])
      ; annotations : Jsonaf.t list
      ; tool_calls : Tool_call.t list
      ; tool_call_id : string option
      }
    [@@deriving sexp_of]
  end
end

module Request = struct
  module Message = struct
    module Tool_call = struct
      module Function_call = struct
        type t =
          { name : string
          ; arguments : string
          }
        [@@deriving equal, jsonaf, sexp_of]
      end

      type t =
        { id : string
        ; type_ : string [@key "type"]
        ; function_ : Function_call.t [@key "function"]
        }
      [@@deriving equal, jsonaf, sexp_of]
    end

    module Content_part = struct
      module Image_url = struct
        type t = { url : string } [@@deriving equal, jsonaf, sexp_of]
      end

      module File_data = struct
        type t =
          { filename : string
          ; file_data : string
          }
        [@@deriving equal, jsonaf, sexp_of]
      end

      type t =
        | Text of { text : string }
        | Image_url of { image_url : Image_url.t }
        | File of { file : File_data.t }
      [@@deriving equal, sexp_of]

      let text s = Text { text = s }

      let image_base64 ~mime_type ~data =
        let url = sprintf "data:%s;base64,%s" mime_type data in
        Image_url { image_url = { url } }
      ;;

      let file ~filename ~file_data = File { file = { filename; file_data } }

      let jsonaf_of_t = function
        | Text { text } -> `Object [ "type", `String "text"; "text", `String text ]
        | Image_url { image_url } ->
          `Object
            [ "type", `String "image_url"; "image_url", Image_url.jsonaf_of_t image_url ]
        | File { file } ->
          `Object [ "type", `String "file"; "file", File_data.jsonaf_of_t file ]
      ;;

      let t_of_jsonaf json =
        match Jsonaf.member "type" json with
        | Some (`String "text") ->
          (match Jsonaf.member "text" json with
           | Some (`String text) -> Text { text }
           | _ -> Jsonaf_kernel.Conv.of_jsonaf_error "Expected 'text' field" json)
        | Some (`String "image_url") ->
          (match Jsonaf.member "image_url" json with
           | Some image_url_json ->
             Image_url { image_url = Image_url.t_of_jsonaf image_url_json }
           | _ -> Jsonaf_kernel.Conv.of_jsonaf_error "Expected 'image_url' field" json)
        | Some (`String "file") ->
          (match Jsonaf.member "file" json with
           | Some file_json -> File { file = File_data.t_of_jsonaf file_json }
           | _ -> Jsonaf_kernel.Conv.of_jsonaf_error "Expected 'file' field" json)
        | _ -> Jsonaf_kernel.Conv.of_jsonaf_error "Unknown content part type" json
      ;;
    end

    module Content = struct
      type t =
        | Text of string
        | Multipart of Content_part.t list
      [@@deriving equal, sexp_of]

      let text s = Text s
      let multipart parts = Multipart parts

      let jsonaf_of_t = function
        | Text s -> `String s
        | Multipart parts -> `Array (List.map parts ~f:Content_part.jsonaf_of_t)
      ;;

      let t_of_jsonaf = function
        | `String s -> Text s
        | `Array arr -> Multipart (List.map arr ~f:Content_part.t_of_jsonaf)
        | json -> Jsonaf_kernel.Conv.of_jsonaf_error "Expected string or array" json
      ;;
    end

    type t =
      { role : string
      ; content : Content.t option
            [@default None]
            [@jsonaf_drop_default.equal]
            (* Content can be null for assistant messages with tool_calls *)
      ; tool_calls : Tool_call.t list
            [@default []]
            [@jsonaf_drop_default.equal]
            (* Tool calls for assistant role (when replaying conversation) *)
      ; tool_call_id : string option [@default None] [@jsonaf_drop_default.equal]
        (* For tool role: ID of the tool call being responded to *)
      }
    [@@deriving jsonaf, sexp_of]

    let user content =
      { role = "user"
      ; content = Some (Content.Text content)
      ; tool_calls = []
      ; tool_call_id = None
      }
    ;;

    let user_multipart parts =
      { role = "user"
      ; content = Some (Content.Multipart parts)
      ; tool_calls = []
      ; tool_call_id = None
      }
    ;;

    let system content =
      { role = "system"
      ; content = Some (Content.Text content)
      ; tool_calls = []
      ; tool_call_id = None
      }
    ;;

    let assistant ?content ?(tool_calls = []) () =
      { role = "assistant"
      ; content = Option.map content ~f:(fun c -> Content.Text c)
      ; tool_calls
      ; tool_call_id = None
      }
    ;;

    let tool ~tool_call_id ~content =
      { role = "tool"
      ; content = Some (Content.Text content)
      ; tool_calls = []
      ; tool_call_id = Some tool_call_id
      }
    ;;
  end

  module Reasoning = struct
    type t = { max_tokens : int } [@@deriving jsonaf, sexp_of]
  end

  module Response_format = struct
    module Json_schema = struct
      type t =
        { name : string
        ; strict : bool option [@jsonaf.option]
        ; schema : Jsonaf.t option [@jsonaf.option]
        ; description : string option [@jsonaf.option]
        }
      [@@deriving jsonaf, sexp_of]
    end

    type t =
      | Json_object
      | Json_schema of Json_schema.t
    [@@deriving sexp_of]

    let json_schema ?strict ?schema ?description ~name () =
      Json_schema { Json_schema.name; strict; schema; description }
    ;;

    let jsonaf_of_t = function
      | Json_object -> `Object [ "type", `String "json_object" ]
      | Json_schema schema ->
        `Object
          [ "type", `String "json_schema"; "json_schema", Json_schema.jsonaf_of_t schema ]
    ;;

    let t_of_jsonaf json =
      match Jsonaf.member "type" json with
      | Some (`String "json_object") -> Json_object
      | Some (`String "json_schema") ->
        (match Jsonaf.member "json_schema" json with
         | Some s -> Json_schema (Json_schema.t_of_jsonaf s)
         | None -> Jsonaf_kernel.Conv.of_jsonaf_error "Missing 'json_schema' field" json)
      | _ -> Jsonaf_kernel.Conv.of_jsonaf_error "Unknown response_format type" json
    ;;
  end

  type t =
    { model : string
    ; messages : Message.t list
    ; stream : bool
    ; reasoning : Reasoning.t option [@jsonaf.option]
    ; tools : Tool.t list [@default []] [@jsonaf_drop_default.equal]
    ; tool_choice : Tool_choice.t option [@jsonaf.option]
    ; parallel_tool_calls : bool option [@jsonaf.option]
    ; plugins : Plugin.t list [@default []] [@jsonaf_drop_default.equal]
    ; temperature : float option [@jsonaf.option]
    ; top_p : float option [@jsonaf.option]
    ; max_tokens : int option [@jsonaf.option]
    ; seed : int option [@jsonaf.option]
    ; stop : string list option [@jsonaf.option]
    ; frequency_penalty : float option [@jsonaf.option]
    ; presence_penalty : float option [@jsonaf.option]
    ; repetition_penalty : float option [@jsonaf.option]
    ; response_format : Response_format.t option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp_of]
end

module Response = struct
  module Usage = struct
    module Prompt_tokens_details = struct
      type t =
        { cached_tokens : int option [@default None]
        ; cache_write_tokens : int option [@default None]
        ; audio_tokens : int option [@default None]
        ; video_tokens : int option [@default None]
        }
      [@@deriving of_jsonaf, sexp_of]
    end

    module Cost_details = struct
      type t =
        { upstream_inference_cost : float option [@default None]
        ; upstream_inference_prompt_cost : float option [@default None]
        ; upstream_inference_completions_cost : float option [@default None]
        }
      [@@deriving of_jsonaf, sexp_of]
    end

    module Completion_tokens_details = struct
      type t =
        { reasoning_tokens : int option [@default None]
        ; image_tokens : int option [@default None]
        ; audio_tokens : int option [@default None]
        }
      [@@deriving of_jsonaf, sexp_of]
    end

    module Server_tool_use = struct
      type t = { web_search_requests : int option [@default None] }
      [@@deriving of_jsonaf, sexp_of]
    end

    type t =
      { prompt_tokens : int
      ; completion_tokens : int
      ; total_tokens : int
      ; cost : float option [@default None]
      ; is_byok : bool option [@default None]
      ; prompt_tokens_details : Prompt_tokens_details.t option [@default None]
      ; cost_details : Cost_details.t option [@default None]
      ; completion_tokens_details : Completion_tokens_details.t option [@default None]
      ; server_tool_use : Server_tool_use.t option [@default None]
      }
    [@@deriving of_jsonaf, sexp_of, fields ~getters]
  end

  module Choice = struct
    type logprobs = unit option [@@deriving of_jsonaf, sexp_of]

    type t =
      { logprobs : logprobs
      ; finish_reason : string
      ; native_finish_reason : string
      ; index : int
      ; message : Message.t
      }
    [@@deriving of_jsonaf, sexp_of]

    module Elide_image = struct
      type nonrec t = t =
        { logprobs : logprobs
        ; finish_reason : string
        ; native_finish_reason : string
        ; index : int
        ; message : Message.Elide_image.t
        }
      [@@deriving sexp_of]
    end
  end

  type t =
    { id : string
    ; provider : string
    ; model : string
    ; object_ : string [@key "object"]
    ; created : int
    ; choices : Choice.t list
    ; system_fingerprint : string option [@jsonaf.option]
    ; service_tier : string option [@default None]
    ; usage : Usage.t
    }
  [@@deriving of_jsonaf, sexp_of]

  module Elide_image = struct
    type nonrec t = t =
      { id : string
      ; provider : string
      ; model : string
      ; object_ : string
      ; created : int
      ; choices : (Choice.Elide_image.t list[@sexp.list])
      ; system_fingerprint : string option
      ; service_tier : string option
      ; usage : Usage.t
      }
    [@@deriving sexp_of]
  end
end

module Stream_chunk = struct
  module Tool_call_chunk = struct
    module Function_call = struct
      type t =
        { name : string option [@default None] [@jsonaf_drop_default.equal]
        ; arguments : string option [@default None] [@jsonaf_drop_default.equal]
        }
      [@@deriving of_jsonaf, sexp_of]
    end

    type t =
      { index : int
      ; id : string option [@default None] [@jsonaf_drop_default.equal]
      ; type_ : string option [@default None] [@jsonaf_drop_default.equal] [@key "type"]
      ; function_ : Function_call.t option
            [@default None] [@jsonaf_drop_default.equal] [@key "function"]
      }
    [@@deriving of_jsonaf, sexp_of]
  end

  module Delta = struct
    type t =
      { role : string option [@default None] [@jsonaf_drop_default.equal]
      ; content : string option [@default None] [@jsonaf_drop_default.equal]
      ; refusal : string option [@default None] [@jsonaf_drop_default.equal]
      ; reasoning : string option [@default None] [@jsonaf_drop_default.equal]
      ; reasoning_details : Reasoning_detail.t list
            [@default []] [@jsonaf_drop_default.equal]
      ; images : Image.t list [@default []] [@jsonaf_drop_default.equal]
      ; annotations :
          (* Annotations from some providers (e.g., Google) - currently just stored as JSON *)
          Jsonaf.t list
            [@default []] [@jsonaf_drop_default.equal]
      ; tool_calls : Tool_call_chunk.t list [@default []] [@jsonaf_drop_default.equal]
      }
    [@@deriving of_jsonaf, sexp_of]

    module Elide_image = struct
      type nonrec t = t =
        { role : string option
        ; content : string option
        ; refusal : string option
        ; reasoning : string option
        ; reasoning_details : Reasoning_detail.t list
        ; images : (Image.Elide_data.t list[@sexp.list])
        ; annotations : Jsonaf.t list
        ; tool_calls : Tool_call_chunk.t list
        }
      [@@deriving sexp_of]
    end
  end

  module Choice = struct
    type logprobs = unit option [@@deriving of_jsonaf, sexp_of]

    type t =
      { logprobs : logprobs [@default None]
      ; finish_reason : string option
      ; native_finish_reason : string option
      ; index : int
      ; delta : Delta.t
      }
    [@@deriving of_jsonaf, sexp_of]
  end

  type t =
    { id : string
    ; provider : string
    ; model : string
    ; object_ : string [@key "object"]
    ; created : int
    ; choices : Choice.t list
    ; system_fingerprint : string option [@jsonaf.option]
    ; service_tier : string option [@default None]
    ; usage : Response.Usage.t option [@jsonaf.option]
    }
  [@@deriving of_jsonaf, sexp_of]
end

let create ~api_key (request : Request.t) =
  let headers = Http.make_headers ~api_key in
  let body =
    [%jsonaf_of: Request.t] request |> Jsonaf.to_string |> Cohttp_async.Body.of_string
  in
  let%bind response, body = Cohttp_async.Client.post ~headers ~body endpoint_url in
  let%map body_string = Cohttp_async.Body.to_string body in
  let%bind.Or_error () =
    match Http.is_success_status response with
    | true -> Ok ()
    | false ->
      let error_message =
        match Jsonaf.parse body_string with
        | Ok json -> Api_error.of_json_or_body ~body_string json
        | Error _ -> body_string
      in
      let status = Cohttp.Response.status response in
      Or_error.error_s
        [%message
          "OpenRouter API error"
            (status : Cohttp.Code.status_code)
            (error_message : string)]
  in
  let%bind.Or_error json =
    Jsonaf.parse body_string
    |> Or_error.tag_s_lazy
         ~tag:
           (lazy
             [%message
               "Failed to parse response body into JSON"
                 (response : Cohttp.Response.t)
                 (body_string : string)])
  in
  Or_error.try_with (fun () -> [%of_jsonaf: Response.t] json)
  |> Or_error.tag_s_lazy
       ~tag:
         (lazy
           [%message
             "Failed to parse JSON into Response.t"
               (response : Cohttp.Response.t)
               (json : Jsonaf.t)])
;;

let lines_of_chunks (chunks : string Pipe.Reader.t) : string Pipe.Reader.t =
  Pipe.create_reader ~close_on_exception:true (fun writer ->
    let buffer = ref "" in
    Pipe.iter chunks ~f:(fun chunk ->
      buffer := !buffer ^ chunk;
      match String.split_on_chars !buffer ~on:[ '\n' ] with
      | [] -> Deferred.unit
      | lines ->
        buffer := List.last_exn lines;
        List.drop_last_exn lines
        |> Deferred.List.iter ~how:`Sequential ~f:(fun line ->
          match String.strip line with
          | "" -> Deferred.unit
          | line -> Pipe.write writer line)))
;;

let create_stream ~api_key (request : Request.t) =
  let headers = Http.make_headers ~api_key in
  let body =
    [%jsonaf_of: Request.t] request |> Jsonaf.to_string |> Cohttp_async.Body.of_string
  in
  let%bind response, body = Cohttp_async.Client.post ~headers ~body endpoint_url in
  match Http.is_success_status response with
  | false ->
    let%map body_string = Cohttp_async.Body.to_string body in
    let error_message =
      match Jsonaf.parse body_string with
      | Ok json -> Api_error.of_json_or_body ~body_string json
      | Error _ -> body_string
    in
    let status = Cohttp.Response.status response in
    let error =
      Error.create_s
        [%message
          "OpenRouter API error"
            (status : Cohttp.Code.status_code)
            (error_message : string)]
    in
    Pipe.of_list [ Error error ]
  | true ->
    return
      (Cohttp_async.Body.to_pipe body
       |> lines_of_chunks
       |> Pipe.filter_map ~f:(fun line ->
         let parse_result =
           match
             String.lsplit2 line ~on:':' |> Option.map ~f:(Tuple2.map_snd ~f:String.strip)
           with
           | Some ("", comment) ->
             (* SSE lines starting with : are comments (e.g., ": OPENROUTER PROCESSING") *)
             `Comment comment
           | Some ("data", "[DONE]") -> `Done
           | Some ("data", content) -> `Data content
           | None | Some (_, _) -> `Unknown line
         in
         [%log.global.debug
           "SSE line"
             (parse_result
              : [ `Comment of string | `Done | `Data of string | `Unknown of string ])];
         let%map.Option data =
           match parse_result with
           | `Comment _ | `Done | `Unknown _ -> None
           | `Data data -> Some data
         in
         let%bind.Or_error json =
           Jsonaf.parse data
           |> Or_error.tag_s_lazy
                ~tag:
                  (lazy
                    [%message
                      "Failed to parse SSE data as JSON"
                        (response : Cohttp.Response.t)
                        (data : string)])
         in
         let%map.Or_error stream_chunk =
           Or_error.try_with (fun () -> [%of_jsonaf: Stream_chunk.t] json)
           |> Or_error.tag_s_lazy
                ~tag:
                  (lazy
                    [%message
                      "Failed to parse Stream_chunk"
                        (response : Cohttp.Response.t)
                        (json : Jsonaf.t)])
         in
         [%log.global.debug "Stream chunk" (stream_chunk : Stream_chunk.t)];
         stream_chunk))
;;

let%expect_test "request" =
  [%jsonaf_of: Request.t]
    { model = "openai/gpt-4o"
    ; messages = [ Request.Message.user "Hello, world!" ]
    ; stream = false
    ; reasoning = None
    ; tools = []
    ; tool_choice = None
    ; parallel_tool_calls = None
    ; plugins = []
    ; temperature = None
    ; top_p = None
    ; max_tokens = None
    ; seed = None
    ; stop = None
    ; frequency_penalty = None
    ; presence_penalty = None
    ; repetition_penalty = None
    ; response_format = None
    }
  |> Jsonaf.to_string_hum
  |> print_endline;
  [%expect
    {|
    {
      "model": "openai/gpt-4o",
      "messages": [
        {
          "role": "user",
          "content": "Hello, world!"
        }
      ],
      "stream": false
    }
    |}];
  Deferred.unit
;;

let%expect_test "request with tools" =
  let search_tool =
    Tool.create
      ~name:"search_books"
      ~description:"Search for books in a library"
      ~parameters:
        (`Object
            [ "type", `String "object"
            ; ( "properties"
              , `Object
                  [ ( "query"
                    , `Object
                        [ "type", `String "string"
                        ; "description", `String "Search query"
                        ] )
                  ] )
            ; "required", `Array [ `String "query" ]
            ])
      ()
  in
  [%jsonaf_of: Request.t]
    { model = "google/gemini-2.0-flash-001"
    ; messages = [ Request.Message.user "Find books about OCaml" ]
    ; stream = false
    ; reasoning = None
    ; tools = [ search_tool ]
    ; tool_choice = Some Tool_choice.auto
    ; parallel_tool_calls = None
    ; plugins = []
    ; temperature = None
    ; top_p = None
    ; max_tokens = None
    ; seed = None
    ; stop = None
    ; frequency_penalty = None
    ; presence_penalty = None
    ; repetition_penalty = None
    ; response_format = None
    }
  |> Jsonaf.to_string_hum
  |> print_endline;
  [%expect
    {|
    {
      "model": "google/gemini-2.0-flash-001",
      "messages": [
        {
          "role": "user",
          "content": "Find books about OCaml"
        }
      ],
      "stream": false,
      "tools": [
        {
          "type": "function",
          "function": {
            "name": "search_books",
            "description": "Search for books in a library",
            "parameters": {
              "type": "object",
              "properties": {
                "query": {
                  "type": "string",
                  "description": "Search query"
                }
              },
              "required": [
                "query"
              ]
            }
          }
        }
      ],
      "tool_choice": "auto"
    }
    |}];
  Deferred.unit
;;

let%expect_test "tool result message" =
  [%jsonaf_of: Request.t]
    { model = "openai/gpt-4o"
    ; messages =
        [ Request.Message.user "Find books about OCaml"
        ; Request.Message.assistant
            ~tool_calls:
              [ { id = "call_abc123"
                ; type_ = "function"
                ; function_ =
                    { name = "search_books"; arguments = {|{"query": "OCaml"}|} }
                }
              ]
            ()
        ; Request.Message.tool
            ~tool_call_id:"call_abc123"
            ~content:{|[{"title": "Real World OCaml", "author": "Yaron Minsky"}]|}
        ]
    ; stream = false
    ; reasoning = None
    ; tools = []
    ; tool_choice = None
    ; parallel_tool_calls = None
    ; plugins = []
    ; temperature = None
    ; top_p = None
    ; max_tokens = None
    ; seed = None
    ; stop = None
    ; frequency_penalty = None
    ; presence_penalty = None
    ; repetition_penalty = None
    ; response_format = None
    }
  |> Jsonaf.to_string_hum
  |> print_endline;
  [%expect
    {|
    {
      "model": "openai/gpt-4o",
      "messages": [
        {
          "role": "user",
          "content": "Find books about OCaml"
        },
        {
          "role": "assistant",
          "tool_calls": [
            {
              "id": "call_abc123",
              "type": "function",
              "function": {
                "name": "search_books",
                "arguments": "{\"query\": \"OCaml\"}"
              }
            }
          ]
        },
        {
          "role": "tool",
          "content": "[{\"title\": \"Real World OCaml\", \"author\": \"Yaron Minsky\"}]",
          "tool_call_id": "call_abc123"
        }
      ],
      "stream": false
    }
    |}];
  Deferred.unit
;;

let%expect_test "request with web search plugin" =
  [%jsonaf_of: Request.t]
    { model = "openai/gpt-4o"
    ; messages = [ Request.Message.user "What's happening in AI today?" ]
    ; stream = true
    ; reasoning = None
    ; tools = []
    ; tool_choice = None
    ; parallel_tool_calls = None
    ; plugins = [ Plugin.web () ]
    ; temperature = None
    ; top_p = None
    ; max_tokens = None
    ; seed = None
    ; stop = None
    ; frequency_penalty = None
    ; presence_penalty = None
    ; repetition_penalty = None
    ; response_format = None
    }
  |> Jsonaf.to_string_hum
  |> print_endline;
  [%expect
    {|
    {
      "model": "openai/gpt-4o",
      "messages": [
        {
          "role": "user",
          "content": "What's happening in AI today?"
        }
      ],
      "stream": true,
      "plugins": [
        {
          "id": "web"
        }
      ]
    }
    |}];
  Deferred.unit
;;

let%expect_test "request with file attachment" =
  [%jsonaf_of: Request.t]
    { model = "anthropic/claude-sonnet-4"
    ; messages =
        [ Request.Message.user_multipart
            [ Request.Message.Content_part.text
                "What are the main points in this document?"
            ; Request.Message.Content_part.file
                ~filename:"document.pdf"
                ~file_data:"https://bitcoin.org/bitcoin.pdf"
            ]
        ]
    ; stream = false
    ; reasoning = None
    ; tools = []
    ; tool_choice = None
    ; parallel_tool_calls = None
    ; plugins = []
    ; temperature = None
    ; top_p = None
    ; max_tokens = None
    ; seed = None
    ; stop = None
    ; frequency_penalty = None
    ; presence_penalty = None
    ; repetition_penalty = None
    ; response_format = None
    }
  |> Jsonaf.to_string_hum
  |> print_endline;
  [%expect
    {|
    {
      "model": "anthropic/claude-sonnet-4",
      "messages": [
        {
          "role": "user",
          "content": [
            {
              "type": "text",
              "text": "What are the main points in this document?"
            },
            {
              "type": "file",
              "file": {
                "filename": "document.pdf",
                "file_data": "https://bitcoin.org/bitcoin.pdf"
              }
            }
          ]
        }
      ],
      "stream": false
    }
    |}];
  Deferred.unit
;;

let%expect_test "request with file-parser plugin" =
  [%jsonaf_of: Request.t]
    { model = "anthropic/claude-sonnet-4"
    ; messages =
        [ Request.Message.user_multipart
            [ Request.Message.Content_part.text "Summarize this PDF"
            ; Request.Message.Content_part.file
                ~filename:"document.pdf"
                ~file_data:"data:application/pdf;base64,JVBERi0xLjQ..."
            ]
        ]
    ; stream = false
    ; reasoning = None
    ; tools = []
    ; tool_choice = None
    ; parallel_tool_calls = None
    ; plugins = [ Plugin.file_parser ~pdf_engine:Mistral_ocr () ]
    ; temperature = None
    ; top_p = None
    ; max_tokens = None
    ; seed = None
    ; stop = None
    ; frequency_penalty = None
    ; presence_penalty = None
    ; repetition_penalty = None
    ; response_format = None
    }
  |> Jsonaf.to_string_hum
  |> print_endline;
  [%expect
    {|
    {
      "model": "anthropic/claude-sonnet-4",
      "messages": [
        {
          "role": "user",
          "content": [
            {
              "type": "text",
              "text": "Summarize this PDF"
            },
            {
              "type": "file",
              "file": {
                "filename": "document.pdf",
                "file_data": "data:application/pdf;base64,JVBERi0xLjQ..."
              }
            }
          ]
        }
      ],
      "stream": false,
      "plugins": [
        {
          "id": "file-parser",
          "pdf": {
            "engine": "mistral-ocr"
          }
        }
      ]
    }
    |}];
  Deferred.unit
;;

let%expect_test "citation parsing from annotation" =
  let annotation_json =
    Jsonaf.parse
      {|{
        "type": "url_citation",
        "url_citation": {
          "url": "https://example.com/article",
          "title": "Example Article",
          "content": "Some snippet from the article...",
          "start_index": 10,
          "end_index": 50
        }
      }|}
    |> Or_error.ok_exn
  in
  (match Citation.of_annotation_jsonaf annotation_json with
   | Some citation -> print_s [%sexp (citation : Citation.t)]
   | None -> print_endline "No citation found");
  [%expect
    {|
    ((url https://example.com/article) (title ("Example Article"))
     (content ("Some snippet from the article...")) (start_index (10))
     (end_index (50)))
    |}];
  Deferred.unit
;;

open! Core
open! Async
open Jsonaf.Export

let endpoint_url = Uri.of_string "https://openrouter.ai/api/v1/embeddings"

module Request = struct
  module Input = struct
    type t =
      | Single of string
      | Multi of string list
    [@@deriving sexp_of, variants]

    let jsonaf_of_t = function
      | Single s -> `String s
      | Multi xs -> `Array (List.map xs ~f:(fun s -> `String s))
    ;;

    let t_of_jsonaf = function
      | `String s -> Single s
      | `Array xs ->
        Multi
          (List.map xs ~f:(function
             | `String s -> s
             | json ->
               Jsonaf_kernel.Conv.of_jsonaf_error "Expected string in input array" json))
      | json -> Jsonaf_kernel.Conv.of_jsonaf_error "Expected string or array" json
    ;;
  end

  type t =
    { model : string
    ; input : Input.t
    ; dimensions : int option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp_of]
end

module Response = struct
  module Embedding = struct
    type t =
      { object_ : string [@key "object"]
      ; index : int
      ; embedding : float list
      }
    [@@deriving of_jsonaf, sexp_of]
  end

  module Usage = struct
    type t =
      { prompt_tokens : int
      ; total_tokens : int
      ; cost : float option [@default None]
      }
    [@@deriving of_jsonaf, sexp_of]
  end

  type t =
    { object_ : string [@key "object"]
    ; model : string
    ; data : Embedding.t list
    ; usage : Usage.t
    ; provider : string option [@default None]
    ; id : string option [@default None]
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

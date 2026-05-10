open! Core
open! Async
open Jsonaf.Export

let endpoint_url = Uri.of_string "https://openrouter.ai/api/v1/models"

(* Models API types - exhaustively defined per OpenRouter API spec *)
module Model_info = struct
  module Pricing = struct
    type t =
      { prompt : String_float.t
      ; completion : String_float.t
      ; request : String_float.t option [@default None]
      ; image : String_float.t option [@default None]
      ; image_token : String_float.t option [@default None]
      ; image_output : String_float.t option [@default None]
      ; audio : String_float.t option [@default None]
      ; input_audio_cache : String_float.t option [@default None]
      ; web_search : String_float.t option [@default None]
      ; internal_reasoning : String_float.t option [@default None]
      ; input_cache_read : String_float.t option [@default None]
      ; input_cache_write : String_float.t option [@default None]
      ; discount : float option [@default None]
      }
    [@@fields.no_zero_alloc]
    [@@deriving fields ~getters, jsonaf, sexp_of]
    [@@jsonaf.allow_extra_fields]
  end

  module Architecture = struct
    type t =
      { tokenizer : string option [@default None]
      ; instruct_type : string option [@default None]
      ; modality : string option [@default None]
      ; input_modalities : string list [@default []]
      ; output_modalities : string list [@default []]
      }
    [@@deriving fields ~getters, jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
  end

  module Top_provider = struct
    type t =
      { context_length : int option [@default None]
      ; max_completion_tokens : int option [@default None]
      ; is_moderated : bool
      }
    [@@deriving fields ~getters, jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
  end

  module Per_request_limits = struct
    type t =
      { prompt_tokens : float
      ; completion_tokens : float
      }
    [@@fields.no_zero_alloc]
    [@@deriving fields ~getters, jsonaf, sexp_of]
    [@@jsonaf.allow_extra_fields]
  end

  module Default_parameters = struct
    type t =
      { temperature : float option [@default None]
      ; top_p : float option [@default None]
      ; top_k : float option [@default None]
      ; frequency_penalty : float option [@default None]
      ; presence_penalty : float option [@default None]
      ; repetition_penalty : float option [@default None]
      }
    [@@fields.no_zero_alloc]
    [@@deriving fields ~getters, jsonaf, sexp_of]
    [@@jsonaf.allow_extra_fields]
  end

  module Links = struct
    type t = { details : string option [@default None] }
    [@@deriving fields ~getters, jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
  end

  type t =
    { id : string
    ; canonical_slug : string
    ; hugging_face_id : string option [@default None]
    ; name : string
    ; created : int
    ; description : string option [@default None]
    ; context_length : int option [@default None]
    ; pricing : Pricing.t
    ; architecture : Architecture.t
    ; top_provider : Top_provider.t
    ; per_request_limits : Per_request_limits.t option [@default None]
    ; supported_parameters : string list [@default []]
    ; default_parameters : Default_parameters.t option [@default None]
    ; knowledge_cutoff : string option [@default None]
    ; expiration_date : string option [@default None]
    ; links : Links.t option [@default None]
    }
  [@@deriving fields ~getters, jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
end

module Response = struct
  type t = { data : Model_info.t list }
  [@@deriving jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
end

let list ~api_key ?app_info ?on_response_body () =
  let headers = Http.make_headers ~api_key ?app_info () in
  let%bind response, body = Cohttp_async.Client.get ~headers endpoint_url in
  let%bind body_string = Cohttp_async.Body.to_string body in
  let%map () =
    match on_response_body with
    | None -> return ()
    | Some f -> f body_string
  in
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

module For_testing = struct
  let response_of_jsonaf = [%of_jsonaf: Response.t]
end

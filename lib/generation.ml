open! Core
open! Async
open Jsonaf.Export

let endpoint_url = Uri.of_string "https://openrouter.ai/api/v1/generation"

module Provider_response = struct
  type t =
    { endpoint_id : string option [@default None]
    ; id : string option [@default None]
    ; is_byok : bool option [@default None]
    ; latency : int option [@default None]
    ; model_permaslug : string option [@default None]
    ; provider_name : string option [@default None]
    ; status : int option [@default None]
    }
  [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
end

module Stats = struct
  type t =
    { id : string
    ; model : string
    ; provider_name : string option [@default None]
    ; created_at : string
    ; api_type : string option [@default None]
    ; origin : string option [@default None]
    ; user_agent : string option [@default None]
    ; http_referer : string option [@default None]
    ; session_id : string option [@default None]
    ; request_id : string option [@default None]
    ; upstream_id : string option [@default None]
    ; app_id : Jsonaf.t option [@default None]
    ; external_user : Jsonaf.t option [@default None]
    ; router : Jsonaf.t option [@default None]
    ; streamed : bool option [@default None]
    ; cancelled : bool option [@default None]
    ; is_byok : bool option [@default None]
    ; finish_reason : string option [@default None]
    ; native_finish_reason : string option [@default None]
    ; service_tier : string option [@default None]
    ; latency : int option [@default None]
    ; moderation_latency : int option [@default None]
    ; generation_time : int option [@default None]
    ; tokens_prompt : int option [@default None]
    ; tokens_completion : int option [@default None]
    ; native_tokens_prompt : int option [@default None]
    ; native_tokens_completion : int option [@default None]
    ; native_tokens_completion_images : int option [@default None]
    ; native_tokens_reasoning : int option [@default None]
    ; native_tokens_cached : int option [@default None]
    ; num_media_prompt : int option [@default None]
    ; num_input_audio_prompt : int option [@default None]
    ; num_media_completion : int option [@default None]
    ; num_search_results : int option [@default None]
    ; num_fetches : int option [@default None]
    ; web_search_engine : string option [@default None]
    ; usage : float option [@default None]
    ; total_cost : float option [@default None]
    ; upstream_inference_cost : float option [@default None]
    ; cache_discount : float option [@default None]
    ; response_cache_source_id : string option [@default None]
    ; provider_responses : Provider_response.t list [@default []]
    }
  [@@deriving fields ~iterators:to_list, of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
end

module Response = struct
  type t = { data : Stats.t } [@@deriving of_jsonaf, sexp_of]
end

let get ~api_key ?app_info ~id () =
  let headers = Http.make_headers ~api_key ?app_info () in
  let uri = Uri.add_query_param' endpoint_url ("id", id) in
  let%bind response, body = Cohttp_async.Client.get ~headers uri in
  let%map body_string = Cohttp_async.Body.to_string body in
  let status = Cohttp.Response.status response in
  match Http.is_success_status response, status with
  | false, `Not_found -> Ok None
  | false, _ ->
    let error_message =
      match Jsonaf.parse body_string with
      | Ok json -> Api_error.of_json_or_body ~body_string json
      | Error _ -> body_string
    in
    Or_error.error_s
      [%message
        "OpenRouter API error" (status : Cohttp.Code.status_code) (error_message : string)]
  | true, _ ->
    let%bind.Or_error json =
      Jsonaf.parse body_string
      |> Or_error.tag_s_lazy
           ~tag:
             (lazy
               [%message
                 "Failed to parse generation response into JSON"
                   (response : Cohttp.Response.t)
                   (body_string : string)])
    in
    Or_error.try_with (fun () -> Some ([%of_jsonaf: Response.t] json).data)
    |> Or_error.tag_s_lazy
         ~tag:
           (lazy
             [%message
               "Failed to parse JSON into Generation.Stats.t"
                 (response : Cohttp.Response.t)
                 (json : Jsonaf.t)])
;;

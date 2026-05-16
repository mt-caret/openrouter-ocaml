(** Query generation stats for a previously-issued completion via the
    [GET /api/v1/generation] endpoint. The generation id is the same [id] returned
    on a chat completion or stream chunk; stats become queryable a few seconds
    after the request finishes. *)

open! Core
open! Async

module Provider_response : sig
  type t =
    { endpoint_id : string option
    ; id : string option
    ; is_byok : bool option
    ; latency : int option
    ; model_permaslug : string option
    ; provider_name : string option
    ; status : int option
    }
  [@@deriving sexp]
end

(** Stats for a single generation. Many fields are nullable on the wire; we keep
    them as [option]s rather than papering over with defaults. *)
module Stats : sig
  type t =
    { id : string
    ; model : string
    ; provider_name : string option
    ; created_at : string
    ; api_type : string option
    ; origin : string option
    ; user_agent : string option
    ; http_referer : string option
    ; session_id : string option
    ; request_id : string option
    ; upstream_id : string option
    ; app_id : Jsonaf.t option
    ; external_user : Jsonaf.t option
    ; router : Jsonaf.t option
    ; streamed : bool option
    ; cancelled : bool option
    ; is_byok : bool option
    ; finish_reason : string option
    ; native_finish_reason : string option
    ; service_tier : string option
    ; latency : int option
    ; moderation_latency : int option
    ; generation_time : int option
    ; tokens_prompt : int option
    ; tokens_completion : int option
    ; native_tokens_prompt : int option
    ; native_tokens_completion : int option
    ; native_tokens_completion_images : int option
    ; native_tokens_reasoning : int option
    ; native_tokens_cached : int option
    ; num_media_prompt : int option
    ; num_input_audio_prompt : int option
    ; num_media_completion : int option
    ; num_search_results : int option
    ; num_fetches : int option
    ; web_search_engine : string option
    ; usage : float option
    ; total_cost : float option
    ; upstream_inference_cost : float option
    ; cache_discount : float option
    ; response_cache_source_id : string option
    ; provider_responses : Provider_response.t list
    }
  [@@deriving fields ~iterators:to_list, sexp]
end

(** [get ~api_key ~id ()] resolves to [Some stats] once the generation has been
    persisted. Returns [None] if the generation is not yet (or not ever)
    queryable — the endpoint typically takes a couple of seconds after the
    completion to materialize. *)
val get
  :  api_key:string
  -> ?app_info:Http.App_info.t
  -> ?on_response_body:(string -> unit Deferred.t)
  -> id:string
  -> unit
  -> Stats.t option Or_error.t Deferred.t

module For_testing : sig
  val stats_of_jsonaf : Jsonaf.t -> Stats.t
end

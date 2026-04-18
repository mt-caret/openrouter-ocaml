open! Core
open! Async

module Model_info : sig
  module Pricing : sig
    type t =
      { prompt : float
      ; completion : float
      ; request : float option
      ; image : float option
      ; image_token : float option
      ; image_output : float option
      ; audio : float option
      ; input_audio_cache : float option
      ; web_search : float option
      ; internal_reasoning : float option
      ; input_cache_read : float option
      ; input_cache_write : float option
      ; discount : float option
      }
    [@@fields.no_zero_alloc] [@@deriving fields ~getters, sexp_of]
  end

  module Architecture : sig
    type t =
      { tokenizer : string option
      ; instruct_type : string option
      ; modality : string option
      ; input_modalities : string list
      ; output_modalities : string list
      }
    [@@deriving fields ~getters, sexp_of]
  end

  module Top_provider : sig
    type t =
      { context_length : int option
      ; max_completion_tokens : int option
      ; is_moderated : bool
      }
    [@@deriving fields ~getters, sexp_of]
  end

  module Per_request_limits : sig
    type t =
      { prompt_tokens : float
      ; completion_tokens : float
      }
    [@@fields.no_zero_alloc] [@@deriving fields ~getters, sexp_of]
  end

  module Default_parameters : sig
    type t =
      { temperature : float option
      ; top_p : float option
      ; top_k : float option
      ; frequency_penalty : float option
      ; presence_penalty : float option
      ; repetition_penalty : float option
      }
    [@@fields.no_zero_alloc] [@@deriving fields ~getters, sexp_of]
  end

  module Links : sig
    type t = { details : string option } [@@deriving fields ~getters, sexp_of]
  end

  type t =
    { id : string
    ; canonical_slug : string
    ; hugging_face_id : string option
    ; name : string
    ; created : int
    ; description : string option
    ; context_length : int option
    ; pricing : Pricing.t
    ; architecture : Architecture.t
    ; top_provider : Top_provider.t
    ; per_request_limits : Per_request_limits.t option
    ; supported_parameters : string list
    ; default_parameters : Default_parameters.t option
    ; knowledge_cutoff : string option
    ; expiration_date : string option
    ; links : Links.t option
    }
  [@@deriving fields ~getters, sexp_of]
end

module Response : sig
  type t = { data : Model_info.t list } [@@deriving sexp_of]
end

val list : api_key:string -> Response.t Or_error.t Deferred.t

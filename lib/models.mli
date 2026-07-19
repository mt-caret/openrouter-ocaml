open! Core
open! Async

module Model_info : sig
  module Pricing : sig
    (** Pricing tier that replaces the base prices once the prompt reaches
        [min_prompt_tokens] (e.g. long-context surcharges). *)
    module Override : sig
      type t =
        { min_prompt_tokens : int
        ; prompt : float
        ; completion : float
        ; audio : float option
        ; input_audio_cache : float option
        ; input_cache_read : float option
        ; input_cache_write : float option
        ; input_cache_write_1h : float option
        }
      [@@fields.no_zero_alloc] [@@deriving fields ~getters, sexp]
    end

    type t =
      { prompt : float
      ; completion : float
      ; request : float option
      ; image : float option
      ; image_token : float option
      ; image_output : float option
      ; audio : float option
      ; audio_output : float option
      ; input_audio_cache : float option
      ; web_search : float option
      ; internal_reasoning : float option
      ; input_cache_read : float option
      ; input_cache_write : float option
      ; input_cache_write_1h : float option
      ; discount : float option
      ; overrides : Override.t list
      }
    [@@fields.no_zero_alloc] [@@deriving fields ~getters, sexp]
  end

  module Architecture : sig
    type t =
      { tokenizer : string option
      ; instruct_type : string option
      ; modality : string option
      ; input_modalities : string list
      ; output_modalities : string list
      }
    [@@deriving fields ~getters, sexp]
  end

  module Top_provider : sig
    type t =
      { context_length : int option
      ; max_completion_tokens : int option
      ; is_moderated : bool
      }
    [@@deriving fields ~getters, sexp]
  end

  module Per_request_limits : sig
    type t =
      { prompt_tokens : float
      ; completion_tokens : float
      }
    [@@fields.no_zero_alloc] [@@deriving fields ~getters, sexp]
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
    [@@fields.no_zero_alloc] [@@deriving fields ~getters, sexp]
  end

  module Links : sig
    type t = { details : string option } [@@deriving fields ~getters, sexp]
  end

  module Benchmarks : sig
    module Artificial_analysis : sig
      type t =
        { intelligence_index : float option
        ; coding_index : float option
        ; agentic_index : float option
        }
      [@@fields.no_zero_alloc] [@@deriving fields ~getters, sexp]
    end

    module Design_arena : sig
      type t =
        { arena : string
        ; category : string
        ; elo : int
        ; rank : int
        ; win_rate : float
        }
      [@@fields.no_zero_alloc] [@@deriving fields ~getters, sexp]
    end

    type t =
      { artificial_analysis : Artificial_analysis.t option
      ; design_arena : Design_arena.t list
      }
    [@@deriving fields ~getters, sexp]
  end

  (** Reasoning capability of the model: whether reasoning is always on
      ([mandatory]), on by default, and which effort levels it accepts. *)
  module Reasoning : sig
    type t =
      { mandatory : bool
      ; default_enabled : bool option
      ; default_effort : string option
      ; supported_efforts : string list
      ; supports_max_tokens : bool option
      }
    [@@deriving fields ~getters, sexp]
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
    ; supported_voices : Jsonaf.t option
      (** Null on every model as of 2026-07; kept as raw JSON until the
          populated shape is observable. *)
    ; knowledge_cutoff : string option
    ; expiration_date : string option
    ; links : Links.t option
    ; benchmarks : Benchmarks.t option
    ; reasoning : Reasoning.t option
    }
  [@@deriving fields ~getters, sexp]
end

module Response : sig
  module Links : sig
    type t = { next : string option } [@@deriving fields ~getters, sexp]
  end

  type t =
    { data : Model_info.t list
    ; total_count : int option
    ; links : Links.t option
    }
  [@@deriving sexp]
end

val list
  :  api_key:string
  -> ?app_info:Http.App_info.t
  -> ?on_response_body:(string -> unit Deferred.t)
  -> unit
  -> Response.t Or_error.t Deferred.t

module For_testing : sig
  val response_of_jsonaf : Jsonaf.t -> Response.t
end

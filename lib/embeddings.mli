open! Core
open! Async

module Request : sig
  (** Input can be a single string or an array of strings to batch-embed. *)
  module Input : sig
    type t =
      | Single of string
      | Multi of string list
    [@@deriving jsonaf, sexp, variants]
  end

  type t =
    { model : string
    ; input : Input.t
    ; dimensions : int option
    }
  [@@deriving jsonaf, sexp]
end

module Response : sig
  module Embedding : sig
    type t =
      { object_ : string
      ; index : int
      ; embedding : float list
      }
    [@@deriving sexp]
  end

  module Usage : sig
    type t =
      { prompt_tokens : int
      ; total_tokens : int
      ; cost : float option
      }
    [@@deriving sexp]
  end

  type t =
    { object_ : string
    ; model : string
    ; data : Embedding.t list
    ; usage : Usage.t
    ; provider : string option
    ; id : string option
    }
  [@@deriving sexp]
end

val create
  :  api_key:string
  -> ?app_info:Http.App_info.t
  -> ?on_response_body:(string -> unit Deferred.t)
  -> Request.t
  -> Response.t Or_error.t Deferred.t

module For_testing : sig
  val response_of_jsonaf : Jsonaf.t -> Response.t
end

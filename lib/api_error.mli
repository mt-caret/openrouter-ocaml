(** API error response from OpenRouter. *)

open! Core

module Code : sig
  (** Error codes are usually integer HTTP status codes (400, 429, …) on the main
      endpoints, but mid-stream chunks and the Responses API surface string codes
      like ["server_error"] or ["rate_limit_exceeded"]. *)
  type t =
    | Int of int
    | String of string
  [@@deriving jsonaf, sexp]
end

module Details : sig
  type t =
    { message : string
    ; code : Code.t option
    ; metadata : Jsonaf.t option
    }
  [@@deriving of_jsonaf, sexp]
end

type t = { error : Details.t } [@@deriving of_jsonaf, sexp]

(** Try to parse error details from JSON, falling back to raw body if parsing fails. *)
val of_json_or_body : body_string:string -> Jsonaf.t -> string

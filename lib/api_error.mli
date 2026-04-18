(** API error response from OpenRouter. *)

open! Core

module Details : sig
  type t =
    { message : string
    ; code : int option
    }
  [@@deriving of_jsonaf, sexp_of]
end

type t = { error : Details.t } [@@deriving of_jsonaf, sexp_of]

(** Try to parse error details from JSON, falling back to raw body if parsing fails. *)
val of_json_or_body : body_string:string -> Jsonaf.t -> string

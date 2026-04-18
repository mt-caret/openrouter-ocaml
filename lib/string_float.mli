(** Float that parses from a JSON string (e.g., "0.00001" -> 0.00001) or number. *)

open! Core

type t = float [@@deriving jsonaf, sexp_of]

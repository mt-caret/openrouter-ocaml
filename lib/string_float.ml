open! Core

type t = float [@@deriving sexp_of]

let t_of_jsonaf json =
  match json with
  | `String s -> Float.of_string s
  | `Number s -> Float.of_string s
  | _ -> Jsonaf_kernel.Conv.of_jsonaf_error "Expected string or number for float" json
;;

let jsonaf_of_t f = `String (Float.to_string f)

open! Core
open Jsonaf.Export

module Code = struct
  type t =
    | Int of int
    | String of string
  [@@deriving sexp]

  let jsonaf_of_t = function
    | Int n -> `Number (Int.to_string n)
    | String s -> `String s
  ;;

  let t_of_jsonaf = function
    | `Number n ->
      (match Int.of_string_opt n with
       | Some i -> Int i
       | None -> String n)
    | `String s -> String s
    | json -> Jsonaf_kernel.Conv.of_jsonaf_error "Expected number or string" json
  ;;
end

module Details = struct
  type t =
    { message : string
    ; code : Code.t option [@default None]
    ; metadata : Jsonaf.t option [@default None]
    }
  [@@deriving of_jsonaf, sexp] [@@jsonaf.allow_extra_fields]
end

type t = { error : Details.t } [@@deriving of_jsonaf, sexp] [@@jsonaf.allow_extra_fields]

let of_json_or_body ~body_string json =
  match Or_error.try_with (fun () -> [%of_jsonaf: t] json) with
  | Ok t -> t.error.message
  | Error _ -> body_string
;;

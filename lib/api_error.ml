open! Core
open Jsonaf.Export

module Details = struct
  type t =
    { message : string
    ; code : int option [@default None]
    }
  [@@deriving of_jsonaf, sexp_of]
end

type t = { error : Details.t } [@@deriving of_jsonaf, sexp_of]

let of_json_or_body ~body_string json =
  match Or_error.try_with (fun () -> [%of_jsonaf: t] json) with
  | Ok t -> t.error.message
  | Error _ -> body_string
;;

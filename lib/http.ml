open! Core
open! Async

let is_success_status (response : Cohttp.Response.t) =
  let code = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
  Cohttp.Code.is_success code
;;

let make_headers ~api_key =
  Cohttp.Header.of_list
    [ "Authorization", "Bearer " ^ api_key; "Content-Type", "application/json" ]
;;

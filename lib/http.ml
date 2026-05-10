open! Core
open! Async

module App_info = struct
  type t =
    { http_referer : string option
    ; x_title : string option
    ; categories : string list
    ; cache : bool
    ; experimental_metadata : bool
    ; extra : (string * string) list
    }

  let none =
    { http_referer = None
    ; x_title = None
    ; categories = []
    ; cache = false
    ; experimental_metadata = false
    ; extra = []
    }
  ;;

  let create
        ?http_referer
        ?x_title
        ?(categories = [])
        ?(cache = false)
        ?(experimental_metadata = false)
        ?(extra = [])
        ()
    =
    { http_referer; x_title; categories; cache; experimental_metadata; extra }
  ;;

  let to_header_list
        { http_referer; x_title; categories; cache; experimental_metadata; extra }
    =
    let optional name = function
      | None -> []
      | Some value -> [ name, value ]
    in
    let bool_flag name = function
      | false -> []
      | true -> [ name, "true" ]
    in
    let categories =
      match categories with
      | [] -> []
      | xs -> [ "X-OpenRouter-Categories", String.concat ~sep:"," xs ]
    in
    List.concat
      [ optional "HTTP-Referer" http_referer
      ; optional "X-Title" x_title
      ; categories
      ; bool_flag "X-OpenRouter-Cache" cache
      ; bool_flag "X-OpenRouter-Experimental-Metadata" experimental_metadata
      ; extra
      ]
  ;;
end

let is_success_status (response : Cohttp.Response.t) =
  let code = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
  Cohttp.Code.is_success code
;;

let make_headers ~api_key ?(app_info = App_info.none) () =
  Cohttp.Header.of_list
    ([ "Authorization", "Bearer " ^ api_key; "Content-Type", "application/json" ]
     @ App_info.to_header_list app_info)
;;

open! Core
open! Async

(** App-level attribution and feature-flag headers. *)
module App_info : sig
  type t =
    { http_referer : string option
    ; x_title : string option
    ; categories : string list
    ; cache : bool
    ; experimental_metadata : bool
    ; extra : (string * string) list
    }

  (** All fields default to "off" / empty. *)
  val none : t

  val create
    :  ?http_referer:string
    -> ?x_title:string
    -> ?categories:string list
    -> ?cache:bool
    -> ?experimental_metadata:bool
    -> ?extra:(string * string) list
    -> unit
    -> t
end

(** Check if HTTP response indicates success (2xx status) *)
val is_success_status : Cohttp.Response.t -> bool

(** Build the standard OpenRouter headers (Authorization + Content-Type), plus any
    app-info headers in [app_info]. *)
val make_headers : api_key:string -> ?app_info:App_info.t -> unit -> Cohttp.Header.t

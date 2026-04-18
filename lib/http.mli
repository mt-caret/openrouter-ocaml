open! Core
open! Async

(** Check if HTTP response indicates success (2xx status) *)
val is_success_status : Cohttp.Response.t -> bool

(** Build the standard OpenRouter headers (Authorization + Content-Type). *)
val make_headers : api_key:string -> Cohttp.Header.t

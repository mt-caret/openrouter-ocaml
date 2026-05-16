open! Core
open! Async
open! Openrouter_api

module Logging : sig
  type t =
    { response_to : string option
    ; request_to : string option
    ; stream_to : string option
    }
end

module Request : sig
  type t

  module Resolved : sig
    type t =
      | Streaming of [ `Streaming ] Completions.Request.t
      | Non_streaming of [ `Non_streaming ] Completions.Request.t
  end

  val resolve : t -> Resolved.t Deferred.Or_error.t
end

type t =
  { request : Request.t
  ; app_info : Http.App_info.t
  ; logging : Logging.t
  }

val param : default_model:string -> t Or_error.t Command.Param.t

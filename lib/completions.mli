open! Core
open! Async

(** Chat completions and streaming chat completions. *)

module Reasoning_detail : sig
  type t =
    { format : string option
    ; index : int option
    ; type_ : string option
    ; text : string option
    ; signature : string option
    ; data : string option
    }
  [@@deriving sexp]
end

module Image : sig
  module Image_url : sig
    type t = { url : string } [@@deriving sexp]
  end

  type t =
    { type_ : string
    ; image_url : Image_url.t
    ; index : int option
    }
  [@@deriving sexp]

  module Elide_data : sig
    type nonrec t = t [@@deriving sexp]
  end
end

module Audio_output : sig
  type t =
    { id : string option
    ; data : string option
    ; expires_at : int option
    ; transcript : string option
    }
  [@@deriving sexp]
end

(** Tool definition for function calling and OpenRouter server-side tools.
    Server tools (web search, web fetch, datetime, image generation) are
    executed by OpenRouter on your behalf and their results fed back to the
    model — no local function dispatch required. *)
module Tool : sig
  module Function : sig
    type t =
      { name : string
      ; description : string option
      ; parameters : Jsonaf.t option
      ; strict : bool option
      }
    [@@deriving sexp]
  end

  module Web_search : sig
    type t =
      { engine : string option
      ; max_results : int option
      ; max_total_results : int option
      ; search_context_size : string option
      ; allowed_domains : string list option
      ; blocked_domains : string list option
      }
    [@@deriving sexp]
  end

  module Web_fetch : sig
    type t =
      { engine : string option
      ; max_uses : int option
      ; max_content_tokens : int option
      ; allowed_domains : string list option
      ; blocked_domains : string list option
      }
    [@@deriving sexp]
  end

  module Image_generation : sig
    type t =
      { model : string option
      ; prompt : string option
      ; parameters : (string * Jsonaf.t) list
      }
    [@@deriving sexp]
  end

  module Search_models : sig
    type t = { max_results : int option } [@@deriving sexp]
  end

  type t =
    | Function of Function.t
    | Web_search of Web_search.t
    | Web_fetch of Web_fetch.t
    | Datetime
    | Image_generation of Image_generation.t
    | Search_models of Search_models.t
  [@@deriving jsonaf, sexp]

  val function_
    :  name:string
    -> ?description:string
    -> ?parameters:Jsonaf.t
    -> ?strict:bool
    -> unit
    -> t

  val web_search
    :  ?engine:string
    -> ?max_results:int
    -> ?max_total_results:int
    -> ?search_context_size:string
    -> ?allowed_domains:string list
    -> ?blocked_domains:string list
    -> unit
    -> t

  val web_fetch
    :  ?engine:string
    -> ?max_uses:int
    -> ?max_content_tokens:int
    -> ?allowed_domains:string list
    -> ?blocked_domains:string list
    -> unit
    -> t

  val datetime : t

  val image_generation
    :  ?model:string
    -> ?prompt:string
    -> ?parameters:(string * Jsonaf.t) list
    -> unit
    -> t

  val search_models : ?max_results:int -> unit -> t
end

(** Tool choice configuration *)
module Tool_choice : sig
  module Function_choice : sig
    type t = { name : string } [@@deriving sexp]
  end

  module Specific : sig
    type t = private
      { type_ : string
      ; function_ : Function_choice.t
      }
    [@@deriving sexp]
  end

  type t =
    | Auto
    | None_
    | Required
    | Specific of Specific.t
  [@@deriving jsonaf, sexp]

  val auto : t
  val none : t
  val required : t
  val force_function : string -> t
  val arg_type : t Command.Arg_type.t
end

(** Tool call made by the model *)
module Tool_call : sig
  module Function_call : sig
    type t =
      { name : string
      ; arguments : string
      }
    [@@deriving sexp]
  end

  type t =
    { id : string
    ; type_ : string
    ; function_ : Function_call.t
    }
  [@@deriving sexp]
end

(** Plugin configuration for OpenRouter plugins. See
    https://openrouter.ai/docs/guides/features/plugins/overview *)
module Plugin : sig
  module Auto_router : sig
    type t =
      { enabled : bool option
      ; allowed_models : string list option
      }
    [@@deriving sexp]
  end

  module Web : sig
    type t =
      { enabled : bool option
      ; max_results : int option
      }
    [@@deriving sexp]
  end

  module Pdf_engine : sig
    type t =
      | Pdf_text
      | Mistral_ocr
      | Native
    [@@deriving sexp]

    val arg_type : t Command.Arg_type.t
  end

  module File_parser : sig
    module Pdf_config : sig
      type t = { engine : Pdf_engine.t } [@@deriving sexp]
    end

    type t = { pdf : Pdf_config.t option } [@@deriving sexp]
  end

  module Pareto_router : sig
    type t =
      { enabled : bool option
      ; min_coding_score : float option
      }
    [@@deriving sexp]
  end

  type t =
    | Auto_router of Auto_router.t
    | Moderation
    | Web of Web.t
    | File_parser of File_parser.t
    | Response_healing
    | Context_compression
    | Pareto_router of Pareto_router.t
  [@@deriving jsonaf, sexp]

  val auto_router : ?enabled:bool -> ?allowed_models:string list -> unit -> t
  val web : ?enabled:bool -> ?max_results:int -> unit -> t
  val file_parser : ?pdf_engine:Pdf_engine.t -> unit -> t
  val moderation : t
  val response_healing : t
  val context_compression : t
  val pareto_router : ?enabled:bool -> ?min_coding_score:float -> unit -> t
end

(** URL citation from web search results. *)
module Citation : sig
  type t =
    { url : string
    ; title : string option
    ; content : string option
    ; start_index : int option
    ; end_index : int option
    }
  [@@deriving sexp]

  (** Parse a citation from an annotation JSON object *)
  val of_annotation_jsonaf : Jsonaf.t -> t option
end

module Message : sig
  type t =
    { role : string
    ; content : string option
    ; audio : Audio_output.t option
    ; refusal : string option
    ; reasoning : string option
    ; reasoning_details : Reasoning_detail.t list
    ; images : Image.t list
    ; annotations : Jsonaf.t list
    ; tool_calls : Tool_call.t list
    ; tool_call_id : string option
    }
  [@@deriving sexp]

  module Elide_image : sig
    type nonrec t = t [@@deriving sexp]
  end
end

module Request : sig
  module Message : sig
    module Tool_call : sig
      module Function_call : sig
        type t =
          { name : string
          ; arguments : string
          }
        [@@deriving sexp]
      end

      type t =
        { id : string
        ; type_ : string
        ; function_ : Function_call.t
        }
      [@@deriving sexp]
    end

    module Content_part : sig
      (** Anthropic-style prompt-cache annotation. Attach to a content part
          and successive identical-prefix requests will hit the cache, yielding
          a non-zero [usage.prompt_tokens_details.cached_tokens]. *)
      module Cache_control : sig
        type t = private
          { type_ : string (** Always ["ephemeral"]. *)
          ; ttl : string option
          }
        [@@deriving sexp]

        val ephemeral : ?ttl:string -> unit -> t
      end

      module Text : sig
        type t =
          { text : string
          ; cache_control : Cache_control.t option
          }
        [@@deriving sexp]
      end

      module Image_url : sig
        module Url : sig
          type t = { url : string } [@@deriving sexp]
        end

        type t =
          { image_url : Url.t
          ; cache_control : Cache_control.t option
          }
        [@@deriving sexp]
      end

      module File : sig
        module Data : sig
          type t =
            { filename : string
            ; file_data : string
            }
          [@@deriving sexp]
        end

        type t =
          { file : Data.t
          ; cache_control : Cache_control.t option
          }
        [@@deriving sexp]
      end

      module Input_audio : sig
        module Data : sig
          type t =
            { data : string (** Base64-encoded audio bytes (no [data:] prefix). *)
            ; format : string (** e.g. "wav", "mp3", "aiff", "aac", "ogg", "flac". *)
            }
          [@@deriving sexp]
        end

        type t =
          { input_audio : Data.t
          ; cache_control : Cache_control.t option
          }
        [@@deriving sexp]
      end

      module Video_url : sig
        module Url : sig
          type t = { url : string } [@@deriving sexp]
        end

        type t =
          { video_url : Url.t
          ; cache_control : Cache_control.t option
          }
        [@@deriving sexp]
      end

      type t =
        | Text of Text.t
        | Image_url of Image_url.t
        | File of File.t
        | Input_audio of Input_audio.t
        | Video_url of Video_url.t
      [@@deriving sexp]

      val text : ?cache_control:Cache_control.t -> string -> t

      (** [mime_type] should be e.g. "image/jpeg", "image/png", "image/webp", "image/gif" *)
      val image_base64
        :  ?cache_control:Cache_control.t
        -> mime_type:string
        -> data:string
        -> unit
        -> t

      (** [file_data] can be either a URL (for publicly accessible files) or a data URL
          like "data:application/pdf;base64,..." for local files *)
      val file
        :  ?cache_control:Cache_control.t
        -> filename:string
        -> file_data:string
        -> unit
        -> t

      (** [data] is base64-encoded audio bytes (no [data:] prefix); [format] is
          e.g. "wav", "mp3". *)
      val audio
        :  ?cache_control:Cache_control.t
        -> format:string
        -> data:string
        -> unit
        -> t

      val video_url : ?cache_control:Cache_control.t -> url:string -> unit -> t

      val video_base64
        :  ?cache_control:Cache_control.t
        -> mime_type:string
        -> data:string
        -> unit
        -> t
    end

    module Content : sig
      type t =
        | Text of string
        | Multipart of Content_part.t list
      [@@deriving sexp]

      val text : string -> t
      val multipart : Content_part.t list -> t
    end

    type t = private
      { role : string
      ; content : Content.t option
      ; tool_calls : Tool_call.t list
      ; tool_call_id : string option
      }
    [@@deriving jsonaf_of, sexp]

    val user : string -> t
    val user_multipart : Content_part.t list -> t
    val system : string -> t
    val assistant : ?content:string -> ?tool_calls:Tool_call.t list -> unit -> t
    val tool : tool_call_id:string -> content:string -> t
  end

  module Reasoning : sig
    module Effort : sig
      type t =
        | Xhigh
        | High
        | Medium
        | Low
        | Minimal
        | None_
      [@@deriving jsonaf, sexp]

      (** [of_string] raises [Jsonaf_kernel.Conv.Of_jsonaf_error] on unknown values. *)
      include Stringable.S with type t := t

      val arg_type : t Command.Arg_type.t
    end

    (** Effort and max_tokens are mutually exclusive: providers accept one but not
        both. Pass [enabled = Some true] alone to enable reasoning with provider
        defaults. Pass [exclude = Some true] to keep reasoning hidden from the
        response while the model still uses it internally. *)
    type t = private
      { effort : Effort.t option
      ; max_tokens : int option
      ; exclude : bool option
      ; enabled : bool option
      ; summary : string option
      }
    [@@deriving jsonaf_of, sexp]

    (** Validates [effort] and [max_tokens] are not both set, matching the
        wire contract. Returns an [Or_error] so the failure surfaces locally
        instead of as a 400. *)
    val create
      :  ?effort:Effort.t
      -> ?max_tokens:int
      -> ?exclude:bool
      -> ?enabled:bool
      -> ?summary:string
      -> unit
      -> t Or_error.t
  end

  module Verbosity : sig
    type t =
      | Low
      | Medium
      | High
      | Xhigh
      | Max
    [@@deriving jsonaf, sexp]

    (** [of_string] raises [Jsonaf_kernel.Conv.Of_jsonaf_error] on unknown values. *)
    include Stringable.S with type t := t

    val arg_type : t Command.Arg_type.t
  end

  module Stream_options : sig
    type t = { include_usage : bool option } [@@deriving jsonaf, sexp]

    val create : ?include_usage:bool -> unit -> t
  end

  module Audio : sig
    type t =
      { voice : string
      ; format : string
      }
    [@@deriving jsonaf, sexp]

    val create : voice:string -> format:string -> t
  end

  module Debug : sig
    type t = { echo_upstream_body : bool option } [@@deriving jsonaf, sexp]

    val create : ?echo_upstream_body:bool -> unit -> t
  end

  module Image_config : sig
    type t = Jsonaf.t [@@deriving jsonaf, sexp]
  end

  module Metadata : sig
    type t = (string * string) list [@@deriving jsonaf, sexp]
  end

  module Trace : sig
    type t = (string * Jsonaf.t) list [@@deriving jsonaf, sexp]
  end

  module Cache_control : sig
    type t = private
      { type_ : string
      ; ttl : string option
      }
    [@@deriving sexp]

    val ephemeral : ?ttl:string -> unit -> t
  end

  module Provider : sig
    module Sort : sig
      type t =
        | Price
        | Throughput
        | Latency
      [@@deriving equal, sexp]

      include Stringable.S with type t := t
      include Jsonaf.Jsonafable.S with type t := t

      val arg_type : t Command.Arg_type.t
    end

    module Data_collection : sig
      type t =
        | Allow
        | Deny
      [@@deriving equal, sexp]

      include Stringable.S with type t := t
      include Jsonaf.Jsonafable.S with type t := t

      val arg_type : t Command.Arg_type.t
    end

    module Max_price : sig
      type t =
        { prompt : float option
        ; completion : float option
        ; request : float option
        ; image : float option
        ; audio : float option
        }
      [@@deriving equal, jsonaf, sexp]
    end

    type t =
      { order : string list option
      ; allow_fallbacks : bool option
      ; require_parameters : bool option
      ; data_collection : Data_collection.t option
      ; zdr : bool option
      ; only : string list option
      ; ignore : string list option
      ; quantizations : string list option
      ; sort : Sort.t option
      ; max_price : Max_price.t option
      ; preferred_min_throughput : float option
      ; preferred_max_latency : float option
      ; enforce_distillable_text : bool option
      }
    [@@deriving equal, jsonaf, sexp]

    (** All-[None] record — equivalent to omitting the [provider] field
        entirely, used as a base for callers that build the record one
        knob at a time. *)
    val empty : t

    (** [true] iff [t = empty]. Useful for deciding whether to attach the
        [provider] field at all. *)
    val is_empty : t -> bool
  end

  module Logit_bias : sig
    (** Map from token id (as a string, per the OpenAI/OpenRouter wire format) to
        bias in [-100, 100]. *)
    type t = (string * int) list [@@deriving jsonaf, sexp]
  end

  (** Response format for structured outputs. See
      https://openrouter.ai/docs/features/structured-outputs *)
  module Response_format : sig
    module Json_schema : sig
      type t =
        { name : string
        ; strict : bool option
        ; schema : Jsonaf.t option
        ; description : string option
        }
      [@@deriving jsonaf, sexp]
    end

    type t =
      | Json_object
      | Json_schema of Json_schema.t
    [@@deriving jsonaf, sexp]

    val json_schema
      :  ?strict:bool
      -> ?schema:Jsonaf.t
      -> ?description:string
      -> name:string
      -> unit
      -> t
  end

  (** Abstract request body, phantom-tagged by streaming kind. *)
  type 'tag t [@@deriving jsonaf_of, sexp]

  module Non_streaming : sig
    type nonrec t = [ `Non_streaming ] t
  end

  module Streaming : sig
    type nonrec t = [ `Streaming ] t
  end

  type 'tag create :=
    ?cache_control:Cache_control.t
    -> ?reasoning:Reasoning.t
    -> ?tools:Tool.t list
    -> ?tool_choice:Tool_choice.t
    -> ?parallel_tool_calls:bool
    -> ?plugins:Plugin.t list
    -> ?metadata:Metadata.t
    -> ?user:string
    -> ?session_id:string
    -> ?route:string
    -> ?trace:Trace.t
    -> ?temperature:float
    -> ?top_p:float
    -> ?top_k:int
    -> ?min_p:float
    -> ?top_a:float
    -> ?max_tokens:int
    -> ?max_completion_tokens:int
    -> ?seed:int
    -> ?stop:string list
    -> ?frequency_penalty:float
    -> ?presence_penalty:float
    -> ?repetition_penalty:float
    -> ?logit_bias:Logit_bias.t
    -> ?logprobs:bool
    -> ?top_logprobs:int
    -> ?verbosity:Verbosity.t
    -> ?response_format:Response_format.t
    -> ?structured_outputs:bool
    -> ?modalities:string list
    -> ?image_config:Image_config.t
    -> ?service_tier:string
    -> ?models:string list
    -> ?transforms:string list
    -> ?provider:Provider.t
    -> model:string
    -> messages:Message.t list
    -> unit
    -> 'tag t

  val create : [ `Non_streaming ] create

  val create_streaming
    :  ?debug:Debug.t
    -> ?audio:Audio.t
    -> ?stream_options:Stream_options.t
    -> [ `Streaming ] create
end

module Logprobs : sig
  module Top_logprob : sig
    type t =
      { token : string
      ; logprob : float
      ; bytes : int list option
      }
    [@@deriving sexp]
  end

  module Token : sig
    type t =
      { token : string
      ; logprob : float
      ; bytes : int list option
      ; top_logprobs : Top_logprob.t list
      }
    [@@deriving sexp]
  end

  type t =
    { content : Token.t list option
    ; refusal : Token.t list option
    }
  [@@deriving sexp]
end

module Response : sig
  module Usage : sig
    module Prompt_tokens_details : sig
      type t =
        { cached_tokens : int option
        ; cache_write_tokens : int option
        ; audio_tokens : int option
        ; video_tokens : int option
        }
      [@@deriving sexp]
    end

    module Cost_details : sig
      type t =
        { upstream_inference_cost : float option
        ; upstream_inference_prompt_cost : float option
        ; upstream_inference_completions_cost : float option
        }
      [@@deriving sexp]
    end

    module Completion_tokens_details : sig
      type t =
        { reasoning_tokens : int option
        ; image_tokens : int option
        ; audio_tokens : int option
        }
      [@@deriving sexp]
    end

    module Server_tool_use : sig
      type t = { web_search_requests : int option } [@@deriving sexp]
    end

    type t =
      { prompt_tokens : int
      ; completion_tokens : int
      ; total_tokens : int
      ; cost : float option
      ; is_byok : bool option
      ; prompt_tokens_details : Prompt_tokens_details.t option
      ; cost_details : Cost_details.t option
      ; completion_tokens_details : Completion_tokens_details.t option
      ; server_tool_use : Server_tool_use.t option
      }
    [@@deriving sexp, fields ~getters]
  end

  module Choice : sig
    type t =
      { logprobs : Logprobs.t option
      ; finish_reason : string
      ; native_finish_reason : string
      ; index : int
      ; message : Message.t
      }
    [@@deriving sexp]

    module Elide_image : sig
      type nonrec t = t [@@deriving sexp]
    end
  end

  type t =
    { id : string
    ; provider : string
    ; model : string
    ; object_ : string
    ; created : int
    ; choices : Choice.t list
    ; system_fingerprint : string option
    ; service_tier : string option
    ; usage : Usage.t
    }
  [@@deriving sexp]

  module Elide_image : sig
    type nonrec t = t [@@deriving sexp]
  end
end

module Stream_chunk : sig
  module Tool_call_chunk : sig
    module Function_call : sig
      type t =
        { name : string option
        ; arguments : string option
        }
      [@@deriving sexp]
    end

    type t =
      { index : int
      ; id : string option
      ; type_ : string option
      ; function_ : Function_call.t option
      }
    [@@deriving sexp]
  end

  module Delta : sig
    type t =
      { role : string option
      ; content : string option
      ; audio : Audio_output.t option
      ; refusal : string option
      ; reasoning : string option
      ; reasoning_details : Reasoning_detail.t list
      ; images : Image.t list
      ; annotations : Jsonaf.t list
      ; tool_calls : Tool_call_chunk.t list
      }
    [@@deriving sexp]

    module Elide_image : sig
      type nonrec t = t [@@deriving sexp]
    end
  end

  module Choice : sig
    type t =
      { logprobs : Logprobs.t option
      ; finish_reason : string option
      ; native_finish_reason : string option
      ; index : int
      ; delta : Delta.t
      ; error : Jsonaf.t option
        (** Populated when the upstream provider fails after generation has
            begun. Schema is provider-specific so we keep it as raw JSON. *)
      }
    [@@deriving sexp]

    val is_error : t -> bool
  end

  type t =
    { id : string
    ; provider : string
    ; model : string
    ; object_ : string
    ; created : int
    ; choices : Choice.t list
    ; system_fingerprint : string option
    ; service_tier : string option
    ; debug : Jsonaf.t option
      (** Populated on debug stream chunks when request debug options such as
          [echo_upstream_body] are enabled. Kept as raw JSON because the shape is
          diagnostic and may evolve independently from normal completion data. *)
    ; usage : Response.Usage.t option
    }
  [@@deriving sexp]
end

val create
  :  api_key:string
  -> ?app_info:Http.App_info.t
  -> ?on_response_body:(string -> unit Deferred.t)
       (** Invoked with the raw HTTP response body before parsing — useful for
           capturing fixtures or logging unexpected shapes. *)
  -> [ `Non_streaming ] Request.t
  -> Response.t Or_error.t Deferred.t

val create_stream
  :  api_key:string
  -> ?app_info:Http.App_info.t
  -> ?on_stream_chunk:(string -> unit Deferred.t)
       (** Invoked with each raw JSON payload (the [data: ...] portion of an SSE
           line, with the [data: ] prefix stripped) before parsing. Useful for
           capturing fixtures or logging unexpected shapes. *)
  -> [ `Streaming ] Request.t
  -> Stream_chunk.t Or_error.t Pipe.Reader.t Deferred.t

module For_testing : sig
  val response_of_jsonaf : Jsonaf.t -> Response.t
  val stream_chunk_of_jsonaf : Jsonaf.t -> Stream_chunk.t
end

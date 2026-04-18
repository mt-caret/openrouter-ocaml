open! Core
open! Async

module Reasoning_detail : sig
  type t =
    { format : string
    ; index : int
    ; type_ : string
    ; text : string option
    ; signature : string option
    ; data : string option
    }
  [@@deriving sexp_of]
end

module Image : sig
  module Image_url : sig
    type t = { url : string } [@@deriving sexp_of]
  end

  type t =
    { type_ : string
    ; image_url : Image_url.t
    ; index : int
    }
  [@@deriving sexp_of]

  module Elide_data : sig
    type nonrec t = t [@@deriving sexp_of]
  end
end

(** Tool definition for function calling *)
module Tool : sig
  module Function : sig
    type t =
      { name : string
      ; description : string option
      ; parameters : Jsonaf.t option
      }
    [@@deriving sexp_of]
  end

  type t =
    { type_ : string
    ; function_ : Function.t
    }
  [@@deriving jsonaf, sexp_of]

  val create : name:string -> ?description:string -> ?parameters:Jsonaf.t -> unit -> t
end

(** Tool choice configuration *)
module Tool_choice : sig
  module Function_choice : sig
    type t = { name : string } [@@deriving sexp_of]
  end

  module Specific : sig
    type t =
      { type_ : string
      ; function_ : Function_choice.t
      }
    [@@deriving sexp_of]
  end

  type t =
    | Auto
    | None_
    | Required
    | Specific of Specific.t
  [@@deriving jsonaf, sexp_of]

  val auto : t
  val none : t
  val required : t
  val force_function : string -> t
end

(** Tool call made by the model *)
module Tool_call : sig
  module Function_call : sig
    type t =
      { name : string
      ; arguments : string
      }
    [@@deriving sexp_of]
  end

  type t =
    { id : string
    ; type_ : string
    ; function_ : Function_call.t
    }
  [@@deriving sexp_of]
end

(** Plugin configuration for OpenRouter plugins. See
    https://openrouter.ai/docs/guides/features/plugins/overview *)
module Plugin : sig
  module Web : sig
    type t =
      { id : string
      ; enabled : bool option
      ; max_results : int option
      }
    [@@deriving sexp_of]

    val default : t
  end

  module Pdf_engine : sig
    type t =
      | Pdf_text
      | Mistral_ocr
      | Native
    [@@deriving sexp_of]
  end

  module File_parser : sig
    module Pdf_config : sig
      type t = { engine : Pdf_engine.t } [@@deriving sexp_of]
    end

    type t =
      { id : string
      ; pdf : Pdf_config.t option
      }
    [@@deriving sexp_of]

    val default : t
  end

  type t =
    | Web of Web.t
    | File_parser of File_parser.t
  [@@deriving jsonaf, sexp_of]

  val web : ?enabled:bool -> ?max_results:int -> unit -> t
  val file_parser : ?pdf_engine:Pdf_engine.t -> unit -> t
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
  [@@deriving sexp_of]

  (** Parse a citation from an annotation JSON object *)
  val of_annotation_jsonaf : Jsonaf.t -> t option
end

module Message : sig
  type t =
    { role : string
    ; content : string option
    ; refusal : string option
    ; reasoning : string option
    ; reasoning_details : Reasoning_detail.t list
    ; images : Image.t list
    ; annotations : Jsonaf.t list
    ; tool_calls : Tool_call.t list
    ; tool_call_id : string option
    }
  [@@deriving sexp_of]

  module Elide_image : sig
    type nonrec t = t [@@deriving sexp_of]
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
        [@@deriving sexp_of]
      end

      type t =
        { id : string
        ; type_ : string
        ; function_ : Function_call.t
        }
      [@@deriving sexp_of]
    end

    module Content_part : sig
      module Image_url : sig
        type t = { url : string } [@@deriving sexp_of]
      end

      module File_data : sig
        type t =
          { filename : string
          ; file_data : string
          }
        [@@deriving sexp_of]
      end

      type t =
        | Text of { text : string }
        | Image_url of { image_url : Image_url.t }
        | File of { file : File_data.t }
      [@@deriving sexp_of]

      val text : string -> t

      (** [mime_type] should be e.g. "image/jpeg", "image/png", "image/webp", "image/gif" *)
      val image_base64 : mime_type:string -> data:string -> t

      (** [file_data] can be either a URL (for publicly accessible files) or a data URL
          like "data:application/pdf;base64,..." for local files *)
      val file : filename:string -> file_data:string -> t
    end

    module Content : sig
      type t =
        | Text of string
        | Multipart of Content_part.t list
      [@@deriving sexp_of]

      val text : string -> t
      val multipart : Content_part.t list -> t
    end

    type t =
      { role : string
      ; content : Content.t option
      ; tool_calls : Tool_call.t list
      ; tool_call_id : string option
      }
    [@@deriving jsonaf, sexp_of]

    val user : string -> t
    val user_multipart : Content_part.t list -> t
    val system : string -> t
    val assistant : ?content:string -> ?tool_calls:Tool_call.t list -> unit -> t
    val tool : tool_call_id:string -> content:string -> t
  end

  module Reasoning : sig
    type t = { max_tokens : int } [@@deriving jsonaf, sexp_of]
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
      [@@deriving jsonaf, sexp_of]
    end

    type t =
      | Json_object
      | Json_schema of Json_schema.t
    [@@deriving jsonaf, sexp_of]

    val json_schema
      :  ?strict:bool
      -> ?schema:Jsonaf.t
      -> ?description:string
      -> name:string
      -> unit
      -> t
  end

  type t =
    { model : string
    ; messages : Message.t list
    ; stream : bool
    ; reasoning : Reasoning.t option
    ; tools : Tool.t list
    ; tool_choice : Tool_choice.t option
    ; parallel_tool_calls : bool option
    ; plugins : Plugin.t list
    ; temperature : float option
    ; top_p : float option
    ; max_tokens : int option
    ; seed : int option
    ; stop : string list option
    ; frequency_penalty : float option
    ; presence_penalty : float option
    ; repetition_penalty : float option
    ; response_format : Response_format.t option
    }
  [@@deriving jsonaf, sexp_of]
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
      [@@deriving sexp_of]
    end

    module Cost_details : sig
      type t =
        { upstream_inference_cost : float option
        ; upstream_inference_prompt_cost : float option
        ; upstream_inference_completions_cost : float option
        }
      [@@deriving sexp_of]
    end

    module Completion_tokens_details : sig
      type t =
        { reasoning_tokens : int option
        ; image_tokens : int option
        ; audio_tokens : int option
        }
      [@@deriving sexp_of]
    end

    module Server_tool_use : sig
      type t = { web_search_requests : int option } [@@deriving sexp_of]
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
    [@@deriving sexp_of, fields ~getters]
  end

  module Choice : sig
    type t =
      { logprobs : unit option
      ; finish_reason : string
      ; native_finish_reason : string
      ; index : int
      ; message : Message.t
      }
    [@@deriving sexp_of]

    module Elide_image : sig
      type nonrec t = t [@@deriving sexp_of]
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
  [@@deriving sexp_of]

  module Elide_image : sig
    type nonrec t = t [@@deriving sexp_of]
  end
end

module Stream_chunk : sig
  module Tool_call_chunk : sig
    module Function_call : sig
      type t =
        { name : string option
        ; arguments : string option
        }
      [@@deriving sexp_of]
    end

    type t =
      { index : int
      ; id : string option
      ; type_ : string option
      ; function_ : Function_call.t option
      }
    [@@deriving sexp_of]
  end

  module Delta : sig
    type t =
      { role : string option
      ; content : string option
      ; refusal : string option
      ; reasoning : string option
      ; reasoning_details : Reasoning_detail.t list
      ; images : Image.t list
      ; annotations : Jsonaf.t list
      ; tool_calls : Tool_call_chunk.t list
      }
    [@@deriving sexp_of]

    module Elide_image : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end

  module Choice : sig
    type t =
      { logprobs : unit option
      ; finish_reason : string option
      ; native_finish_reason : string option
      ; index : int
      ; delta : Delta.t
      }
    [@@deriving sexp_of]
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
    ; usage : Response.Usage.t option
    }
  [@@deriving sexp_of]
end

val create : api_key:string -> Request.t -> Response.t Or_error.t Deferred.t

val create_stream
  :  api_key:string
  -> Request.t
  -> Stream_chunk.t Or_error.t Pipe.Reader.t Deferred.t

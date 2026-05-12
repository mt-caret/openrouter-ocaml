open! Core
open! Async
open Jsonaf.Export

let endpoint_url = Uri.of_string "https://openrouter.ai/api/v1/chat/completions"

(* Reasoning details from models with extended thinking.
   Different providers use different formats:
   - Claude: has `text` and `signature` fields
   - Google Gemini: has `text` or `data` (for encrypted reasoning) fields
   In streaming mode, intermediate chunks have `text` but no `signature`.
   The final chunk has `signature` but may have empty `text`. *)
module Reasoning_detail = struct
  (* Anthropic streams reasoning blocks with [format] and [type] populated, but
     non-streaming responses sometimes return [null] for [type], so both stay
     optional. [index] is also absent in non-streaming responses. *)
  type t =
    { format : string option [@default None] [@jsonaf_drop_default.equal]
    ; index : int option [@default None] [@jsonaf_drop_default.equal]
    ; type_ : string option [@key "type"] [@default None] [@jsonaf_drop_default.equal]
    ; text : string option [@default None] [@jsonaf_drop_default.equal]
    ; signature : string option [@default None] [@jsonaf_drop_default.equal]
    ; data : string option [@default None] [@jsonaf_drop_default.equal]
      (* Google's encrypted reasoning *)
    }
  [@@deriving equal, jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
end

(* Image in a message response. [index] is absent on Gemini's image-output
   responses, so it stays optional. *)
module Image = struct
  module Image_url = struct
    type t = { url : string }
    [@@deriving equal, jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
  end

  type t =
    { type_ : string [@key "type"]
    ; image_url : Image_url.t
    ; index : int option [@default None] [@jsonaf_drop_default.equal]
    }
  [@@deriving equal, jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]

  module Elide_data = struct
    type nonrec t = t =
      { type_ : string
      ; image_url : (Image_url.t[@sexp.opaque])
      ; index : int option
      }
    [@@deriving sexp_of]
  end
end

(* Tool calling types per OpenRouter API spec:
   https://openrouter.ai/docs/guides/features/tool-calling *)

module Tool = struct
  (* JSON value with equality (Jsonaf.t has exactly_equal but not equal). *)
  module Json_schema = struct
    type t = Jsonaf.t [@@deriving jsonaf, sexp_of]

    let equal = Jsonaf.exactly_equal
  end

  module Function = struct
    type t =
      { name : string
      ; description : string option [@jsonaf.option]
      ; parameters : Json_schema.t option [@jsonaf.option]
      }
    [@@deriving equal, jsonaf, sexp_of]
  end

  module Web_search = struct
    type t =
      { engine : string option [@jsonaf.option]
      ; max_total_results : int option [@jsonaf.option]
      ; allowed_domains : string list option [@jsonaf.option]
      ; blocked_domains : string list option [@jsonaf.option]
      }
    [@@deriving equal, jsonaf, sexp_of]
  end

  module Web_fetch = struct
    type t =
      { engine : string option [@jsonaf.option]
      ; max_uses : int option [@jsonaf.option]
      ; max_content_tokens : int option [@jsonaf.option]
      ; allowed_domains : string list option [@jsonaf.option]
      ; blocked_domains : string list option [@jsonaf.option]
      }
    [@@deriving equal, jsonaf, sexp_of]
  end

  module Image_generation = struct
    type t = { prompt : string option [@jsonaf.option] }
    [@@deriving equal, jsonaf, sexp_of]
  end

  module T = struct
    type t =
      | Function of Function.t
      | Web_search of Web_search.t
      | Web_fetch of Web_fetch.t
      | Datetime
      | Image_generation of Image_generation.t
    [@@deriving equal, sexp_of, typed_variants]

    let discriminator = "type"

    (* The "function" tool is the OpenAI standard; everything else is an
       OpenRouter server-tool, namespaced under [openrouter:]. *)
    let tag =
      `Custom
        (fun { Typed_variant.Packed.f = T v } ->
          match v with
          | Function -> "function"
          | Web_search | Web_fetch | Datetime | Image_generation ->
            "openrouter:" ^ Typed_variant.name v)
    ;;

    let codec : type a. a Typed_variant.t -> a Json_helper.Tagged_union_codec.t = function
      | Function ->
        Json_helper.nested ~key:"function" Function.jsonaf_of_t Function.t_of_jsonaf
      | Web_search -> Inline (Web_search.jsonaf_of_t, Web_search.t_of_jsonaf)
      | Web_fetch -> Inline (Web_fetch.jsonaf_of_t, Web_fetch.t_of_jsonaf)
      | Datetime -> Tag_only
      | Image_generation ->
        Inline (Image_generation.jsonaf_of_t, Image_generation.t_of_jsonaf)
    ;;
  end

  include T
  include Json_helper.Make_tagged_union (T)

  let function_ ~name ?description ?parameters () =
    Function { name; description; parameters }
  ;;

  let web_search ?engine ?max_total_results ?allowed_domains ?blocked_domains () =
    Web_search { engine; max_total_results; allowed_domains; blocked_domains }
  ;;

  let web_fetch ?engine ?max_uses ?max_content_tokens ?allowed_domains ?blocked_domains ()
    =
    Web_fetch { engine; max_uses; max_content_tokens; allowed_domains; blocked_domains }
  ;;

  let datetime = Datetime
  let image_generation ?prompt () = Image_generation { prompt }
end

module Tool_choice = struct
  module Function_choice = struct
    type t = { name : string } [@@deriving jsonaf, sexp_of]
  end

  module Specific = struct
    type t =
      { type_ : string [@key "type"]
      ; function_ : Function_choice.t [@key "function"]
      }
    [@@deriving jsonaf, sexp_of]
  end

  type t =
    | Auto
    | None_
    | Required
    | Specific of Specific.t
  [@@deriving sexp_of]

  let t_of_jsonaf json =
    match json with
    | `String "auto" -> Auto
    | `String "none" -> None_
    | `String "required" -> Required
    | `Object _ -> Specific (Specific.t_of_jsonaf json)
    | _ -> Jsonaf_kernel.Conv.of_jsonaf_error "Invalid tool_choice" json
  ;;

  let jsonaf_of_t = function
    | Auto -> `String "auto"
    | None_ -> `String "none"
    | Required -> `String "required"
    | Specific s -> Specific.jsonaf_of_t s
  ;;

  let auto = Auto
  let none = None_
  let required = Required
  let force_function name = Specific { type_ = "function"; function_ = { name } }
end

module Tool_call = struct
  module Function_call = struct
    type t =
      { name : string
      ; arguments : string (* JSON-encoded string *)
      }
    [@@deriving equal, jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
  end

  type t =
    { id : string
    ; type_ : string [@key "type"]
    ; function_ : Function_call.t [@key "function"]
    }
  [@@deriving equal, jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
end

module Plugin = struct
  module Web = struct
    type t =
      { enabled : bool option [@jsonaf.option]
      ; max_results : int option [@jsonaf.option]
      }
    [@@deriving equal, jsonaf, sexp_of]
  end

  module Pdf_engine = struct
    module T = struct
      type t =
        | Pdf_text
        | Mistral_ocr
        | Native
      [@@deriving equal, sexp_of, enumerate]
    end

    include T
    include Json_helper.Make_string_variant (T)
  end

  module File_parser = struct
    module Pdf_config = struct
      type t = { engine : Pdf_engine.t } [@@deriving equal, jsonaf, sexp_of]
    end

    type t = { pdf : Pdf_config.t option [@jsonaf.option] }
    [@@deriving equal, jsonaf, sexp_of]
  end

  module T = struct
    type t =
      | Web of Web.t
      | File_parser of File_parser.t
      | Response_healing
      | Context_compression
    [@@deriving equal, sexp_of, typed_variants]

    let discriminator = "id"
    let tag = `Kebab_case

    let codec : type a. a Typed_variant.t -> a Json_helper.Tagged_union_codec.t = function
      | Web -> Inline (Web.jsonaf_of_t, Web.t_of_jsonaf)
      | File_parser -> Inline (File_parser.jsonaf_of_t, File_parser.t_of_jsonaf)
      | Response_healing -> Tag_only
      | Context_compression -> Tag_only
    ;;
  end

  include T
  include Json_helper.Make_tagged_union (T)

  let web ?enabled ?max_results () = Web { enabled; max_results }

  let file_parser ?pdf_engine () =
    let pdf =
      Option.map pdf_engine ~f:(fun engine -> { File_parser.Pdf_config.engine })
    in
    File_parser { pdf }
  ;;

  let response_healing = Response_healing
  let context_compression = Context_compression
end

module Citation = struct
  type t =
    { url : string
    ; title : string option [@jsonaf.option]
    ; content : string option [@jsonaf.option]
    ; start_index : int option [@jsonaf.option]
    ; end_index : int option [@jsonaf.option]
    }
  [@@deriving equal, of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]

  let of_annotation_jsonaf json =
    match Jsonaf.member "type" json with
    | Some (`String "url_citation") ->
      (match Jsonaf.member "url_citation" json with
       | Some citation_json -> Some (t_of_jsonaf citation_json)
       | None -> None)
    | _ -> None
  ;;
end

module Message = struct
  type t =
    { role : string
    ; content : string option
          [@default None] (* content can be null when assistant makes tool calls *)
    ; refusal : string option [@default None]
    ; reasoning : string option [@default None]
    ; reasoning_details : Reasoning_detail.t list
          [@default []] [@jsonaf_drop_default.equal]
    ; images : Image.t list [@default []] [@jsonaf_drop_default.equal]
    ; annotations : Jsonaf.t list [@default []]
    ; tool_calls : Tool_call.t list
          [@default []] [@jsonaf_drop_default.equal] (* Tool calls made by assistant *)
    ; tool_call_id : string option [@default None] [@jsonaf_drop_default.equal]
      (* For tool role messages: ID of the tool call being responded to *)
    }
  [@@deriving jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]

  module Elide_image = struct
    type nonrec t = t =
      { role : string
      ; content : string option
      ; refusal : string option
      ; reasoning : string option
      ; reasoning_details : Reasoning_detail.t list
      ; images : (Image.Elide_data.t list[@sexp.list])
      ; annotations : Jsonaf.t list
      ; tool_calls : Tool_call.t list
      ; tool_call_id : string option
      }
    [@@deriving sexp_of]
  end
end

module Request = struct
  module Message = struct
    module Tool_call = struct
      module Function_call = struct
        type t =
          { name : string
          ; arguments : string
          }
        [@@deriving equal, jsonaf, sexp_of]
      end

      type t =
        { id : string
        ; type_ : string [@key "type"]
        ; function_ : Function_call.t [@key "function"]
        }
      [@@deriving equal, jsonaf, sexp_of]
    end

    module Content_part = struct
      module Cache_control = struct
        type t =
          { type_ : string [@key "type"]
          ; ttl : string option [@jsonaf.option]
          }
        [@@deriving equal, jsonaf, sexp_of]

        let ephemeral ?ttl () = { type_ = "ephemeral"; ttl }
      end

      module Text = struct
        type t =
          { text : string
          ; cache_control : Cache_control.t option [@jsonaf.option]
          }
        [@@deriving equal, jsonaf, sexp_of]
      end

      module Image_url = struct
        module Url = struct
          type t = { url : string } [@@deriving equal, jsonaf, sexp_of]
        end

        type t =
          { image_url : Url.t
          ; cache_control : Cache_control.t option [@jsonaf.option]
          }
        [@@deriving equal, jsonaf, sexp_of]
      end

      module File = struct
        module Data = struct
          type t =
            { filename : string
            ; file_data : string
            }
          [@@deriving equal, jsonaf, sexp_of]
        end

        type t =
          { file : Data.t
          ; cache_control : Cache_control.t option [@jsonaf.option]
          }
        [@@deriving equal, jsonaf, sexp_of]
      end

      module Input_audio = struct
        module Data = struct
          type t =
            { data : string (** Base64-encoded audio bytes (no [data:] prefix). *)
            ; format : string (** e.g. "wav", "mp3", "aiff", "aac", "ogg", "flac". *)
            }
          [@@deriving equal, jsonaf, sexp_of]
        end

        type t =
          { input_audio : Data.t
          ; cache_control : Cache_control.t option [@jsonaf.option]
          }
        [@@deriving equal, jsonaf, sexp_of]
      end

      module Video_url = struct
        module Url = struct
          type t = { url : string } [@@deriving equal, jsonaf, sexp_of]
        end

        type t =
          { video_url : Url.t
          ; cache_control : Cache_control.t option [@jsonaf.option]
          }
        [@@deriving equal, jsonaf, sexp_of]
      end

      module T = struct
        type t =
          | Text of Text.t
          | Image_url of Image_url.t
          | File of File.t
          | Input_audio of Input_audio.t
          | Video_url of Video_url.t
        [@@deriving equal, sexp_of, typed_variants]

        let discriminator = "type"
        let tag = `Infer

        let codec : type a. a Typed_variant.t -> a Json_helper.Tagged_union_codec.t =
          function
          | Text -> Inline (Text.jsonaf_of_t, Text.t_of_jsonaf)
          | Image_url -> Inline (Image_url.jsonaf_of_t, Image_url.t_of_jsonaf)
          | File -> Inline (File.jsonaf_of_t, File.t_of_jsonaf)
          | Input_audio -> Inline (Input_audio.jsonaf_of_t, Input_audio.t_of_jsonaf)
          | Video_url -> Inline (Video_url.jsonaf_of_t, Video_url.t_of_jsonaf)
        ;;
      end

      include T
      include Json_helper.Make_tagged_union (T)

      let text ?cache_control s = Text { text = s; cache_control }

      let image_base64 ?cache_control ~mime_type ~data () =
        let url = sprintf "data:%s;base64,%s" mime_type data in
        Image_url { image_url = { url }; cache_control }
      ;;

      let file ?cache_control ~filename ~file_data () =
        File { file = { filename; file_data }; cache_control }
      ;;

      let audio ?cache_control ~format ~data () =
        Input_audio { input_audio = { data; format }; cache_control }
      ;;

      let video_url ?cache_control ~url () =
        Video_url { video_url = { url }; cache_control }
      ;;

      let video_base64 ?cache_control ~mime_type ~data () =
        let url = sprintf "data:%s;base64,%s" mime_type data in
        Video_url { video_url = { url }; cache_control }
      ;;
    end

    module Content = struct
      type t =
        | Text of string
        | Multipart of Content_part.t list
      [@@deriving equal, sexp_of]

      let text s = Text s
      let multipart parts = Multipart parts

      let jsonaf_of_t = function
        | Text s -> `String s
        | Multipart parts -> `Array (List.map parts ~f:Content_part.jsonaf_of_t)
      ;;

      let t_of_jsonaf = function
        | `String s -> Text s
        | `Array arr -> Multipart (List.map arr ~f:Content_part.t_of_jsonaf)
        | json -> Jsonaf_kernel.Conv.of_jsonaf_error "Expected string or array" json
      ;;
    end

    type t =
      { role : string
      ; content : Content.t option
            [@default None]
            [@jsonaf_drop_default.equal]
            (* Content can be null for assistant messages with tool_calls *)
      ; tool_calls : Tool_call.t list
            [@default []]
            [@jsonaf_drop_default.equal]
            (* Tool calls for assistant role (when replaying conversation) *)
      ; tool_call_id : string option [@default None] [@jsonaf_drop_default.equal]
        (* For tool role: ID of the tool call being responded to *)
      }
    [@@deriving jsonaf, sexp_of]

    let user content =
      { role = "user"
      ; content = Some (Content.Text content)
      ; tool_calls = []
      ; tool_call_id = None
      }
    ;;

    let user_multipart parts =
      { role = "user"
      ; content = Some (Content.Multipart parts)
      ; tool_calls = []
      ; tool_call_id = None
      }
    ;;

    let system content =
      { role = "system"
      ; content = Some (Content.Text content)
      ; tool_calls = []
      ; tool_call_id = None
      }
    ;;

    let assistant ?content ?(tool_calls = []) () =
      { role = "assistant"
      ; content = Option.map content ~f:(fun c -> Content.Text c)
      ; tool_calls
      ; tool_call_id = None
      }
    ;;

    let tool ~tool_call_id ~content =
      { role = "tool"
      ; content = Some (Content.Text content)
      ; tool_calls = []
      ; tool_call_id = Some tool_call_id
      }
    ;;
  end

  module Reasoning = struct
    module Effort = struct
      module T = struct
        type t =
          | Xhigh
          | High
          | Medium
          | Low
          | Minimal
          | None_
        [@@deriving sexp_of, enumerate]
      end

      include T
      include Json_helper.Make_string_variant (T)
    end

    type t =
      { effort : Effort.t option [@jsonaf.option]
      ; max_tokens : int option [@jsonaf.option]
      ; exclude : bool option [@jsonaf.option]
      ; enabled : bool option [@jsonaf.option]
      }
    [@@deriving jsonaf, sexp_of]

    (* OpenRouter's reasoning API spec: "[effort] and [max_tokens] are
       mutually exclusive". Enforce that here so the failure surfaces locally
       instead of as a 400 from the wire. *)
    let create ?effort ?max_tokens ?exclude ?enabled () =
      match effort, max_tokens with
      | Some _, Some _ ->
        Or_error.error_string "Reasoning: effort and max_tokens are mutually exclusive"
      | _ -> Ok { effort; max_tokens; exclude; enabled }
    ;;
  end

  module Verbosity = struct
    module T = struct
      type t =
        | Low
        | Medium
        | High
        | Xhigh
        | Max
      [@@deriving sexp_of, enumerate]
    end

    include T
    include Json_helper.Make_string_variant (T)
  end

  module Stream_options = struct
    type t = { include_usage : bool option [@jsonaf.option] } [@@deriving jsonaf, sexp_of]
  end

  module Provider = struct
    module Sort = struct
      module T = struct
        type t =
          | Price
          | Throughput
          | Latency
        [@@deriving equal, sexp_of, enumerate]
      end

      include T
      include Json_helper.Make_string_variant (T)
    end

    module Data_collection = struct
      module T = struct
        type t =
          | Allow
          | Deny
        [@@deriving equal, sexp_of, enumerate]
      end

      include T
      include Json_helper.Make_string_variant (T)
    end

    module Max_price = struct
      type t =
        { prompt : float option [@jsonaf.option]
        ; completion : float option [@jsonaf.option]
        ; request : float option [@jsonaf.option]
        ; image : float option [@jsonaf.option]
        }
      [@@deriving equal, jsonaf, sexp_of]
    end

    type t =
      { order : string list option [@jsonaf.option]
      ; allow_fallbacks : bool option [@jsonaf.option]
      ; require_parameters : bool option [@jsonaf.option]
      ; data_collection : Data_collection.t option [@jsonaf.option]
      ; zdr : bool option [@jsonaf.option]
      ; only : string list option [@jsonaf.option]
      ; ignore : string list option [@jsonaf.option]
      ; quantizations : string list option [@jsonaf.option]
      ; sort : Sort.t option [@jsonaf.option]
      ; max_price : Max_price.t option [@jsonaf.option]
      ; preferred_min_throughput : float option [@jsonaf.option]
      ; preferred_max_latency : float option [@jsonaf.option]
      }
    [@@deriving equal, jsonaf, sexp_of]

    let empty =
      { order = None
      ; allow_fallbacks = None
      ; require_parameters = None
      ; data_collection = None
      ; zdr = None
      ; only = None
      ; ignore = None
      ; quantizations = None
      ; sort = None
      ; max_price = None
      ; preferred_min_throughput = None
      ; preferred_max_latency = None
      }
    ;;

    let is_empty t = equal t empty
  end

  module Logit_bias = struct
    type t = (string * int) list [@@deriving sexp_of]

    let jsonaf_of_t t =
      `Object (List.map t ~f:(fun (k, v) -> k, `Number (Int.to_string v)))
    ;;

    let t_of_jsonaf = function
      | `Object kvs ->
        List.map kvs ~f:(fun (k, v) ->
          match v with
          | `Number n ->
            (match Int.of_string_opt n with
             | Some i -> k, i
             | None -> Jsonaf_kernel.Conv.of_jsonaf_error "Expected integer bias value" v)
          | _ -> Jsonaf_kernel.Conv.of_jsonaf_error "Expected number for logit bias" v)
      | json -> Jsonaf_kernel.Conv.of_jsonaf_error "Expected object for logit_bias" json
    ;;
  end

  module Response_format = struct
    module Json_schema = struct
      type t =
        { name : string
        ; strict : bool option [@jsonaf.option]
        ; schema : Jsonaf.t option [@jsonaf.option]
        ; description : string option [@jsonaf.option]
        }
      [@@deriving jsonaf, sexp_of]
    end

    module T = struct
      type t =
        | Json_object
        | Json_schema of Json_schema.t
      [@@deriving sexp_of, typed_variants]

      let discriminator = "type"
      let tag = `Infer

      let codec : type a. a Typed_variant.t -> a Json_helper.Tagged_union_codec.t =
        function
        | Json_object -> Tag_only
        | Json_schema ->
          Json_helper.nested
            ~key:"json_schema"
            Json_schema.jsonaf_of_t
            Json_schema.t_of_jsonaf
      ;;
    end

    include T
    include Json_helper.Make_tagged_union (T)

    let json_schema ?strict ?schema ?description ~name () =
      Json_schema { Json_schema.name; strict; schema; description }
    ;;
  end

  (* The tag is a purely type-level discriminator that prevents passing a
     streaming request to the non-streaming entry point (and vice versa);
     it doesn't appear in the wire format. ppx_jsonaf_conv threads a dummy
     converter through [jsonaf_of_t]/[sexp_of_t], so call sites that want
     to serialize go through the [%jsonaf_of: [`Non_streaming] Request.t]
     extension form. *)
  type 'tag t =
    { model : string
    ; messages : Message.t list
    ; stream : bool
    ; reasoning : Reasoning.t option [@jsonaf.option]
    ; tools : Tool.t list [@default []] [@jsonaf_drop_default.equal]
    ; tool_choice : Tool_choice.t option [@jsonaf.option]
    ; parallel_tool_calls : bool option [@jsonaf.option]
    ; plugins : Plugin.t list [@default []] [@jsonaf_drop_default.equal]
    ; temperature : float option [@jsonaf.option]
    ; top_p : float option [@jsonaf.option]
    ; top_k : int option [@jsonaf.option]
    ; min_p : float option [@jsonaf.option]
    ; top_a : float option [@jsonaf.option]
    ; max_tokens : int option [@jsonaf.option]
    ; max_completion_tokens : int option [@jsonaf.option]
    ; seed : int option [@jsonaf.option]
    ; stop : string list option [@jsonaf.option]
    ; frequency_penalty : float option [@jsonaf.option]
    ; presence_penalty : float option [@jsonaf.option]
    ; repetition_penalty : float option [@jsonaf.option]
    ; logit_bias : Logit_bias.t option [@jsonaf.option]
    ; logprobs : bool option [@jsonaf.option]
    ; top_logprobs : int option [@jsonaf.option]
    ; verbosity : Verbosity.t option [@jsonaf.option]
    ; response_format : Response_format.t option [@jsonaf.option]
    ; structured_outputs : bool option [@jsonaf.option]
    ; modalities : string list option [@jsonaf.option]
    ; stream_options : Stream_options.t option [@jsonaf.option]
    ; service_tier : string option [@jsonaf.option]
    ; models : string list [@default []] [@jsonaf_drop_default.equal]
    ; transforms : string list [@default []] [@jsonaf_drop_default.equal]
    ; provider : Provider.t option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp_of]

  let create_body
        ~stream
        ?reasoning
        ?(tools = [])
        ?tool_choice
        ?parallel_tool_calls
        ?(plugins = [])
        ?temperature
        ?top_p
        ?top_k
        ?min_p
        ?top_a
        ?max_tokens
        ?max_completion_tokens
        ?seed
        ?stop
        ?frequency_penalty
        ?presence_penalty
        ?repetition_penalty
        ?logit_bias
        ?logprobs
        ?top_logprobs
        ?verbosity
        ?response_format
        ?structured_outputs
        ?modalities
        ?stream_options
        ?service_tier
        ?(models = [])
        ?(transforms = [])
        ?provider
        ~model
        ~messages
        ()
    : 'tag t
    =
    { model
    ; messages
    ; stream
    ; reasoning
    ; tools
    ; tool_choice
    ; parallel_tool_calls
    ; plugins
    ; temperature
    ; top_p
    ; top_k
    ; min_p
    ; top_a
    ; max_tokens
    ; max_completion_tokens
    ; seed
    ; stop
    ; frequency_penalty
    ; presence_penalty
    ; repetition_penalty
    ; logit_bias
    ; logprobs
    ; top_logprobs
    ; verbosity
    ; response_format
    ; structured_outputs
    ; modalities
    ; stream_options
    ; service_tier
    ; models
    ; transforms
    ; provider
    }
  ;;

  let create
        ?reasoning
        ?tools
        ?tool_choice
        ?parallel_tool_calls
        ?plugins
        ?temperature
        ?top_p
        ?top_k
        ?min_p
        ?top_a
        ?max_tokens
        ?max_completion_tokens
        ?seed
        ?stop
        ?frequency_penalty
        ?presence_penalty
        ?repetition_penalty
        ?logit_bias
        ?logprobs
        ?top_logprobs
        ?verbosity
        ?response_format
        ?structured_outputs
        ?modalities
        ?stream_options
        ?service_tier
        ?models
        ?transforms
        ?provider
        ~model
        ~messages
        ()
    : [ `Non_streaming ] t
    =
    create_body
      ~stream:false
      ?reasoning
      ?tools
      ?tool_choice
      ?parallel_tool_calls
      ?plugins
      ?temperature
      ?top_p
      ?top_k
      ?min_p
      ?top_a
      ?max_tokens
      ?max_completion_tokens
      ?seed
      ?stop
      ?frequency_penalty
      ?presence_penalty
      ?repetition_penalty
      ?logit_bias
      ?logprobs
      ?top_logprobs
      ?verbosity
      ?response_format
      ?structured_outputs
      ?modalities
      ?stream_options
      ?service_tier
      ?models
      ?transforms
      ?provider
      ~model
      ~messages
      ()
  ;;

  let create_streaming
        ?reasoning
        ?tools
        ?tool_choice
        ?parallel_tool_calls
        ?plugins
        ?temperature
        ?top_p
        ?top_k
        ?min_p
        ?top_a
        ?max_tokens
        ?max_completion_tokens
        ?seed
        ?stop
        ?frequency_penalty
        ?presence_penalty
        ?repetition_penalty
        ?logit_bias
        ?logprobs
        ?top_logprobs
        ?verbosity
        ?response_format
        ?structured_outputs
        ?modalities
        ?stream_options
        ?service_tier
        ?models
        ?transforms
        ?provider
        ~model
        ~messages
        ()
    : [ `Streaming ] t
    =
    create_body
      ~stream:true
      ?reasoning
      ?tools
      ?tool_choice
      ?parallel_tool_calls
      ?plugins
      ?temperature
      ?top_p
      ?top_k
      ?min_p
      ?top_a
      ?max_tokens
      ?max_completion_tokens
      ?seed
      ?stop
      ?frequency_penalty
      ?presence_penalty
      ?repetition_penalty
      ?logit_bias
      ?logprobs
      ?top_logprobs
      ?verbosity
      ?response_format
      ?structured_outputs
      ?modalities
      ?stream_options
      ?service_tier
      ?models
      ?transforms
      ?provider
      ~model
      ~messages
      ()
  ;;
end

(* Per-token logprobs returned in [choices[].logprobs] when [logprobs = true] is
   requested. OpenAI shape: a [content] array (and optionally [refusal] array) of
   per-token records, each with up to [top_logprobs] alternative tokens. *)
module Logprobs = struct
  module Top_logprob = struct
    type t =
      { token : string
      ; logprob : float
      ; bytes : int list option [@default None]
      }
    [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
  end

  module Token = struct
    type t =
      { token : string
      ; logprob : float
      ; bytes : int list option [@default None]
      ; top_logprobs : Top_logprob.t list [@default []]
      }
    [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
  end

  type t =
    { content : Token.t list option [@default None]
    ; refusal : Token.t list option [@default None]
    }
  [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
end

module Response = struct
  module Usage = struct
    module Prompt_tokens_details = struct
      type t =
        { cached_tokens : int option [@default None]
        ; cache_write_tokens : int option [@default None]
        ; audio_tokens : int option [@default None]
        ; video_tokens : int option [@default None]
        }
      [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
    end

    module Cost_details = struct
      type t =
        { upstream_inference_cost : float option [@default None]
        ; upstream_inference_prompt_cost : float option [@default None]
        ; upstream_inference_completions_cost : float option [@default None]
        }
      [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
    end

    module Completion_tokens_details = struct
      type t =
        { reasoning_tokens : int option [@default None]
        ; image_tokens : int option [@default None]
        ; audio_tokens : int option [@default None]
        }
      [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
    end

    module Server_tool_use = struct
      type t = { web_search_requests : int option [@default None] }
      [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
    end

    type t =
      { prompt_tokens : int
      ; completion_tokens : int
      ; total_tokens : int
      ; cost : float option [@default None]
      ; is_byok : bool option [@default None]
      ; prompt_tokens_details : Prompt_tokens_details.t option [@default None]
      ; cost_details : Cost_details.t option [@default None]
      ; completion_tokens_details : Completion_tokens_details.t option [@default None]
      ; server_tool_use : Server_tool_use.t option [@default None]
      }
    [@@deriving of_jsonaf, sexp_of, fields ~getters] [@@jsonaf.allow_extra_fields]
  end

  module Choice = struct
    type t =
      { logprobs : Logprobs.t option [@default None]
      ; finish_reason : string
      ; native_finish_reason : string
      ; index : int
      ; message : Message.t
      }
    [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]

    module Elide_image = struct
      type nonrec t = t =
        { logprobs : Logprobs.t option
        ; finish_reason : string
        ; native_finish_reason : string
        ; index : int
        ; message : Message.Elide_image.t
        }
      [@@deriving sexp_of]
    end
  end

  type t =
    { id : string
    ; provider : string
    ; model : string
    ; object_ : string [@key "object"]
    ; created : int
    ; choices : Choice.t list
    ; system_fingerprint : string option [@default None]
    ; service_tier : string option [@default None]
    ; usage : Usage.t
    }
  [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]

  module Elide_image = struct
    type nonrec t = t =
      { id : string
      ; provider : string
      ; model : string
      ; object_ : string
      ; created : int
      ; choices : (Choice.Elide_image.t list[@sexp.list])
      ; system_fingerprint : string option
      ; service_tier : string option
      ; usage : Usage.t
      }
    [@@deriving sexp_of]
  end
end

module Stream_chunk = struct
  module Tool_call_chunk = struct
    module Function_call = struct
      type t =
        { name : string option [@default None] [@jsonaf_drop_default.equal]
        ; arguments : string option [@default None] [@jsonaf_drop_default.equal]
        }
      [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
    end

    type t =
      { index : int
      ; id : string option [@default None] [@jsonaf_drop_default.equal]
      ; type_ : string option [@default None] [@jsonaf_drop_default.equal] [@key "type"]
      ; function_ : Function_call.t option
            [@default None] [@jsonaf_drop_default.equal] [@key "function"]
      }
    [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
  end

  module Delta = struct
    type t =
      { role : string option [@default None] [@jsonaf_drop_default.equal]
      ; content : string option [@default None] [@jsonaf_drop_default.equal]
      ; refusal : string option [@default None] [@jsonaf_drop_default.equal]
      ; reasoning : string option [@default None] [@jsonaf_drop_default.equal]
      ; reasoning_details : Reasoning_detail.t list
            [@default []] [@jsonaf_drop_default.equal]
      ; images : Image.t list [@default []] [@jsonaf_drop_default.equal]
      ; annotations :
          (* Annotations from some providers (e.g., Google) - currently just stored as JSON *)
          Jsonaf.t list
            [@default []] [@jsonaf_drop_default.equal]
      ; tool_calls : Tool_call_chunk.t list [@default []] [@jsonaf_drop_default.equal]
      }
    [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]

    module Elide_image = struct
      type nonrec t = t =
        { role : string option
        ; content : string option
        ; refusal : string option
        ; reasoning : string option
        ; reasoning_details : Reasoning_detail.t list
        ; images : (Image.Elide_data.t list[@sexp.list])
        ; annotations : Jsonaf.t list
        ; tool_calls : Tool_call_chunk.t list
        }
      [@@deriving sexp_of]
    end
  end

  module Choice = struct
    type t =
      { logprobs : Logprobs.t option [@default None]
      ; finish_reason : string option
      ; native_finish_reason : string option
      ; index : int
      ; delta : Delta.t
      ; error : Jsonaf.t option [@default None]
        (* When the upstream provider fails after generation has begun, the
           final chunk may carry [finish_reason = "error"] and an [error]
           object. We keep this as raw JSON since OpenRouter doesn't document
           a stable schema for it. *)
      }
    [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]

    (** [true] when this chunk represents a mid-stream upstream failure (i.e.
        [finish_reason = "error"]). *)
    let is_error t = [%equal: string option] t.finish_reason (Some "error")
  end

  type t =
    { id : string
    ; provider : string
    ; model : string
    ; object_ : string [@key "object"]
    ; created : int
    ; choices : Choice.t list
    ; system_fingerprint : string option [@default None]
    ; service_tier : string option [@default None]
    ; usage : Response.Usage.t option [@jsonaf.option]
    }
  [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
end

let create ~api_key ?app_info ?on_response_body (request : [ `Non_streaming ] Request.t) =
  let headers = Http.make_headers ~api_key ?app_info () in
  let body =
    [%jsonaf_of: [ `Non_streaming ] Request.t] request
    |> Jsonaf.to_string
    |> Cohttp_async.Body.of_string
  in
  let%bind response, body = Cohttp_async.Client.post ~headers ~body endpoint_url in
  let%bind body_string = Cohttp_async.Body.to_string body in
  let%map () =
    match on_response_body with
    | None -> return ()
    | Some f -> f body_string
  in
  let%bind.Or_error () =
    match Http.is_success_status response with
    | true -> Ok ()
    | false ->
      let error_message =
        match Jsonaf.parse body_string with
        | Ok json -> Api_error.of_json_or_body ~body_string json
        | Error _ -> body_string
      in
      let status = Cohttp.Response.status response in
      Or_error.error_s
        [%message
          "OpenRouter API error"
            (status : Cohttp.Code.status_code)
            (error_message : string)]
  in
  let%bind.Or_error json =
    Jsonaf.parse body_string
    |> Or_error.tag_s_lazy
         ~tag:
           (lazy
             [%message
               "Failed to parse response body into JSON"
                 (response : Cohttp.Response.t)
                 (body_string : string)])
  in
  Or_error.try_with (fun () -> [%of_jsonaf: Response.t] json)
  |> Or_error.tag_s_lazy
       ~tag:
         (lazy
           [%message
             "Failed to parse JSON into Response.t"
               (response : Cohttp.Response.t)
               (json : Jsonaf.t)])
;;

let lines_of_chunks (chunks : string Pipe.Reader.t) : string Pipe.Reader.t =
  Pipe.create_reader ~close_on_exception:true (fun writer ->
    let buffer = ref "" in
    Pipe.iter chunks ~f:(fun chunk ->
      buffer := !buffer ^ chunk;
      match String.split_on_chars !buffer ~on:[ '\n' ] with
      | [] -> Deferred.unit
      | lines ->
        buffer := List.last_exn lines;
        List.drop_last_exn lines
        |> Deferred.List.iter ~how:`Sequential ~f:(fun line ->
          match String.strip line with
          | "" -> Deferred.unit
          | line -> Pipe.write writer line)))
;;

let create_stream ~api_key ?app_info ?on_stream_chunk (request : [ `Streaming ] Request.t)
  =
  let headers = Http.make_headers ~api_key ?app_info () in
  let body =
    [%jsonaf_of: [ `Streaming ] Request.t] request
    |> Jsonaf.to_string
    |> Cohttp_async.Body.of_string
  in
  let%bind response, body = Cohttp_async.Client.post ~headers ~body endpoint_url in
  match Http.is_success_status response with
  | false ->
    let%map body_string = Cohttp_async.Body.to_string body in
    let error_message =
      match Jsonaf.parse body_string with
      | Ok json -> Api_error.of_json_or_body ~body_string json
      | Error _ -> body_string
    in
    let status = Cohttp.Response.status response in
    let error =
      Error.create_s
        [%message
          "OpenRouter API error"
            (status : Cohttp.Code.status_code)
            (error_message : string)]
    in
    Pipe.of_list [ Error error ]
  | true ->
    return
      (Pipe.create_reader ~close_on_exception:true (fun writer ->
         Cohttp_async.Body.to_pipe body
         |> lines_of_chunks
         |> Pipe.iter ~f:(fun line ->
           let parse_result =
             match
               String.lsplit2 line ~on:':'
               |> Option.map ~f:(Tuple2.map_snd ~f:String.strip)
             with
             | Some ("", comment) ->
               (* SSE lines starting with : are comments (e.g., ": OPENROUTER PROCESSING") *)
               `Comment comment
             | Some ("data", "[DONE]") -> `Done
             | Some ("data", content) -> `Data content
             | None | Some (_, _) -> `Unknown line
           in
           [%log.global.debug
             "SSE line"
               (parse_result
                : [ `Comment of string | `Done | `Data of string | `Unknown of string ])];
           match parse_result with
           | `Comment _ | `Done | `Unknown _ -> Deferred.unit
           | `Data data ->
             let%bind () =
               match on_stream_chunk with
               | None -> Deferred.unit
               | Some f -> f data
             in
             let result =
               let%bind.Or_error json =
                 Jsonaf.parse data
                 |> Or_error.tag_s_lazy
                      ~tag:
                        (lazy
                          [%message
                            "Failed to parse SSE data as JSON"
                              (response : Cohttp.Response.t)
                              (data : string)])
               in
               let%map.Or_error stream_chunk =
                 Or_error.try_with (fun () -> [%of_jsonaf: Stream_chunk.t] json)
                 |> Or_error.tag_s_lazy
                      ~tag:
                        (lazy
                          [%message
                            "Failed to parse Stream_chunk"
                              (response : Cohttp.Response.t)
                              (json : Jsonaf.t)])
               in
               [%log.global.debug "Stream chunk" (stream_chunk : Stream_chunk.t)];
               stream_chunk
             in
             Pipe.write writer result)))
;;

module For_testing = struct
  let response_of_jsonaf = [%of_jsonaf: Response.t]
  let stream_chunk_of_jsonaf = [%of_jsonaf: Stream_chunk.t]
end

let%expect_test "Reasoning.create rejects effort + max_tokens together" =
  Request.Reasoning.create ~effort:Low ~max_tokens:200 ()
  |> [%sexp_of: Request.Reasoning.t Or_error.t]
  |> print_s;
  [%expect {| (Error "Reasoning: effort and max_tokens are mutually exclusive") |}];
  Deferred.unit
;;

let%expect_test "Provider.is_empty: empty <-> non-empty" =
  let open Request.Provider in
  print_s [%sexp (is_empty empty : bool)];
  [%expect {| true |}];
  print_s [%sexp (is_empty { empty with sort = Some Sort.Price } : bool)];
  [%expect {| false |}];
  Deferred.unit
;;

let%expect_test "citation parsing from annotation" =
  let annotation_json =
    Jsonaf.parse
      {|{
        "type": "url_citation",
        "url_citation": {
          "url": "https://example.com/article",
          "title": "Example Article",
          "content": "Some snippet from the article...",
          "start_index": 10,
          "end_index": 50
        }
      }|}
    |> Or_error.ok_exn
  in
  (match Citation.of_annotation_jsonaf annotation_json with
   | Some citation -> print_s [%sexp (citation : Citation.t)]
   | None -> print_endline "No citation found");
  [%expect
    {|
    ((url https://example.com/article) (title ("Example Article"))
     (content ("Some snippet from the article...")) (start_index (10))
     (end_index (50)))
    |}];
  Deferred.unit
;;

open! Core

(** Build [Stringable.S] and [Jsonaf.Jsonafable.S] for a nullary variant whose
    JSON wire format is a bare string. Constructor names are converted by:

    - lowercasing,
    - replacing [_] with [-], and
    - stripping any trailing [-].

    So [Pdf_text] → ["pdf-text"], [Mistral_ocr] → ["mistral-ocr"],
    [None_] → ["none"], [Xhigh] → ["xhigh"]. The constructor name is
    recovered from [sexp_of_t], so only nullary constructors are supported —
    a non-atom sexp is treated as a programmer error.

    The functor result intentionally does not introduce its own [type t] —
    it only adds operations on [T.t] via destructive substitution — so the
    standard [module T = struct ... end include T include Make (T)] pattern
    keeps [t] concrete to the outside world without [T] needing to leak
    into the [.mli]. *)
module Make_string_variant (T : sig
    type t [@@deriving sexp, enumerate]
  end) : sig
  include Stringable.S with type t := T.t
  include Jsonaf.Jsonafable.S with type t := T.t

  val arg_type : T.t Command.Arg_type.t
end

(** Derive jsonaf converters for JSON discriminated unions: objects with a
    single tag field that selects the payload shape.

    {[
      { "type": "datetime" }                                       (* tag-only *)
      { "type": "openrouter:web_search", "max_total_results": 5 }  (* inlined  *)
      { "type": "function", "function": { ... } }                  (* nested   *)
    ]} *)

(** Per-variant payload layout. "Nested" (payload under a single sub-key) is
    expressed via [nested], which lifts an encoder/decoder pair into an
    [Inline] codec whose payload is a one-field object.

    The functor adds the discriminator field on serialization and strips it
    on deserialization, so encoder/decoder pairs only see the payload —
    never the tag field on the wire.

    TODO: [Inline] would read better carrying
    [(module Jsonaf.Jsonafable.S with type t = 'a)] so call sites become
    [Inline (module Function)]. Packing FCMs under a GADT existential with
    a path-imported module type used to hit a type-checker bug (compile
    time from <1s to >2min); the underlying fix landed in OCaml as commit
    4f386455 and should ship with OCaml 5.5 — switch over then. *)
module Tagged_union_codec : sig
  type 'a t =
    | Tag_only : unit t (** No payload — wire form is [{discriminator: tag}]. *)
    | Inline : ('a -> Jsonaf.t) * (Jsonaf.t -> 'a) -> 'a t
    (** Payload is a JSON object whose fields sit alongside the discriminator. *)
end

(** Wrap an encoder/decoder pair so that on-the-wire form is a single-field
    object [{key: <inner JSON>}]. Combined with [Inline], this expresses the
    "tag + payload-under-a-sub-key" encoding. *)
val nested : key:string -> ('a -> Jsonaf.t) -> (Jsonaf.t -> 'a) -> 'a Tagged_union_codec.t

(** Derive [jsonaf_of_t] / [t_of_jsonaf] for a variant type that has
    [\[@@deriving typed_variants\]]. The wire tag for each variant comes
    from [tag] (defaulting to [Typed_variant.name]); the per-variant
    [codec] only describes the payload layout. *)
module Make_tagged_union (V : sig
    type t

    module Typed_variant : Typed_variants_lib.S with type derived_on := t

    val discriminator : string

    (** [`Infer] uses [Typed_variant.name v] (snake_case OCaml name) as the
        wire tag. [`Kebab_case] swaps underscores for hyphens. [`Custom f]
        runs [f] on each variant — useful when the wire format diverges from
        the OCaml constructor name (namespace prefix, exceptions, etc.). *)
    val tag : [ `Infer | `Kebab_case | `Custom of Typed_variant.Packed.t -> string ]

    val codec : 'a Typed_variant.t -> 'a Tagged_union_codec.t
  end) : Jsonaf.Jsonafable.S with type t := V.t

(** Helpers for the common shape of OpenAI-compatible JSON discriminated
    unions: an object with a single tag field (typically named ["type"], but
    sometimes ["id"]) whose string value selects how the rest of the fields
    are interpreted. The variants we encounter encode their non-tag payload
    in three patterns:

    - tag-only: [{tag: "datetime"}] (no extra fields).
    - inlined: the payload is itself an object whose fields are spread next
      to the tag — [{tag: "openrouter:web_search", max_total_results: 5}].
    - nested: the payload sits under a single sub-key — [{tag: "function",
      function: {...}}].

    [to_jsonaf] supports all three via the [?inline] and [?extra_fields]
    parameters; [of_jsonaf] just centralises the dispatch + error reporting. *)

open! Core

(** Build [{discriminator: tag, ...inline, ...extra_fields}]. Pass [?inline]
    when the variant's payload is itself an object that should be spread
    into the parent (e.g. server-tool config records). Pass [?extra_fields]
    for explicit additional key/value pairs (e.g. nested sub-keys).

    Raises if [inline] is not [`Object _]. *)
val to_jsonaf
  :  discriminator:string
  -> tag:string
  -> ?inline:Jsonaf.t
  -> ?extra_fields:(string * Jsonaf.t) list
  -> unit
  -> Jsonaf.t

(** Extract the [discriminator] field as a string and dispatch to
    [handle_tag]. Reports [Of_jsonaf_error] when the discriminator field is
    missing or not a string; [handle_tag] is responsible for reporting an
    error when the tag itself is unknown. *)
val of_jsonaf
  :  discriminator:string
  -> handle_tag:(string -> Jsonaf.t -> 'a)
  -> Jsonaf.t
  -> 'a

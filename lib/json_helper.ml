open! Core

module Make_string_variant (T : sig
    type t [@@deriving sexp, enumerate]
  end) =
struct
  module T = struct
    type t = T.t

    let to_string t =
      [%sexp_of: T.t] t
      |> Sexp.to_string
      |> String.lowercase
      |> String.tr ~target:'_' ~replacement:'-'
      |> String.rstrip ~drop:(Char.equal '-')
    ;;

    let of_string =
      let table = String.Map.of_list_with_key_exn T.all ~get_key:to_string in
      fun s ->
        match Map.find table s with
        | Some t -> t
        | None -> Jsonaf_kernel.Conv.of_jsonaf_error "unknown string variant" (`String s)
    ;;
  end

  let arg_type = Command.Arg_type.create T.of_string

  include T
  include Jsonaf.Jsonafable.Of_stringable (T)
end

module Tagged_union_codec = struct
  type 'a t =
    | Tag_only : unit t
    | Inline : ('a -> Jsonaf.t) * (Jsonaf.t -> 'a) -> 'a t
end

let nested ~key (encode : 'a -> Jsonaf.t) (decode : Jsonaf.t -> 'a)
  : 'a Tagged_union_codec.t
  =
  let encode' v = `Object [ key, encode v ] in
  let decode' json =
    match Jsonaf.member key json with
    | Some j -> decode j
    | None ->
      Jsonaf_kernel.Conv.of_jsonaf_error
        [%string "Tagged_union: missing %{key} field"]
        json
  in
  Inline (encode', decode')
;;

module Make_tagged_union (V : sig
    type t

    module Typed_variant : Typed_variants_lib.S with type derived_on := t

    val discriminator : string
    val tag : [ `Infer | `Kebab_case | `Custom of Typed_variant.Packed.t -> string ]
    val codec : 'a Typed_variant.t -> 'a Tagged_union_codec.t
  end) : Jsonaf.Jsonafable.S with type t := V.t = struct
  let tag_of_variant variant =
    match V.tag with
    | `Infer -> V.Typed_variant.name variant
    | `Kebab_case ->
      V.Typed_variant.name variant |> String.tr ~target:'_' ~replacement:'-'
    | `Custom f -> f (V.Typed_variant.Packed.pack variant)
  ;;

  let variant_by_tag =
    String.Map.of_list_with_key_exn
      V.Typed_variant.Packed.all
      ~get_key:(fun { f = T variant } -> tag_of_variant variant)
  ;;

  let jsonaf_of_t (t : V.t) : Jsonaf.t =
    let { f = T variant } = V.Typed_variant.which t in
    let discriminator_field = V.discriminator, `String (tag_of_variant variant) in
    match V.codec variant with
    | Tag_only -> `Object [ discriminator_field ]
    | Inline (encode, _) ->
      (* TODO: it's a bit sad that we need to [Option.value_exn]; ideally
         ppx_typed_variants would generate a safe [get] function. *)
      let payload = Option.value_exn (V.Typed_variant.get variant t) in
      (match encode payload with
       | `Object kvs -> `Object (discriminator_field :: kvs)
       | other ->
         raise_s
           [%message
             "Tagged_union: payload encoder must return a JSON object" (other : Jsonaf.t)])
  ;;

  let t_of_jsonaf json : V.t =
    let discriminator_kvs, payload_kvs =
      match json with
      | `Object kvs ->
        List.partition_tf kvs ~f:(fun (k, _) -> String.equal k V.discriminator)
      | _ -> Jsonaf_kernel.Conv.of_jsonaf_error "Tagged_union: expected JSON object" json
    in
    let tag =
      match discriminator_kvs with
      | [ (_, `String s) ] -> s
      | [ (_, non_string) ] ->
        Jsonaf_kernel.Conv.of_jsonaf_error
          [%string "Tagged_union: expected string for %{V.discriminator}"]
          non_string
      | [] ->
        Jsonaf_kernel.Conv.of_jsonaf_error
          [%string "Tagged_union: missing %{V.discriminator} field"]
          json
      | _ -> Jsonaf_kernel.Conv.of_jsonaf_error "Multiple discriminator fields found" json
    in
    match Map.find variant_by_tag tag with
    | None ->
      Jsonaf_kernel.Conv.of_jsonaf_error
        [%string "Tagged_union: unknown %{V.discriminator} %{tag}"]
        json
    | Some { f = T variant } ->
      (match V.codec variant with
       | Tag_only -> V.Typed_variant.create variant ()
       | Inline (_, decode) ->
         V.Typed_variant.create variant (decode (`Object payload_kvs)))
  ;;
end

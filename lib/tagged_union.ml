open! Core

let to_jsonaf ~discriminator ~tag ?inline ?(extra_fields = []) () =
  let inline_fields =
    match inline with
    | None -> []
    | Some (`Object kvs) -> kvs
    | Some other ->
      raise_s
        [%message
          "Tagged_union.to_jsonaf: [inline] must be a JSON object" (other : Jsonaf.t)]
  in
  `Object (((discriminator, `String tag) :: inline_fields) @ extra_fields)
;;

let of_jsonaf ~discriminator ~handle_tag json =
  match Jsonaf.member discriminator json with
  | None ->
    Jsonaf_kernel.Conv.of_jsonaf_error
      [%string "Tagged_union: missing %{discriminator} field"]
      json
  | Some (`String tag) -> handle_tag tag json
  | Some non_string ->
    Jsonaf_kernel.Conv.of_jsonaf_error
      [%string "Tagged_union: expected string for %{discriminator} field"]
      non_string
;;

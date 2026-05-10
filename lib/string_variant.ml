open! Core

let wire_name_of_constructor name =
  name
  |> String.lowercase
  |> String.tr ~target:'_' ~replacement:'-'
  |> String.rstrip ~drop:(Char.equal '-')
;;

module Make (T : sig
    type t [@@deriving sexp_of, enumerate]
  end) =
struct
  let to_string t =
    match [%sexp_of: T.t] t with
    | Atom name -> wire_name_of_constructor name
    | List _ as sexp ->
      raise_s [%message "expected nullary variant constructor" (sexp : Sexp.t)]
  ;;

  let of_string =
    let table =
      lazy (List.map T.all ~f:(fun t -> to_string t, t) |> String.Map.of_alist_exn)
    in
    fun s ->
      match Map.find (force table) s with
      | Some t -> t
      | None -> Jsonaf_kernel.Conv.of_jsonaf_error "unknown string variant" (`String s)
  ;;

  include Jsonaf.Jsonafable.Of_stringable (struct
      type nonrec t = T.t

      let to_string = to_string
      let of_string = of_string
    end)
end

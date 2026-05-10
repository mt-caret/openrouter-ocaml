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

open! Core

module Make (T : sig
    type t [@@deriving sexp_of, enumerate]
  end) : sig
  include Stringable.S with type t := T.t
  include Jsonaf.Jsonafable.S with type t := T.t
end


(** The common signature that all error modules must (at least) satisfy. *)

type expected =
  | Attr of string
  | Tag of string
  | Data
  | Root

exception Expected of expected * expected
exception Expected_Leaf

val string_of_expectation : expected * expected -> string
val raise_expectation : expected -> expected -> 'a

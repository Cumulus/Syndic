
(** The common signature that all error modules must (at least) satisfy. *)

type expected =
  | Attr of string
  | Tag of string
  | Data
  | Root

type error = [
  | `Expected_data of string
  | `Expected_node of string
  | `Expected_attr of string
]

exception Expected of expected * expected
exception Expected_Data
exception Error of Xmlm.pos * error

val string_of_expectation : expected * expected -> string
val raise_expectation : expected -> expected -> 'a

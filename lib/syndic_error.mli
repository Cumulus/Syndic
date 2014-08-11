
(** The common signature that all error modules must (at least) satisfy. *)

type error = [
  | `Expected of string
]

exception Error of Xmlm.pos * error

val to_string : exn -> string

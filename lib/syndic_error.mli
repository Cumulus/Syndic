(** The common signature that all error modules must (at least) satisfy. *)

type t = Xmlm.pos * string

exception Error of t

val to_string : exn -> string


(** The common signature that all error modules must (at least) satisfy. *)

exception Error of Xmlm.pos * string

val to_string : exn -> string

(** A XML tree. *)
type t =
  | Node of Xmlm.tag * t list
  | Leaf of string

val of_xmlm : Xmlm.input -> (string option * t)

val to_xmlm : t -> Xmlm.output -> unit

val to_string : t -> string

val to_buffer : t -> Buffer.t -> unit

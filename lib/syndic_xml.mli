
(** A XML tree. *)
type t =
  | Node of Xmlm.tag * t list
  | Leaf of string

val of_xmlm : Xmlm.input -> t

val of_xmlm_with_dtd : Xmlm.input -> Xmlm.dtd * t


val to_xmlm : t -> Xmlm.output -> unit

val to_string : t -> string

val to_buffer : t -> Buffer.t -> unit

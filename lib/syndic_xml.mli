(** Common module for XML parsing. *)

type dtd = string option
(** The type for the optional
    {{:http://www.w3.org/TR/REC-xml/#dt-doctype}DTD}. *)

(** A XML tree. *)
type t =
  | Node of Xmlm.pos * Xmlm.tag * t list
  | Data of Xmlm.pos * string

val resolve : xmlbase: Uri.t option -> Uri.t -> Uri.t
(** [resolve base uri] resolve the [uri] against the possible base. *)

val get_position : t -> Xmlm.pos

val of_xmlm : Xmlm.input -> (dtd * t)
(** [of_xmlm doc] converts an XML document [doc] into a DTD and a
    tree representing the document. *)

val to_xmlm : ?dtd:string -> t -> Xmlm.output -> unit

val to_string : ?ns_prefix:(string -> string option) -> t -> string

val to_buffer : ?ns_prefix:(string -> string option) -> t -> Buffer.t -> unit

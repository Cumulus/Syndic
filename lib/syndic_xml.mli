(** Common module for XML parsing. *)

(** The type for the optional {{:http://www.w3.org/TR/REC-xml/#dt-doctype}DTD}. *)
type dtd = string option

type pos = Xmlm.pos
type tag = Xmlm.tag

(** A XML tree. *)
type t = Node of pos * tag * t list | Data of pos * string

val resolve : xmlbase:Uri.t option -> Uri.t -> Uri.t
(** [resolve base uri] resolve the [uri] against the possible base. *)

val get_position : t -> pos
val input_of_channel : in_channel -> Xmlm.input

val of_xmlm : Xmlm.input -> dtd * t
(** [of_xmlm doc] converts an XML document [doc] into a DTD and a tree
    representing the document. *)

val make_output :
  ?ns_prefix:(string -> string option) -> Xmlm.dest -> Xmlm.output

val to_xmlm : ?dtd:string -> t -> Xmlm.output -> unit
val to_string : ?ns_prefix:(string -> string option) -> t -> string
val to_buffer : ?ns_prefix:(string -> string option) -> t -> Buffer.t -> unit

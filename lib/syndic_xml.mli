
type dtd = string option
(** The type for the optional
    {{:http://www.w3.org/TR/REC-xml/#dt-doctype}DTD}. *)

(** A XML tree. *)
type t =
  | Node of Xmlm.tag * t list
  | Leaf of string

val of_xmlm : Xmlm.input -> (dtd * t)
(** [of_xmlm doc] converts an XML document [doc] into a DTD and a
    tree representing the document. *)

val of_html : ?enc:Xmlm.encoding -> ?entity:(string -> string option) ->
              string -> t list
(** [of_html s] tries to convert a sequence of HTML tags into a list
    of trees.  This function tries to be permissive and is suitable
    for excepts of HTML code.

    @param enc gives the character encoding.  Default: [`UTF_8].

    @param entity how [Xmlm] unknown entities are transformed.  By
    default, they are preserved in their original form. *)

val to_xmlm : t -> Xmlm.output -> unit

val to_string : t -> string

val to_buffer : t -> Buffer.t -> unit

module XML : sig
  type t = Syndic_xml.t
  type node = Syndic_xml.pos * Syndic_xml.tag * t list

  val generate_catcher :
       ?namespaces:string list
    -> ?attr_producer:(string * (xmlbase:Uri.t option -> string -> 'a)) list
    -> ?data_producer:(string * (xmlbase:Uri.t option -> node -> 'a)) list
    -> ?leaf_producer:(xmlbase:Uri.t option -> Xmlm.pos -> string -> 'a)
    -> (pos:Xmlm.pos -> 'a list -> 'b)
    -> xmlbase:Uri.t option
    -> node
    -> 'b

  val dummy_of_xml :
       ctor:(xmlbase:Uri.t option -> string -> 'a)
    -> xmlbase:Uri.t option
    -> node
    -> 'a

  val xmlbase_of_attr :
    xmlbase:Uri.t option -> Xmlm.attribute list -> Uri.t option
end

module Util : sig
  val find : ('a -> bool) -> 'a list -> 'a option
  val recursive_find : (XML.t -> bool) -> XML.t -> XML.t option
  val filter_map : 'a list -> ('a -> 'b option) -> 'b list
  val take : 'a list -> int -> 'a list
  val tag_is : Xmlm.tag -> string -> bool
  val attr_is : Xmlm.attribute -> string -> bool
  val datas_has_leaf : XML.t list -> bool
  val get_leaf : XML.t list -> string
  val get_attrs : Xmlm.tag -> Xmlm.attribute list
  val get_value : Xmlm.attribute -> string
  val get_attr_name : Xmlm.attribute -> string
  val get_tag_name : Xmlm.tag -> string
  val only_whitespace : string -> bool

  (** {2 Helpers to output XML} *)

  val dummy_pos : Xmlm.pos
  (** A dummy position when generating XML files. *)

  val add_attr :
    Xmlm.name -> string option -> Xmlm.attribute list -> Xmlm.attribute list

  val add_attr_uri :
    Xmlm.name -> Uri.t option -> Xmlm.attribute list -> Xmlm.attribute list

  val tag : string -> Xmlm.tag
  (** [tag n] returns a tag with name [n], no namespace, and no attributes. *)

  val node_data : Xmlm.tag -> string -> XML.t
  (** [node_data tag content] returns a node named [tag] with data set to
      [content]. *)

  val node_uri : Xmlm.tag -> Uri.t -> XML.t
  val add_node_data : Xmlm.tag -> string option -> XML.t list -> XML.t list
  val add_node_uri : Xmlm.tag -> Uri.t option -> XML.t list -> XML.t list

  val add_nodes_rev_map : ('a -> XML.t) -> 'a list -> XML.t list -> XML.t list
  (** [add_nodes_rev_map f l nodes] apply [f] to each element of [l] and add
      the resulting HTML trees in reverse order in front of [nodes]. *)

  val add_node_option : ('a -> XML.t) -> 'a option -> XML.t list -> XML.t list
  (** [add_node_option f o nodes]: if [o] is [None], return [nodes]; otherwise
      apply [f] to the value carried by [o] and add the resulting XML tree in
      front of [nodes]. *)
end

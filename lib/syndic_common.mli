module XML : sig
  type t = Syndic_xml.t

  val generate_catcher :
    ?namespaces:string list ->
    ?attr_producer:(string * ('a list -> Xmlm.pos -> string -> 'a)) list ->
    ?data_producer:(string * ('a list -> Xmlm.pos * Xmlm.tag * t list -> 'a)) list ->
    ?leaf_producer:('a list -> Xmlm.pos -> string -> 'a) ->
    ('a list -> 'b) -> Xmlm.pos * Xmlm.tag * t list -> 'b

  val dummy_of_xml : ctor:(string -> 'a) ->
    Xmlm.pos * Xmlm.tag * t list -> 'a
end

module Util : sig
  val find : ('a -> bool) -> 'a list -> 'a option
  val filter_map : 'a list -> ('a -> 'b option) -> 'b list

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

  val add_attr : Xmlm.name -> string option ->
                 Xmlm.attribute list -> Xmlm.attribute list
  val add_attr_uri : Xmlm.name -> Uri.t option ->
                     Xmlm.attribute list -> Xmlm.attribute list

  val tag : string -> Xmlm.tag
  (** [tag n] returns a tag with name [n], no namespace, and no
      attributes. *)

  val node_data : Xmlm.tag -> string -> XML.t
  (** [node_data name content] returns a node named [name] with data
      set to [content]. *)

  val node_uri : Xmlm.tag -> Uri.t -> XML.t

  val add_node_data : Xmlm.tag -> string option -> XML.t list -> XML.t list
  val add_node_uri : Xmlm.tag -> Uri.t option -> XML.t list -> XML.t list
  val add_nodes_map : ('a -> XML.t) -> 'a list -> XML.t list -> XML.t list
  val add_node_option : ('a -> XML.t) -> 'a option -> XML.t list -> XML.t list
end

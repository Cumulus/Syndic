module XML : sig
  type tree =
    | Node of Xmlm.tag * tree list
    | Leaf of string

  val generate_catcher :
    ?attr_producer:(string * ('a list -> string -> 'a)) list ->
    ?data_producer:(string * ('a list -> Xmlm.tag * tree list -> 'a)) list ->
    ?leaf_producer:('a list -> string -> 'a) ->
    ('a list -> 'b) -> Xmlm.tag * tree list -> 'b

  val dummy_of_xml : ctor:(string -> 'a) ->
    Xmlm.tag * tree list -> 'a
end

module Error : sig
  type expected_type =
    | Attr of string
    | Tag of string
    | Data
    | Root

  exception Expected of expected_type * expected_type
  exception Expected_Leaf

  val string_of_expectation : expected_type * expected_type -> string
  val raise_expectation : expected_type -> expected_type -> 'a
end

module Util : sig
  val find : ('a -> bool) -> 'a list -> 'a option

  val tag_is : Xmlm.tag -> string -> bool
  val attr_is : Xmlm.attribute -> string -> bool

  val datas_has_leaf : XML.tree list -> bool

  val get_leaf : XML.tree list -> string
  val get_attrs : Xmlm.tag -> Xmlm.attribute list
  val get_value : Xmlm.attribute -> string
  val get_attr_name : Xmlm.attribute -> string
  val get_tag_name : Xmlm.tag -> string

end

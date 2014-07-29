module Error : sig
  type expected_type

  exception Expected of expected_type * expected_type
  exception Expected_Leaf
  exception Malformed_URL of string
  exception Duplicate_Link of ((Uri.t * string * string) * (string * string))

  val string_of_expectation : expected_type * expected_type -> string
  val string_of_duplicate_exception :
    (Uri.t * string * string) * (string * string) -> string
end

type rel =
  | Alternate
  | Related
  | Self
  | Enclosure
  | Via
  | Link of Uri.t

type type_content =
  | Html
  | Text
  | Xhtml
  | Mime of string

type content = {
  ty : type_content;
  src : Uri.t option;
}

type author =
  {
    name : string;
    uri : Uri.t option;
    email : string option;
  }

type author' = [
  | `Name of string
  | `URI of Uri.t
  | `Email of string
]

val author_of_xml : Xmlm.tag * Common.XML.tree list -> author
val author_of_xml' : Xmlm.tag * Common.XML.tree list -> [> author' ] list

type category =
  {
    term : string;
    scheme : Uri.t option;
    label : string option;
  }

type category' = [
  | `Term of string
  | `Scheme of Uri.t
  | `Label of string
]

val category_of_xml : Xmlm.tag * Common.XML.tree list -> category
val category_of_xml' : Xmlm.tag * Common.XML.tree list -> [> category' ] list

val contributor_of_xml : Xmlm.tag * Common.XML.tree list -> author
val contributor_of_xml' : Xmlm.tag * Common.XML.tree list -> [> author' ] list

type generator =
  {
    version : string option;
    uri : Uri.t option;
    content : string;
  }

type generator' = [
  | `URI of Uri.t
  | `Version of string
  | `Content of string
]

val generator_of_xml : Xmlm.tag * Common.XML.tree list -> generator
val generator_of_xml' : Xmlm.tag * Common.XML.tree list -> [> generator' ] list

type icon = Uri.t
type icon' = [ `URI of Uri.t ]

val icon_of_xml : Xmlm.tag * Common.XML.tree list -> icon
val icon_of_xml' : Xmlm.tag * Common.XML.tree list -> [> icon' ] list

type id = Uri.t
type id' = [ `URI of Uri.t ]

val id_of_xml : Xmlm.tag * Common.XML.tree list -> id
val id_of_xml' : Xmlm.tag * Common.XML.tree list -> [> id' ] list

type link =
  {
    href : Uri.t;
    rel : rel;
    type_media : string option;
    hreflang : string option;
    title : string option;
    length : int option;
  }

type link' = [
  | `HREF of Uri.t
  | `Rel of rel
  | `Type of string
  | `HREFLang of string
  | `Title of string
  | `Length of int
]

val link_of_xml : Xmlm.tag * Common.XML.tree list -> link
val link_of_xml' : Xmlm.tag * Common.XML.tree list -> [> link' ] list

type source = {
  author : author * author list;
  category : category list;
  contributor : author list;
  generator : generator option;
  icon : Uri.t option;
  id : Uri.t;
  link : link * link list;
  logo : Uri.t option;
  rights : string option;
  subtitle : string option;
  title : string;
  updated : string option;
}

type entry = {
  author : author * author list;
  category : category list;
  content : (content * string) option;
  contributor : author list;
  id : Uri.t;
  link : link list;
  published : string option;
  rights : string option;
  source : source list;
  summary : string option;
  title : string;
  updated : string;
}

type feed = {
  author : author list;
  category : category list;
  contributor : author list;
  generator : generator option;
  icon : Uri.t option;
  id : Uri.t;
  link : link list;
  logo : Uri.t option;
  rights : string option;
  subtitle : string option;
  title : string;
  updated : string;
  entry : entry list;
}

val analyze : Xmlm.input -> feed

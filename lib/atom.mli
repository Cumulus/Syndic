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

type author =
  {
    name : string;
    uri : Uri.t option;
    email : string option;
  }

type category =
  {
    term : string;
    scheme : Uri.t option;
    label : string option;
  }

type generator =
  {
    version : string option;
    uri : Uri.t option;
    content : string;
  }

type icon = Uri.t

type id = Uri.t

type rel =
  | Alternate
  | Related
  | Self
  | Enclosure
  | Via
  | Link of Uri.t

type link =
  {
    href : Uri.t;
    rel : rel;
    type_media : string option;
    hreflang : string option;
    title : string option;
    length : int option;
  }

type logo = Uri.t

type published = Netdate.t

type rights = string

type title = string

type subtitle = string

type updated = Netdate.t

type source =
  {
    authors: author * author list;
    categories: category list;
    contributors: author list;
    generator: generator option;
    icon: icon option;
    id: id;
    links: link * link list;
    logo: logo option;
    rights: rights option;
    subtitle: subtitle option;
    title: title;
    updated: updated option;
  }

type type_content =
  | Html
  | Text
  | Xhtml
  | Mime of string

type content =
  {
    ty : type_content;
    src : Uri.t option;
    data : string;
  }

type summary = string

type entry =
  {
    authors: author * author list;
    categories: category list;
    content: content option;
    contributors: author list;
    id: id;
    links: link list;
    published: published option;
    rights: rights option;
    sources: source list;
    summary: summary option;
    title: title;
    updated: updated;
  }

type feed =
  {
    authors : author list;
    categories : category list;
    contributors : author list;
    generator : generator option;
    icon : icon option;
    id : id;
    links : link list;
    logo : logo option;
    rights : rights option;
    subtitle : subtitle option;
    title : title;
    updated : updated;
    entries : entry list;
  }

val analyze : Xmlm.input -> feed

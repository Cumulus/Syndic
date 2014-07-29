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

val author_of_xml : Xmlm.tag * Common.XML.tree list -> author

type category =
  {
    term : string;
    scheme : Uri.t option;
    label : string option;
  }

val category_of_xml : Xmlm.tag * Common.XML.tree list -> category

val contributor_of_xml : Xmlm.tag * Common.XML.tree list -> author

type generator =
  {
    version : string option;
    uri : Uri.t option;
    content : string;
  }

val generator_of_xml : Xmlm.tag * Common.XML.tree list -> generator

type icon = Uri.t

val icon_of_xml : Xmlm.tag * Common.XML.tree list -> icon

type id = Uri.t

val id_of_xml : Xmlm.tag * Common.XML.tree list -> id

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

val link_of_xml : Xmlm.tag * Common.XML.tree list -> link

type logo = Uri.t

val logo_of_xml : Xmlm.tag * Common.XML.tree list -> logo

type published = Netdate.t

val published_of_xml : Xmlm.tag * Common.XML.tree list -> published

type rights = string

val rights_of_xml : Xmlm.tag * Common.XML.tree list -> rights

type title = string

val title_of_xml : Xmlm.tag * Common.XML.tree list -> title

type subtitle = string

val subtitle_of_xml : Xmlm.tag * Common.XML.tree list -> subtitle

type updated = Netdate.t

val updated_of_xml : Xmlm.tag * Common.XML.tree list -> updated

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

val source_of_xml : Xmlm.tag * Common.XML.tree list -> source

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

val content_of_xml : Xmlm.tag * Common.XML.tree list -> content

type summary = string

val summary_of_xml : Xmlm.tag * Common.XML.tree list -> summary

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

type feed' = [
  | `Author of author
  | `Category of category
  | `Contributor of author
  | `Generator of generator
  | `Icon of icon
  | `ID of id
  | `Link of link
  | `Logo of logo
  | `Rights of rights
  | `Subtitle of subtitle
  | `Title of title
  | `Updated of updated
  | `Entry of entry
]

val entry_of_xml : [< feed' > `Author ] list -> Xmlm.tag * Common.XML.tree list -> entry

type feed = {
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

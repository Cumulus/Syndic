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

type author = {
  name : string;
  uri : Uri.t option;
  email : string option;
}

type category = {
  term : string;
  scheme : Uri.t option;
  label : string option;
}

type generator = {
  version : string option;
  uri : Uri.t option;
}

type link = {
  href : Uri.t;
  rel : rel;
  type_media : string option;
  hreflang : string option;
  title : string option;
  length : int option;
}

type source = {
  author : author * author list;
  category : category list;
  contributor : author list;
  generator : (generator * string) option;
  icon : Uri.t option;
  id : Uri.t;
  link : link * link list;
  logo : Uri.t option;
  rights : string option;
  subtitle : string option;
  title : string;
  updated : string option;
}

type source' =
    [ `SourceAuthor of author
    | `SourceCategory of category
    | `SourceContributor of author
    | `SourceGenerator of generator * string
    | `SourceIcon of Uri.t
    | `SourceId of Uri.t
    | `SourceLink of link
    | `SourceLogo of Uri.t
    | `SourceRights of string
    | `SourceSubtitle of string
    | `SourceTitle of string
    | `SourceUpdated of string ]

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

type entry' =
    [ `EntryAuthor of author
    | `EntryCategory of category
    | `EntryContent of content * string
    | `EntryContributor of author
    | `EntryId of Uri.t
    | `EntryLink of link
    | `EntryPublished of string
    | `EntryRights of string
    | `EntrySource of source
    | `EntrySummary of string
    | `EntryTitle of string
    | `EntryUpdated of string ]

type feed = {
  author : author list;
  category : category list;
  contributor : author list;
  generator : (generator * string) option;
  icon : Uri.t option;
  id : Uri.t;
  link : link * link list;
  logo : Uri.t option;
  rights : string option;
  subtitle : string option;
  title : string;
  updated : string;
  entry : entry list;
}

type feed' =
    [ `FeedAuthor of author
    | `FeedCategory of category
    | `FeedContributor of author
    | `FeedEntry of entry
    | `FeedGenerator of generator * string
    | `FeedIcon of Uri.t
    | `FeedId of Uri.t
    | `FeedLink of link
    | `FeedLogo of Uri.t
    | `FeedRights of string
    | `FeedSubtitle of string
    | `FeedTitle of string
    | `FeedUpdated of string ]

type expected_type =
  | EAttr of string
  | ETag of string
  | EData

exception Expected of expected_type * expected_type
exception ExpectedLeaf
exception Malformed_URL of string
exception DuplicateLink of ((Uri.t * string * string) * (string * string))

val string_of_expectation : expected_type * expected_type -> string
val raise_expectation : expected_type -> expected_type -> 'a

val raise_duplicate_string : link -> string * string -> 'a
val string_of_duplicate_exception :
  (Uri.t * string * string) * (string * string) -> string

val make_author :
  [ `AuthorEmail of string | `AuthorName of string | `AuthorURI of Uri.t ]
  list -> author
val make_category :
  [ `CategoryLabel of string
  | `CategoryScheme of Uri.t
  | `CategoryTerm of string ] list -> category
val make_contributor :
  [ `AuthorEmail of string | `AuthorName of string | `AuthorURI of Uri.t ]
  list -> author
val make_generator :
  [ `GeneratorContent of string
  | `GeneratorURI of Uri.t
  | `GeneratorVersion of string ] list -> generator * string
val make_icon : [ `IconURI of Uri.t ] list -> Uri.t
val make_id : [ `IdURI of Uri.t ] list -> Uri.t
val make_link :
  [ `LinkHREF of Uri.t
  | `LinkHREFLang of string
  | `LinkLength of int
  | `LinkRel of rel
  | `LinkTitle of string
  | `LinkType of string ] list -> link
val make_logo : [ `LogoURI of Uri.t ] list -> Uri.t
val make_published : [ `PublishedDate of string ] list -> string
val make_rights : [ `RightData of string ] list -> string
val make_title : [ `TitleData of string ] list -> string
val make_subtitle : [ `SubtitleData of string ] list -> string
val make_updated : [ `UpdatedData of string ] list -> string
val make_source : source' list -> source
val make_content :
  [ `ContentData of string
  | `ContentSRC of Uri.t
  | `ContentType of type_content ] list -> content * string
val make_summary : [ `SummaryData of string ] list -> string
val uniq_link_alternate : link list -> link list
val make_entry : [< feed' > `FeedAuthor ] list -> entry' list -> entry
val make_feed : feed' list -> feed

val analyze : Xmlm.input -> feed

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
  generator : (generator * string) option;
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

type expected_type =
  | EAttr of string
  | ETag of string
  | EData

exception Expected of expected_type * expected_type
exception ExpectedLeaf
exception Malformed_URL of string
exception Duplicate_Link of ((Uri.t * string * string) * (string * string))

val string_of_expectation : expected_type * expected_type -> string
val raise_expectation : expected_type -> expected_type -> 'a

val raise_duplicate_string : link -> string * string -> 'a
val string_of_duplicate_exception :
  (Uri.t * string * string) * (string * string) -> string

val analyze : Xmlm.input -> feed

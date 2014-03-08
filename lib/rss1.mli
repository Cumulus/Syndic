type channel = {
  about: Uri.t;
  title: string;
  link: Uri.t;
  description: string;
  image: Uri.t option;
  items: Uri.t list;
  textinput: Uri.t option;
}

type expected_type =
  | EAttr of string
  | ETag of string
  | EData

exception Expected of expected_type * expected_type
exception ExpectedLeaf
exception Malformed_URL of string

val string_of_expectation : expected_type * expected_type -> string
val raise_expectation : expected_type -> expected_type -> 'a

val analyze : Xmlm.input -> channel

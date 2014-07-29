module Error : sig
  type expected_type

  exception Expected of expected_type * expected_type
  exception Expected_Leaf
  exception Malformed_URL of string

  val string_of_expectation : expected_type * expected_type -> string
end

type channel = {
  about: Uri.t;
  title: string;
  link: Uri.t;
  description: string;
  image: Uri.t option;
  items: Uri.t list;
  textinput: Uri.t option;
}

type image = {
  about: Uri.t;
  title: string;
  url: Uri.t;
  link: Uri.t;
}

type item = {
  about: Uri.t; (* about and link are same *)
  title: string;
  link: Uri.t;
  description: string option;
}

type textinput = {
  about: Uri.t;
  title: string;
  description: string;
  name: string;
  link: Uri.t;
}

type rdf = {
  channel: channel;
  image: image option;
  item: item list;
  textinput: textinput option;
}

val analyze : Xmlm.input -> rdf

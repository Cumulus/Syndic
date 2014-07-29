module Error : sig
  type expected_type

  exception Expected of expected_type * expected_type
  exception Expected_Leaf
  exception Malformed_URL of string

  val string_of_expectation : expected_type * expected_type -> string
end

type image = {
  url: Uri.t;
  title: string;
  link: Uri.t;
  width: int;
  height: int;
  description: string option;
}

type cloud = {
  domain: Uri.t;
  port: int;
  path: string;
  registerProcedure: string;
  protocol: string;
}

type textinput = {
  title: string;
  description: string;
  name: string;
  link: Uri.t;
}

type category = {
  data: string;
  domain: Uri.t option;
}

type enclosure = {
  url: Uri.t;
  length: int;
  mime: string;
}

type guid = {
  data: Uri.t; (* must be uniq *)
  permalink: bool; (* default true *)
}

type source = {
  data: string;
  url: Uri.t;
}

(* must have title or description *)

type story =
  | All of string * string
  | Title of string
  | Description of string

type item = {
  story: story;
  link: Uri.t option;
  author:  string option; (* e-mail *)
  category: category option;
  comments: Uri.t option;
  enclosure: enclosure option;
  guid: guid option;
  pubDate: string option; (* date *)
  source: source option;
}

type channel = {
  title: string;
  link: Uri.t;
  description: string;
  language: string option;
  copyright: string option;
  managingEditor: string option; (* e-mail *)
  webMaster: string option; (* e-mail *)
  pubDate: string option; (* date *)
  lastBuildDate: string option; (* date *)
  category: string option;
  generator: string option;
  docs: Uri.t option;
  cloud: cloud option; (* lol *)
  ttl: int option;
  image: image option;
  rating: int option; (* lol *)
  textInput: textinput option;
  skipHours: int option;
  skipDays: int option;
  items: item list;
}

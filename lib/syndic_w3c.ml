open Syndic_common.XML
open Syndic_common.Util

module XML = Syndic_xml
module Error = Syndic_error

type error' = [
  | `Line of string
  | `Column of string
  | `Text of string
  | `Element of string
  | `Parent of string
  | `Value of string
]

type error =
  {
    line    : int;    (** Within the source code of the validated document,
                          refers to the line where the error was
                          detected. *)
    column  : int;    (** Within the source code of the validated document,
                          refers to the line where the column was
                          detected. *)
    text    : string; (** The actual error message. *)
    element : string; (** Element in the feed where the message was
                          triggered. *)
    parent  : string; (** In the feed, parent of the element. *)
    value   : string; (** If applicable the value of the element, attribute
                          or content which triggered the message. *)
  }

let url = function
  | `String data ->
    Uri.of_string
      ("http://validator.w3.org/feed/check.cgi?output=soap12&rawdata="
       ^ data)
  | `Uri uri ->
    Uri.of_string
      ("http://validator.w3.org/feed/check.cgi?output=soap12&url="
       ^ (Uri.to_string uri))

let make_error ~pos (l : [< error'] list) =
  let line = match find (function `Line _ -> true | _ -> false) l with
    | Some (`Line line) -> (try int_of_string line with _ -> 0)
    | _ -> 0
  in
  let column = match find (function `Column _ -> true | _ -> false) l with
    | Some (`Column column) -> (try int_of_string column with _ -> 0)
    | _ -> 0
  in
  let text = match find (function `Text _ -> true | _ -> false) l with
    | Some (`Text text) -> text
    | _ -> ""
  in
  let element = match find (function `Element _ -> true | _ -> false) l with
    | Some (`Element element) -> element
    | _ -> ""
  in
  let parent = match find (function `Parent _ -> true | _ -> false) l with
    | Some (`Parent parent) -> parent
    | _ -> ""
  in
  let value = match find (function `Value _ -> true | _ -> false) l with
    | Some (`Value value) -> value
    | _ -> ""
  in
  ({ line; column; text; element; parent; value; } : error)

let error_of_xml =
  let data_producer = [
    "line", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Line a);
    "column", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Column a);
    "text", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Text a);
    "element", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Element a);
    "parent", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Parent a);
    "value", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Value a);
  ] in
  generate_catcher
    ~data_producer
    make_error

let make_errorlist ~pos (l : error list) = l

let errorlist_of_xml =
  let data_producer = [
    "error", error_of_xml;
  ] in
  generate_catcher
    ~data_producer
    make_errorlist

let find_errorlist l =
  recursive_find
    (function XML.Node (_, t, _) -> tag_is t "errorlist" | _ -> false) l

let to_error { line; column; text; _ } =
  ((line, column), text)

let parse ?xmlbase input =
  match (XML.of_xmlm input |> snd) |> find_errorlist with
  | Some (XML.Node (p, t, d)) -> errorlist_of_xml ~xmlbase (p, t, d)
  | _ -> []

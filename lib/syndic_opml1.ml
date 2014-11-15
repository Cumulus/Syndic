open Syndic_common.XML
open Syndic_common.Util
open Printf

module XML = Syndic_xml
module Error = Syndic_error
module Date = Syndic_date

type head =
  {
    title : string;
    date_created : CalendarLib.Calendar.t option;
    date_modified : CalendarLib.Calendar.t;
    owner_name : string;
    owner_email : string;
    expansion_state : int list;
    vert_scroll_state : int option;
    window_top : int option;
    window_left : int option;
    window_bottom : int option;
    window_right : int option;
  }

let string_of_xml name (pos, _, datas) =
  try get_leaf datas
  with Not_found -> raise (Error.Error (pos, name ^ " must not be empty"))

let title_of_xml = string_of_xml "<title>"

let owner_name_of_xml = string_of_xml "<ownerName>"

let owner_email_of_xml = string_of_xml "<ownerEmail>"

let expansion_state_of_xml (pos, _, datas) =
  let explode s =
    let rec aux acc i =
      if i = String.length s then acc
      else aux (s.[i] :: acc) (succ i)
    in aux [] 0 |> List.rev
  in
  let implode l =
    let rec aux s = function
      | x :: xs -> aux (s ^ Char.escaped x) xs
      | [] -> s
    in aux "" l
  in
  let split sep s =
    let rec aux acc_char acc = function
      | x :: xs when x = sep -> aux [] (List.rev acc_char :: acc) xs
      | x :: xs -> aux (x :: acc_char) acc xs
      | [] -> (List.rev acc_char) :: acc
    in explode s
       |> aux [] []
       |> List.rev
       |> List.map implode
  in
  try get_leaf datas
      |> split ','
      |> List.map int_of_string
  with Not_found -> []
     | _ -> raise (Error.Error (pos, "<expansionState> must be a list of \
                                     numbers separated by commas as 1,2,3"))

let int_of_xml name (pos, _, datas) =
  try get_leaf datas |> int_of_string
  with Not_found -> raise (Error.Error (pos, name ^ " must not be empty"))
     | Failure _ -> raise (Error.Error (pos, name ^ " must be an integer"))

let vert_scroll_state_of_xml = int_of_xml "<vertScrollState>"

let window_top_of_xml = int_of_xml "<windowTop>"

let window_left_of_xml = int_of_xml "<windowLeft>"

let window_bottom_of_xml = int_of_xml "<windowBotton>"

let window_right_of_xml = int_of_xml "<windowRight>"

type head' = [
  | `Title of string
  | `DateCreated of CalendarLib.Calendar.t
  | `DateModified of CalendarLib.Calendar.t
  | `OwnerName of string
  | `OwnerEmail of string
  | `ExpansionState of int list
  | `VertScrollState of int
  | `WindowTop of int
  | `WindowLeft of int
  | `WindowBottom of int
  | `WindowRight of int
]

let make_head ~pos (l : [< head'] list) =
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title s) -> s
    | _ -> raise (Error.Error (pos, "<head> MUST contains exactly one <title> \
                                     				     element"))
  in
  let date_created =
    match find (function `DateCreated _ -> true | _ -> false) l with
    | Some (`DateCreated d) -> Some d
    | _ -> None
  in
  let date_modified =
    match find (function `DateModified _ -> true | _ -> false) l with
    | Some (`DateModified d) -> d
    | _ -> raise (Error.Error (pos, "<head> MUST contains exactly one \
                                    <dateModified> element"))
  in
  let owner_name =
    match find (function `OwnerName _ -> true | _ -> false) l with
    | Some (`OwnerName s) -> s
    | _ -> raise (Error.Error (pos, "<head> MUST contains exactly one \
                                    <ownerName> element"))
  in
  let owner_email =
    match find (function `OwnerEmail _ -> true | _ -> false) l with
    | Some (`OwnerEmail s) -> s
    | _ -> raise (Error.Error (pos, "<head> MUST contains exactly one \
                                    <ownerEmail> element"))
  in
  let expansion_state =
    match find (function `ExpansionState _ -> true | _ -> false) l with
    | Some (`ExpansionState l) -> l
    | _ -> []
  in
  let vert_scroll_state =
    match find (function `VertScrollState _ -> true | _ -> false) l with
    | Some (`VertScrollState n) -> Some n
    | _ -> None
  in
  let window_top =
    match find (function `WindowTop _ -> true | _ -> false) l with
    | Some (`WindowTop h) -> Some h
    | _ -> None
  in
  let window_left =
    match find (function `WindowLeft _ -> true | _ -> false) l with
    | Some (`WindowLeft x) -> Some x
    | _ -> None
  in
  let window_bottom =
    match find (function `WindowBottom _ -> true | _ -> false) l with
    | Some (`WindowBottom y) -> Some y
    | _ -> None
  in
  let window_right =
    match find (function `WindowRight _ -> true | _ -> false) l with
    | Some (`WindowRight r) -> Some r
    | _ -> None
  in
  {
    title;
    date_created;
    date_modified;
    owner_name;
    owner_email;
    expansion_state;
    vert_scroll_state;
    window_top;
    window_left;
    window_bottom;
    window_right
  }


let date_of_xml name (pos, _, datas) =
  let d =
    try get_leaf datas
    with Not_found -> raise (Error.Error (pos, name ^ " must not be empty")) in
  try Date.of_rfc822 d
  with _ -> raise (Error.Error (pos, sprintf "Date %S incorrect" d))

let head_of_xml =
  let data_producer = [
    "title", (fun _ a -> `Title (title_of_xml a));
    "dateCreated", (fun _ a -> `DateCreated (date_of_xml "<dateCreated>" a));
    "dateModified", (fun _ a -> `DateModified (date_of_xml "<dateModified>" a));
    "ownerName", (fun _ a -> `OwnerName (owner_name_of_xml a));
    "ownerEmail", (fun _ a -> `OwnerEmail (owner_email_of_xml a));
    "expansionState", (fun _ a -> `ExpansionState (expansion_state_of_xml a));
    "vertScrollState", (fun _ a -> `VertScrollState (vert_scroll_state_of_xml a));
    "windowTop", (fun _ a -> `WindowTop (window_top_of_xml a));
    "windowLeft", (fun _ a -> `WindowLeft (window_left_of_xml a));
    "windowBottom", (fun _ a -> `WindowBottom (window_bottom_of_xml a));
    "windowRight", (fun _ a -> `WindowRight (window_right_of_xml a))
    ] in
  fun ((pos, _, _) as xml) ->
  generate_catcher
    ~data_producer
    (make_head ~pos) xml

let head_of_xml' =
  let data_producer = [
    "title", (fun _ -> dummy_of_xml ~ctor:(fun a -> `Title a));
    "dateCreated", (fun _ -> dummy_of_xml ~ctor:(fun a -> `DateCreated a));
    "dateModified", (fun _ -> dummy_of_xml ~ctor:(fun a -> `DateModified a));
    "ownerName", (fun _ -> dummy_of_xml ~ctor:(fun a -> `OwnerName a));
    "ownerEmail", (fun _ -> dummy_of_xml ~ctor:(fun a -> `OwnerEmail a));
    "expansionState", (fun _ -> dummy_of_xml ~ctor:(fun a -> `ExpansionSate a));
    "vertScrollState", (fun _ -> dummy_of_xml ~ctor:(fun a -> `VertScrollState a));
    "windowTop", (fun _ -> dummy_of_xml ~ctor:(fun a -> `WindowTop a));
    "windowLeft", (fun _ -> dummy_of_xml ~ctor:(fun a -> `WindowLeft a));
    "windowBottom", (fun _ -> dummy_of_xml ~ctor:(fun a -> `WindowBottom a));
    "windowRight", (fun _ -> dummy_of_xml ~ctor:(fun a -> `WindowRight a))
  ] in
  generate_catcher
    ~data_producer
    (fun x -> x)

type outline =
  {
    text : string option;
    type_ : string option;
    is_comment : bool; (* see common attributes *)
    is_breakpoint : bool; (* see common attributes *)
    (* attrs : (string * string) list; *)
    outlines : outline list
  }

let string_option_of_xml (pos, _, datas) =
  try Some (get_leaf datas)
  with Not_found -> None

let text_of_xml = string_option_of_xml
let type_of_xml = string_option_of_xml

let bool_option_of_xml (pos, _, datas) =
  try Some ((get_leaf datas) |> bool_of_string)
  with Not_found -> None
     | Failure _ -> raise (Error.Error (pos, "bool attributes must have \
                                             true or false value"))

let is_comment_of_xml xml =
  match bool_option_of_xml xml with Some v -> v | _ -> false
let is_breakpoint_of_xml xml =
  match bool_option_of_xml xml with Some v -> v | _ -> false

type outline' = [
  | `Text of string
  | `Type of string
  | `IsComment of bool
  | `IsBreakpoint of bool
  | `Outline of outline
]

let make_outline ~pos (l : [< outline'] list) =
  let text = match find (function `Text _ -> true | _ -> false) l with
    | Some (`Text t) -> Some t
    | _ -> None
  in
  let type_ = match find (function `Type _ -> true | _ -> false) l with
    | Some (`Type t) -> Some t
    | _ -> None
  in
  let is_comment =
    match find (function `IsComment _ -> true | _ -> false) l with
    | Some (`IsComment b) -> true
    | _ -> false
  in
  let is_breakpoint =
    match find (function `IsBreakpoint _ -> true | _ -> false) l with
    | Some (`IsBreakpoint b) -> true
    | _ -> false
  in
  let outlines =
    (*    l
          |> List.filter (function `Outline _ -> true | _ -> false)
          |> List.map (function `Outline o -> o | _ -> assert false)*)
    List.fold_left
      (fun acc -> function `Outline o -> o :: acc | _ -> acc) [] l (* from dino *)
    |> List.rev
  in
  {
    text;
    type_;
    is_comment;
    is_breakpoint;
    outlines
  }

let rec outline_of_xml ((pos, _, _) as xml) =
  let attr_producer = [
    "text", (fun _ _ a -> `Text a);
    "type", (fun _ _ a -> `Type a);
    "isComment",
    (fun _ _ a ->
       `IsComment
         (try bool_of_string a
          with _ -> raise(Error.Error (pos, "<isComment> must have true or \
                                            false value."))));
    "isBreakpoint",
    (fun _ _ a ->
       `IsBreakpoint
         (try bool_of_string a
          with _ -> raise (Error.Error (pos, "<isBreakpoint> must have true \
                                             or false value."))))
  ] in
  let data_producer = [
    "outline", (fun _ a -> (`Outline (outline_of_xml a)))
  ] in
  generate_catcher
    ~attr_producer
    ~data_producer
    (make_outline ~pos) xml

let rec outline_of_xml' () =
  let attr_producer = [
    "text", (fun _ _ a -> `Text a);
    "type", (fun _ _ a -> `Type a);
    "isComment", (fun _ _ a -> `IsComment a);
    "isBreakpoint", (fun _ _ a -> `IsBreakpoint a)
  ] in
  let data_producer = [
    "outline", (fun _ a -> `Outline (outline_of_xml' () a))
  ] in
  generate_catcher
    ~attr_producer
    ~data_producer
    (fun x -> x)

type body = outline list

type body' = [`Outline of outline]

let make_body ~pos (l : [< body'] list) =
  let l =
    List.map (function `Outline o -> o) l
    |> List.rev
  in
  if List.length l <> 0 then l
  else raise (Error.Error (pos, "Body must contains one <outline> element."))

let body_of_xml ((pos, _, _) as xml) =
  let data_producer = [
    "outline", (fun _ a -> (`Outline (outline_of_xml a)))
  ] in
  generate_catcher ~data_producer (make_body ~pos) xml

let body_of_xml' =
  let data_producer = [
    "outline", (fun _ a -> (`Outline (outline_of_xml' () a)))
  ] in
  generate_catcher
    ~data_producer
    (fun x -> x)

type opml =
  {
    version : string;
    head : head;
    body : body
  }

type opml' =  [
  | `Version of string
  | `Head of head
  | `Body of body
]

let make_opml ~pos (l : [< opml'] list) =
  let version = match find (function `Version _ -> true | _ -> false) l with
    | Some (`Version v) -> v
    | _ -> raise (Error.Error (pos, "Opml tag must have <version> attribut"))
  in
  let head = match find (function `Head _ -> true | _ -> false) l with
    | Some (`Head h) -> h
    | _ -> raise (Error.Error (pos, "Opml tag must have exactly one <head> \
                                    element"))
  in
  let body = match find (function `Body _ -> true | _ -> false) l with
    | Some (`Body b) -> b
    | _ -> raise (Error.Error (pos, "Opml tag must have exactly one <body> \
                                    element"))
  in { version; head; body}

let opml_of_xml ((pos, _, _) as xml) =
  let attr_producer = [
    "version", (fun _ _ a -> `Version a)
  ] in
  let data_producer = [
    "head", (fun _ a -> `Head (head_of_xml a));
    "body", (fun _ a -> `Body (body_of_xml a))
  ] in
  generate_catcher
    ~attr_producer
    ~data_producer
    (make_opml ~pos) xml

let opml_of_xml' =
  let attr_producer = [
    "version", (fun _ _ a -> `Version a)
  ] in
  let data_producer = [
    "head", (fun _ a -> `Head (head_of_xml' a));
    "body", (fun _ a -> `Body (body_of_xml' a))
  ] in
  generate_catcher
    ~attr_producer
    ~data_producer
    (fun x -> x)

let find_opml l =
  find (function XML.Node (_, t, _) -> tag_is t "opml" | _ -> false) l

let parse input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, data) ->
    if tag_is tag "opml" then
      opml_of_xml (pos, tag, data)
    else
      begin match find_opml data with
        | Some (XML.Node (p, t, d)) -> opml_of_xml (p, t, d)
        | _ -> raise (Error.Error ((0, 0),
                                   "document MUST contains exactly one <opml> \
                                    				    element"))
      end
  | _ -> raise (Error.Error ((0, 0),
                             "document MUST contains exactly one <opml> \
                              			      element"))

let unsafe input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, data) ->
    if tag_is tag "opml" then
      `Opml (opml_of_xml' (pos, tag, data))
    else
      begin match find_opml data with
        | Some (XML.Node (p, t, d)) -> `Opml (opml_of_xml' (p, t, d))
        | _ -> `Opml []
      end
  | _ -> `Opml []

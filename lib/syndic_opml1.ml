open Syndic_common.XML
open Syndic_common.Util
open Printf

module XML = Syndic_xml
module Error = Syndic_error
module Date = Syndic_date

type head =
  {
    title : string;
    date_created : Date.t option;
    date_modified : Date.t;
    owner_name : string;
    owner_email : string;
    expansion_state : int list;
    vert_scroll_state : int option;
    window_top : int option;
    window_left : int option;
    window_bottom : int option;
    window_right : int option;
  }

let head ?date_created ?(expansion_state=[]) ?vert_scroll_state
         ?window_top ?window_left ?window_bottom ?window_right
         ~date_modified ~owner_name ~owner_email title =
  { title;  date_created;  date_modified;  owner_name;  owner_email;
    expansion_state;  vert_scroll_state;  window_top;  window_left;
    window_bottom;  window_right }

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
  | `DateCreated of Date.t
  | `DateModified of Date.t
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
    "title", (fun a -> `Title (title_of_xml a));
    "dateCreated", (fun a -> `DateCreated (date_of_xml "<dateCreated>" a));
    "dateModified", (fun a -> `DateModified (date_of_xml "<dateModified>" a));
    "ownerName", (fun a -> `OwnerName (owner_name_of_xml a));
    "ownerEmail", (fun a -> `OwnerEmail (owner_email_of_xml a));
    "expansionState", (fun a -> `ExpansionState (expansion_state_of_xml a));
    "vertScrollState", (fun a -> `VertScrollState (vert_scroll_state_of_xml a));
    "windowTop", (fun a -> `WindowTop (window_top_of_xml a));
    "windowLeft", (fun a -> `WindowLeft (window_left_of_xml a));
    "windowBottom", (fun a -> `WindowBottom (window_bottom_of_xml a));
    "windowRight", (fun a -> `WindowRight (window_right_of_xml a))
    ] in
  generate_catcher
    ~data_producer
    make_head

let head_of_xml' =
  let data_producer = [
    "title", dummy_of_xml ~ctor:(fun a -> `Title a);
    "dateCreated", dummy_of_xml ~ctor:(fun a -> `DateCreated a);
    "dateModified", dummy_of_xml ~ctor:(fun a -> `DateModified a);
    "ownerName", dummy_of_xml ~ctor:(fun a -> `OwnerName a);
    "ownerEmail", dummy_of_xml ~ctor:(fun a -> `OwnerEmail a);
    "expansionState", dummy_of_xml ~ctor:(fun a -> `ExpansionSate a);
    "vertScrollState", dummy_of_xml ~ctor:(fun a -> `VertScrollState a);
    "windowTop", dummy_of_xml ~ctor:(fun a -> `WindowTop a);
    "windowLeft", dummy_of_xml ~ctor:(fun a -> `WindowLeft a);
    "windowBottom", dummy_of_xml ~ctor:(fun a -> `WindowBottom a);
    "windowRight", dummy_of_xml ~ctor:(fun a -> `WindowRight a)
  ] in
  generate_catcher
    ~data_producer
    (fun ~pos x -> x)

type outline =
  {
    text : string;
    typ : string option;
    is_comment : bool; (* see common attributes *)
    is_breakpoint : bool; (* see common attributes *)
    xml_url : Uri.t option;
    html_url : Uri.t option;
    attrs : (string * string) list;
    outlines : outline list
  }

let outline ?typ ?(is_comment=false) ?(is_breakpoint=false) ?xml_url ?html_url
            ?(attrs=[]) ?(outlines=[]) text =
  { text;  typ;  is_comment;  is_breakpoint;  xml_url;  html_url;
    attrs;  outlines }

let rec outline_of_xml (pos, ((_outline, attributes): Xmlm.tag), datas) =
  let text = ref ""
  and typ = ref None
  and is_comment = ref false
  and is_breakpoint = ref false
  and xml_url = ref None
  and html_url = ref None
  and attrs = ref []
  and outlines = ref [] in
  let process_attrs ((_, name), v) = match name with
    | "text" -> text := v
    | "type" -> typ := Some v
    | "isComment" ->
       (try is_comment := bool_of_string v
        with _ -> raise(Error.Error (pos, "<isComment> must have true or \
                                          false value.")))
    | "isBreakpoint" ->
       (try is_breakpoint := bool_of_string v
        with _ -> raise (Error.Error (pos, "<isBreakpoint> must have true \
                                           or false value.")))
    | "xmlUrl" ->
       (try xml_url := Some(Uri.of_string v)
        with _ -> raise(Error.Error(pos, "<xmlUrl> content must be an URL")))
    | "htmlUrl" ->
       (try html_url := Some(Uri.of_string v)
        with _ -> raise(Error.Error(pos, "<htmlUrl> content must be an URL")))
    | _ ->
       attrs := (name, v) :: !attrs in
  List.iter process_attrs attributes;
  let process_outlines = function
    | XML.Node (p, (((ns, name), _) as t), d) ->
       if ns = "" && name = "outline" then
         outlines := outline_of_xml (p, t, d) :: !outlines
    | XML.Data _ -> () in
  List.iter process_outlines datas;
  { text = !text;
    typ = !typ;
    is_comment = !is_comment;
    is_breakpoint = !is_breakpoint;
    xml_url = !xml_url;
    html_url = !html_url;
    attrs = !attrs;
    outlines = !outlines;
  }

let rec outline_of_xml' (pos, ((_outline, attributes): Xmlm.tag), datas) =
  let el_of_attrs ((_, name), v) = match name with
    | "text" -> `Text v
    | "type" -> `Type v
    | "isComment" -> `IsComment v
    | "isBreakpoint" -> `IsBreakpoint v
    | "xmlUrl" -> `XML_url v
    | "htmlUrl" -> `HTML_url v
    | _ -> `Attr (name, v) in
  let el = ref (List.map el_of_attrs attributes) in
  let process_outlines = function
    | XML.Node (p, (((ns, name), _) as t), d) ->
       if ns = "" && name = "outline" then
         el := `Outline(outline_of_xml' (p, t, d)) :: !el
    | XML.Data _ -> () in
  List.iter process_outlines datas;
  !el

type body = outline list

type body' = [`Outline of outline]

let make_body ~pos (l : [< body'] list) =
  let l =
    List.map (function `Outline o -> o) l
    |> List.rev
  in
  if List.length l <> 0 then l
  else raise (Error.Error (pos, "Body must contains one <outline> element."))

let body_of_xml =
  let data_producer = [
    "outline", (fun a -> (`Outline (outline_of_xml a)))
  ] in
  generate_catcher ~data_producer make_body

let body_of_xml' =
  let data_producer = [
    "outline", (fun a -> (`Outline (outline_of_xml' a)))
  ] in
  generate_catcher
    ~data_producer
    (fun ~pos x -> x)

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

let opml_of_xml =
  let attr_producer = [
    "version", (fun a -> `Version a)
  ] in
  let data_producer = [
    "head", (fun a -> `Head (head_of_xml a));
    "body", (fun a -> `Body (body_of_xml a))
  ] in
  generate_catcher
    ~attr_producer
    ~data_producer
    make_opml

let opml_of_xml' =
  let attr_producer = [
    "version", (fun a -> `Version a)
  ] in
  let data_producer = [
    "head", (fun a -> `Head (head_of_xml' a));
    "body", (fun a -> `Body (body_of_xml' a))
  ] in
  generate_catcher
    ~attr_producer
    ~data_producer
    (fun ~pos x -> x)

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


(* Output functions *)

(* Names have have no namespace.  This shortcut makes it more readable. *)
let n x = ("", x)

let node name sub = XML.Node(dummy_pos, (n name, []), sub)
let data d = XML.Data(dummy_pos, d)

let add_node name opt to_string xml =
  match opt with
  | None -> xml
  | Some d -> node name [data(to_string d)] :: xml

let head_to_xml h =
  let xml =
    add_node "windowRight"     h.window_right  string_of_int []
    |> add_node "windowBottom" h.window_bottom string_of_int
    |> add_node "windowLeft"   h.window_left   string_of_int
    |> add_node "windowTop"    h.window_top    string_of_int
    |> add_node "vertScrollState" h.vert_scroll_state string_of_int
    |> (fun x -> let c = List.map string_of_int h.expansion_state in
              node "expansionState" [data(String.concat "," c)]  :: x)
    |> add_node "dateCreated" h.date_created Date.to_rfc822 in
  node "title" [data h.title]
  :: node "dateModified" [data (Date.to_rfc822 h.date_modified)]
  :: node "ownerName" [data h.owner_name]
  :: node "ownerEmail" [data h.owner_email]
  :: xml

let add_attr name opt to_string attr =
  match opt with
  | None -> attr
  | Some d -> (n name, to_string d) :: attr

let id_string (s: string) = s

let rec outline_to_xml o =
  let attr =
    [(n "text", o.text);
     (n "isComment", o.is_comment |> string_of_bool);
     (n "isBreakpoint", o.is_breakpoint |> string_of_bool) ]
    |> add_attr "type" o.typ id_string in
  XML.Node(dummy_pos, (n "outline", attr),
           List.map outline_to_xml o.outlines)


let to_xml (o: opml) =
  XML.Node(dummy_pos, (n"opml", [n"version", o.version]),
           [node "head" (head_to_xml o.head);
            node "body" (List.map outline_to_xml o.body)  ])

let output opml dest =
  let o = Xmlm.make_output dest ~decl:true in
  XML.to_xmlm (to_xml opml) o

open Syndic_common.XML
open Syndic_common.Util
open Printf
module XML = Syndic_xml
module Error = Syndic_error
module Date = Syndic_date

type head =
  { title: string
  ; date_created: Date.t option
  ; date_modified: Date.t
  ; owner_name: string
  ; owner_email: string
  ; expansion_state: int list
  ; vert_scroll_state: int option
  ; window_top: int option
  ; window_left: int option
  ; window_bottom: int option
  ; window_right: int option }

let head ?date_created ?(expansion_state = []) ?vert_scroll_state ?window_top
    ?window_left ?window_bottom ?window_right ~date_modified ~owner_name
    ~owner_email title =
  { title
  ; date_created
  ; date_modified
  ; owner_name
  ; owner_email
  ; expansion_state
  ; vert_scroll_state
  ; window_top
  ; window_left
  ; window_bottom
  ; window_right }

let string_of_xml name (pos, _, datas) =
  try get_leaf datas with Not_found ->
    raise (Error.Error (pos, name ^ " must not be empty"))

let title_of_xml ~xmlbase:_ a = `Title (string_of_xml "<title>" a)
let owner_name_of_xml ~xmlbase:_ a = `OwnerName (string_of_xml "<ownerName>" a)

let owner_email_of_xml ~xmlbase:_ a =
  `OwnerEmail (string_of_xml "<ownerEmail>" a)

let expansion_state_of_xml ~xmlbase:_ (pos, _, datas) =
  let explode s =
    let rec aux acc i =
      if i = String.length s then acc else aux (s.[i] :: acc) (succ i)
    in
    aux [] 0 |> List.rev
  in
  let implode l =
    let rec aux s = function
      | x :: xs -> aux (s ^ Char.escaped x) xs
      | [] -> s
    in
    aux "" l
  in
  let split sep s =
    let rec aux acc_char acc = function
      | x :: xs when x = sep -> aux [] (List.rev acc_char :: acc) xs
      | x :: xs -> aux (x :: acc_char) acc xs
      | [] -> List.rev acc_char :: acc
    in
    explode s |> aux [] [] |> List.rev |> List.map implode
  in
  try
    `ExpansionState (get_leaf datas |> split ',' |> List.map int_of_string)
  with
  | Not_found -> `ExpansionState []
  | _ ->
      raise
        (Error.Error
           ( pos
           , "<expansionState> must be a list of numbers separated by commas \
              as 1,2,3" ))

let int_of_xml name (pos, _, datas) =
  try get_leaf datas |> int_of_string with
  | Not_found -> raise (Error.Error (pos, name ^ " must not be empty"))
  | Failure _ -> raise (Error.Error (pos, name ^ " must be an integer"))

let vert_scroll_state_of_xml ~xmlbase:_ a =
  `VertScrollState (int_of_xml "<vertScrollState>" a)

let window_top_of_xml ~xmlbase:_ a = `WindowTop (int_of_xml "<windowTop>" a)
let window_left_of_xml ~xmlbase:_ a = `WindowLeft (int_of_xml "<windowLeft>" a)

let window_bottom_of_xml ~xmlbase:_ a =
  `WindowBottom (int_of_xml "<windowBotton>" a)

let window_right_of_xml ~xmlbase:_ a =
  `WindowRight (int_of_xml "<windowRight>" a)

type head' =
  [ `Title of string
  | `DateCreated of Date.t
  | `DateModified of Date.t
  | `OwnerName of string
  | `OwnerEmail of string
  | `ExpansionState of int list
  | `VertScrollState of int
  | `WindowTop of int
  | `WindowLeft of int
  | `WindowBottom of int
  | `WindowRight of int ]

let make_head ~pos (l : [< head'] list) =
  let title =
    match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title s) -> s
    | _ ->
        raise
          (Error.Error (pos, "<head> MUST contains exactly one <title> element"))
  in
  let date_created =
    match find (function `DateCreated _ -> true | _ -> false) l with
    | Some (`DateCreated d) -> Some d
    | _ -> None
  in
  let date_modified =
    match find (function `DateModified _ -> true | _ -> false) l with
    | Some (`DateModified d) -> d
    | _ ->
        raise
          (Error.Error
             (pos, "<head> MUST contains exactly one <dateModified> element"))
  in
  let owner_name =
    match find (function `OwnerName _ -> true | _ -> false) l with
    | Some (`OwnerName s) -> s
    | _ ->
        raise
          (Error.Error
             (pos, "<head> MUST contains exactly one <ownerName> element"))
  in
  let owner_email =
    match find (function `OwnerEmail _ -> true | _ -> false) l with
    | Some (`OwnerEmail s) -> s
    | _ ->
        raise
          (Error.Error
             (pos, "<head> MUST contains exactly one <ownerEmail> element"))
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
  `Head
    { title
    ; date_created
    ; date_modified
    ; owner_name
    ; owner_email
    ; expansion_state
    ; vert_scroll_state
    ; window_top
    ; window_left
    ; window_bottom
    ; window_right }

let date_of_xml name (pos, _, datas) =
  let d =
    try get_leaf datas with Not_found ->
      raise (Error.Error (pos, name ^ " must not be empty"))
  in
  try Date.of_rfc822 d with _ ->
    raise (Error.Error (pos, sprintf "Date %S incorrect" d))

let date_created_of_xml ~xmlbase:_ a =
  `DateCreated (date_of_xml "<dateCreated>" a)

let date_modified_of_xml ~xmlbase:_ a =
  `DateModified (date_of_xml "<dateModified>" a)

let head_of_xml =
  let data_producer =
    [ ("title", title_of_xml)
    ; ("dateCreated", date_created_of_xml)
    ; ("dateModified", date_modified_of_xml)
    ; ("ownerName", owner_name_of_xml)
    ; ("ownerEmail", owner_email_of_xml)
    ; ("expansionState", expansion_state_of_xml)
    ; ("vertScrollState", vert_scroll_state_of_xml)
    ; ("windowTop", window_top_of_xml)
    ; ("windowLeft", window_left_of_xml)
    ; ("windowBottom", window_bottom_of_xml)
    ; ("windowRight", window_right_of_xml) ]
  in
  generate_catcher ~data_producer make_head

let head_of_xml' =
  let data_producer =
    [ ("title", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Title a))
    ; ("dateCreated", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `DateCreated a))
    ; ("dateModified", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `DateModified a))
    ; ("ownerName", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `OwnerName a))
    ; ("ownerEmail", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `OwnerEmail a))
    ; ( "expansionState"
      , dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `ExpansionSate a) )
    ; ( "vertScrollState"
      , dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `VertScrollState a) )
    ; ("windowTop", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `WindowTop a))
    ; ("windowLeft", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `WindowLeft a))
    ; ("windowBottom", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `WindowBottom a))
    ; ("windowRight", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `WindowRight a))
    ]
  in
  generate_catcher ~data_producer (fun ~pos:_ x -> `Head x)

type outline =
  { text: string
  ; typ: string option
  ; is_comment: bool
  ; (* see common attributes *)
    is_breakpoint: bool
  ; (* see common attributes *)
    xml_url: Uri.t option
  ; html_url: Uri.t option
  ; attrs: Xmlm.attribute list
  ; outlines: outline list }

let outline ?typ ?(is_comment = false) ?(is_breakpoint = false) ?xml_url
    ?html_url ?(attrs = []) ?(outlines = []) text =
  {text; typ; is_comment; is_breakpoint; xml_url; html_url; attrs; outlines}

let rec outline_of_node ~xmlbase ((pos, (_, attributes), datas) : node) =
  let text = ref ""
  and typ = ref None
  and is_comment = ref false
  and is_breakpoint = ref false
  and xml_url = ref None
  and html_url = ref None
  and attrs = ref []
  and outlines = ref [] in
  (* Get xml:base first as it must be used the these attributes too. *)
  let xmlbase = xmlbase_of_attr ~xmlbase attributes in
  let process_attrs ((name, v) as attr) =
    match name with
    | _, "text" -> text := v
    | _, "type" -> typ := Some v
    | _, "isComment" -> (
      try is_comment := bool_of_string v with _ ->
        raise (Error.Error (pos, "<isComment> must have true or false value."))
      )
    | _, "isBreakpoint" -> (
      try is_breakpoint := bool_of_string v with _ ->
        raise
          (Error.Error (pos, "<isBreakpoint> must have true or false value."))
      )
    | _, "xmlUrl" -> (
      try xml_url := Some (XML.resolve ~xmlbase (Uri.of_string v)) with _ ->
        raise (Error.Error (pos, "<xmlUrl> content must be an URL")) )
    | _, "htmlUrl" -> (
      try html_url := Some (XML.resolve ~xmlbase (Uri.of_string v)) with _ ->
        raise (Error.Error (pos, "<htmlUrl> content must be an URL")) )
    | _ -> attrs := attr :: !attrs
  in
  List.iter process_attrs attributes ;
  let process_outlines = function
    | XML.Node (p, (((ns, name), _) as t), d) ->
        if ns = "" && name = "outline" then
          outlines := outline_of_node ~xmlbase (p, t, d) :: !outlines
    | XML.Data _ -> ()
  in
  List.iter process_outlines datas ;
  { text= !text
  ; typ= !typ
  ; is_comment= !is_comment
  ; is_breakpoint= !is_breakpoint
  ; xml_url= !xml_url
  ; html_url= !html_url
  ; attrs= !attrs
  ; outlines= !outlines }

let outline_of_xml ~xmlbase a = `Outline (outline_of_node ~xmlbase a)

let rec outline_of_node' ~xmlbase ((_pos, (_, attributes), datas) : node) =
  let el = ref [] in
  let xmlbase = xmlbase_of_attr ~xmlbase attributes in
  let el_of_attrs (name, v) =
    match name with
    | _, "text" -> el := `Text v :: !el
    | _, "type" -> el := `Type v :: !el
    | _, "isComment" -> el := `IsComment v :: !el
    | _, "isBreakpoint" -> el := `IsBreakpoint v :: !el
    | _, "xmlUrl" -> el := `XML_url (xmlbase, v) :: !el
    | _, "htmlUrl" -> el := `HTML_url (xmlbase, v) :: !el
    | _, name -> el := `Attr (name, v) :: !el
  in
  List.iter el_of_attrs attributes ;
  let process_outlines = function
    | XML.Node (p, (((ns, name), _) as t), d) ->
        if ns = "" && name = "outline" then
          el := `Outline (outline_of_node' ~xmlbase (p, t, d)) :: !el
    | XML.Data _ -> ()
  in
  List.iter process_outlines datas ;
  !el

let outline_of_xml' ~xmlbase a = `Outline (outline_of_node' ~xmlbase a)

type body = outline list
type body' = [`Outline of outline]

let make_body ~pos (l : [< body'] list) =
  let l = List.map (function `Outline o -> o) l |> List.rev in
  if List.length l <> 0 then `Body l
  else raise (Error.Error (pos, "Body must contains one <outline> element."))

let body_of_xml =
  let data_producer = [("outline", outline_of_xml)] in
  generate_catcher ~data_producer make_body

let body_of_xml' =
  let data_producer = [("outline", outline_of_xml')] in
  generate_catcher ~data_producer (fun ~pos:_ x -> `Body x)

type t = {version: string; head: head; body: body}
type opml = t

(* FIXME: @deprecated *)

type opml' = [`Version of string | `Head of head | `Body of body]

let make_opml ~pos (l : [< opml'] list) =
  let version =
    match find (function `Version _ -> true | _ -> false) l with
    | Some (`Version v) -> v
    | _ -> raise (Error.Error (pos, "Opml tag must have <version> attribut"))
  in
  let head =
    match find (function `Head _ -> true | _ -> false) l with
    | Some (`Head h) -> h
    | _ ->
        raise
          (Error.Error (pos, "Opml tag must have exactly one <head> element"))
  in
  let body =
    match find (function `Body _ -> true | _ -> false) l with
    | Some (`Body b) -> b
    | _ ->
        raise
          (Error.Error (pos, "Opml tag must have exactly one <body> element"))
  in
  {version; head; body}

let opml_of_xml =
  let attr_producer = [("version", fun ~xmlbase:_ a -> `Version a)] in
  let data_producer = [("head", head_of_xml); ("body", body_of_xml)] in
  generate_catcher ~attr_producer ~data_producer make_opml

let opml_of_xml' =
  let attr_producer = [("version", fun ~xmlbase:_ a -> `Version a)] in
  let data_producer = [("head", head_of_xml'); ("body", body_of_xml')] in
  generate_catcher ~attr_producer ~data_producer (fun ~pos:_ x -> x)

let find_opml l =
  find (function XML.Node (_, t, _) -> tag_is t "opml" | _ -> false) l

let parse ?xmlbase input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, data) -> (
      if tag_is tag "opml" then opml_of_xml ~xmlbase (pos, tag, data)
      else
        match find_opml data with
        | Some (XML.Node (p, t, d)) -> opml_of_xml ~xmlbase (p, t, d)
        | _ ->
            raise
              (Error.Error
                 ((0, 0), "document MUST contains exactly one <opml> element"))
      )
  | _ ->
      raise
        (Error.Error
           ((0, 0), "document MUST contains exactly one <opml> element"))

let read ?xmlbase fname =
  let fh = open_in fname in
  try
    let x = parse ?xmlbase (XML.input_of_channel fh) in
    close_in fh ; x
  with e -> close_in fh ; raise e

type uri = Uri.t option * string

let unsafe ?xmlbase input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, data) -> (
      if tag_is tag "opml" then `Opml (opml_of_xml' ~xmlbase (pos, tag, data))
      else
        match find_opml data with
        | Some (XML.Node (p, t, d)) -> `Opml (opml_of_xml' ~xmlbase (p, t, d))
        | _ -> `Opml [] )
  | _ -> `Opml []

(* Output functions *)

(* Names have have no namespace. This shortcut makes it more readable. *)
let n x = ("", x)
let node name sub = XML.Node (dummy_pos, (n name, []), sub)
let data d = XML.Data (dummy_pos, d)

let add_node name opt to_string xml =
  match opt with
  | None -> xml
  | Some d -> node name [data (to_string d)] :: xml

let head_to_xml h =
  let xml =
    add_node "windowRight" h.window_right string_of_int []
    |> add_node "windowBottom" h.window_bottom string_of_int
    |> add_node "windowLeft" h.window_left string_of_int
    |> add_node "windowTop" h.window_top string_of_int
    |> add_node "vertScrollState" h.vert_scroll_state string_of_int
    |> (fun x ->
         let c = List.map string_of_int h.expansion_state in
         node "expansionState" [data (String.concat "," c)] :: x )
    |> add_node "dateCreated" h.date_created Date.to_rfc822
  in
  node "title" [data h.title]
  :: node "dateModified" [data (Date.to_rfc822 h.date_modified)]
  :: node "ownerName" [data h.owner_name]
  :: node "ownerEmail" [data h.owner_email]
  :: xml

let add_attr name opt to_string attr =
  match opt with None -> attr | Some d -> (n name, to_string d) :: attr

let id_string (s : string) = s

let rec outline_to_xml o =
  (* isComment and isBreakpoint: absent <=> false *)
  let attr =
    if o.is_comment then (n "isComment", "true") :: o.attrs else o.attrs
  in
  let attr =
    if o.is_breakpoint then (n "isBreakpoint", "true") :: attr else attr
  in
  let attr =
    (n "text", o.text) :: attr
    |> add_attr "type" o.typ id_string
    |> add_attr "xmlUrl" o.xml_url Uri.to_string
    |> add_attr "htmlUrl" o.html_url Uri.to_string
  in
  XML.Node (dummy_pos, (n "outline", attr), List.map outline_to_xml o.outlines)

let to_xml (o : t) =
  XML.Node
    ( dummy_pos
    , (n "opml", [(n "version", o.version)])
    , [ node "head" (head_to_xml o.head)
      ; node "body" (List.map outline_to_xml o.body) ] )

let output opml dest = XML.to_xmlm (to_xml opml) (XML.make_output dest)

let write opml fname =
  let fh = open_out fname in
  try
    output opml (`Channel fh) ;
    close_out fh
  with e -> close_out fh ; raise e

(* Creation from atom feeds *)

(* Remove all tags *)
let rec add_to_buffer buf = function
  | XML.Node (_, _, subs) -> List.iter (add_to_buffer buf) subs
  | XML.Data (_, d) -> Buffer.add_string buf d

let xhtml_to_string ~buf xhtml =
  Buffer.clear buf ;
  List.iter (add_to_buffer buf) xhtml ;
  Buffer.contents buf

let string_of_text_construct ~buf = function
  (* FIXME: Once we use a proper HTML library, we probably would like to parse
     the HTML and remove the tags *)
  | (Syndic_atom.Text s : Syndic_atom.text_construct) | Syndic_atom.Html (_, s)
    ->
      s
  | Syndic_atom.Xhtml (_, x) -> xhtml_to_string ~buf x

let rec first_non_empty = function
  | a :: tl ->
      if a.Syndic_atom.name = "" then first_non_empty tl
      else a.Syndic_atom.name
  | [] -> ""

let outine_of_feed ~buf (f : Syndic_atom.feed) =
  let open Syndic_atom in
  let author =
    match f.authors with
    | _ :: _ -> first_non_empty f.authors
    | [] -> (
      match f.entries with
      | e :: _ ->
          let a0, a = e.authors in
          if a0.name = "" then first_non_empty a else a0.name
      | [] -> "" )
  in
  let title = string_of_text_construct ~buf f.title in
  let xml_url, is_comment =
    try
      let l = List.find (fun l -> l.rel = Self) f.links in
      let is_comment =
        match l.length with Some len -> len < 0 | None -> false
      in
      (Some l.href, is_comment)
    with Not_found -> (None, false)
  in
  outline ~typ:"rss" ~is_comment
    ~attrs:[(("", "title"), title)]
    ?xml_url author

let of_atom ~head feeds =
  let buf = Buffer.create 1024 in
  {version= "1.1"; head; body= List.map (outine_of_feed ~buf) feeds}

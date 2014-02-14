let input = Xmlm.make_input (`Channel stdin)

type tree =
  | Node of Xmlm.tag * tree list
  | Leaf of string

type state =
  | Root
  | Entry

type rel =
  | Alternate
  | Related
  | Self
  | Enclosure
  | Via
  | Link of Neturl.url

type ty_content =
  | Html
  | Text
  | Xhtml

type content = { ty: ty_content; src: string option; data: string; }

type author = {
  name: string;
  uri: Neturl.url option;
  email: string option;
}

type category = {
  term: string;
  scheme: Neturl.url option;
  label: string option;
}

type generator = {
  content: string;
  version: string option;
  uri: Neturl.url option;
}

type link = {
  href: Neturl.url; (* iri *)
  rel: rel;
  type_media: string option;
  hreflang: string option;
  title: string option;
  length: int option;
}

type feed = {
  (*
   * si atom:entry ne contient pas atom:author,
   * atom:author <- atom:source/atom:author
   * sinon erreur
   *)
  author: string;
  category: category list;
  contributor: string list;
  generator: (generator * string) option;
  icon: Neturl.url option; (* iri *)
  id: Neturl.url; (* iri *)
  link: link * link list; (* link self * other link *)
  logo: Neturl.url option (* iri *);
  published: string option (* date *);
  rights: string option;
  subtitle: string option;
  content: content option;
  (*
   * atom:summary obligatoire si atom:entry contient atom:content avec
   * attribut src ou atom:entry codé en base64 (LOL)
   *)
  summary: string option;
  title: string;
  updated: string (* date *);
}

type incomplete_feed = [
  | `Author of string
  | `Category of category list
  | `Contributor of string list
  | `Generator of (generator * string)
  | `Icon of Neturl.url
  | `Id of Neturl.url
  | `Link of link list
  | `Logo of Neturl.url
  | `Published of string
  | `Rights of string
  | `Subtitle of string
  | `Content of content
  | `Summary of string
  | `Title of string
  | `Updated of string
]

type context = {
  state: state;
  input: Xmlm.input;
}

type opts_neturl = {
  schemes: (string, Neturl.url_syntax) Hashtbl.t;
  base_syntax: Neturl.url_syntax;
  accept_8bits: bool;
  enable_fragment: bool;
}

(* Exception *)

exception Malformed_URL of string
exception ExpectedLeaf
exception ExpectedAuthorName
exception ExpectedCategoryTerm
exception ExpectedGeneratorContent
exception ExpectedIconURI
exception ExpectedIdURI
exception ExpectedLinkHREF
exception ExpectedLogoURI

(* Util *)

let find f l = try Some (List.find f l) with Not_found -> None

let url_of_string opts_neturl str =
  try Neturl.parse_url
    ~schemes:opts_neturl.schemes
    ~base_syntax:opts_neturl.base_syntax
    ~accept_8bits:opts_neturl.accept_8bits
    ~enable_fragment:opts_neturl.enable_fragment
    str
  with Neturl.Malformed_URL -> raise (Malformed_URL str)

let tag_is ((prefix, name), attrs) = (=) name
let attr_is ((prefix, name), value) = (=) name
let datas_has_leaf = List.exists (function | Leaf _ -> true | _ -> false)
let get_leaf l = match find (function Leaf _ -> true | _ -> false) l with
  | Some (Leaf s) -> s
  | _ -> raise ExpectedLeaf
let get_attr (_, attrs) = attrs
let get_value (_, value) = value
let get_attr_name ((prefix, name), _) = name

let make_context ?enc ?strip ?ns ?entity input =
  { state = Root; input = Xmlm.make_input input; }

let make_opts_neturl
  ?(schemes = Neturl.common_url_syntax)
  ?(base_syntax = Hashtbl.find Neturl.common_url_syntax "http")
  ?(accept_8bits = true)
  ?(enable_fragment = true) () =
{
  schemes;
  base_syntax;
  accept_8bits;
  enable_fragment;
}

let update_ctx_state { state; input; } new_state =
  { state = new_state; input; }

(* XML to string *)

let string_of_name (prefix, name) =
  let buffer = Buffer.create 16 in
  (* Buffer.add_string buffer prefix; *)
  (* Buffer.add_char buffer ':'; *)
  Buffer.add_string buffer name;
  Buffer.contents buffer

let string_of_attr (name, value) =
  let buffer = Buffer.create 16 in
  Buffer.add_string buffer (string_of_name name);
  Buffer.add_string buffer " = ";
  Buffer.add_string buffer value;
  Buffer.contents buffer

let string_of_tag (name, attrs) =
  let buffer = Buffer.create 16 in
  Buffer.add_char buffer '<';
  Buffer.add_string buffer (string_of_name name);
  if List.length attrs = 0  then () else Buffer.add_char buffer ' ';
  (match attrs with
    | [] -> ()
    | x :: r ->
      Buffer.add_string buffer (string_of_attr x);
      List.iter (fun x -> Buffer.add_char buffer ' '; Buffer.add_string buffer (string_of_attr x)) r);
  Buffer.add_char buffer '>';
  Buffer.contents buffer

let string_of_close_tag (name, _) =
  let buffer = Buffer.create 16 in
  Buffer.add_string buffer "</";
  Buffer.add_string buffer (string_of_name name);
  Buffer.add_char buffer '>';
  Buffer.contents buffer

let string_of_xml ctx =
  let el tag datas =
    let buffer = Buffer.create 16
    in
      Buffer.add_string buffer (string_of_tag tag);
      List.iter (fun x -> Buffer.add_string buffer x) datas;
      Buffer.add_string buffer (string_of_close_tag tag);
      Buffer.contents buffer
  in let data str = str
  in let (_, str) = Xmlm.input_doc_tree ~el ~data ctx.input
  in str

(* Produce XML *)

let generate_catcher attr_producer data_producer maker =
  let get_producer attr map =
    try Some (List.assoc (get_attr_name attr) map)
    with _ -> None
  in
  let rec catch_attr acc = function
    | attr :: r -> begin match get_producer attr attr_producer with
      | Some f -> catch_attr ((f attr) :: acc) r
      | None -> catch_attr acc r end
    | [] -> acc
  in
  let rec catch_data datas = match data_producer with
    | Some f ->
      if datas_has_leaf datas
      then [f (get_leaf datas)]
      else []
    | None -> []
  in
  let generate ctx (tag, datas) =
    maker (catch_attr (catch_data datas) (get_attr tag))
  in generate

(* RFC Compliant (or raise error) *)

let make_author (l : [< `AuthorName of string | `AuthorURI of Neturl.url | `AuthorEmail of string] list) =
  let name = match find (function `AuthorName _ -> true | _ -> false) l with
    | Some (`AuthorName s) -> s
    | _ -> raise ExpectedAuthorName
  in let uri = match find (function `AuthorURI _ -> true | _ -> false) l with
    | Some (`AuthorURI u) -> Some u
    | _ -> None
  in let email = match find (function `AuthorEmail _ -> true | _ -> false) l with
    | Some (`AuthorEmail e) -> Some e
    | _ -> None
  in { name; uri; email; }

let author_of_xml ctx (tag, datas) =
  let rec aux acc = function
    | (tag, datas) :: r when tag_is tag "name" && datas_has_leaf datas ->
      aux (`AuthorName (get_leaf datas) :: acc) r
    | (tag, datas) :: r when tag_is tag "uri" && datas_has_leaf datas ->
      aux (`AuthorURI (url_of_string (make_opts_neturl ()) (get_leaf datas)) :: acc) r
    | (tag, datas) :: r when tag_is tag "email" && datas_has_leaf datas ->
      aux (`AuthorEmail (get_leaf datas) :: acc) r
    | _ :: r -> aux acc r
    | [] -> acc
  in make_author (aux [] datas)

(* RFC Compliant (or raise error) *)

let make_category (l : [< `CategoryTerm of string | `CategoryScheme of Neturl.url | `CategoryLabel of string] list) =
  let term = match find (function `CategoryTerm _ -> true | _ -> false) l with
    | Some (`CategoryTerm t) -> t
    | _ -> raise ExpectedCategoryTerm
  in let scheme = match find (function `CategoryScheme _ -> true | _ -> false) l with
    | Some (`CategoryScheme u) -> Some u
    | _ -> None
  in let label = match find (function `CategoryLabel _ -> true | _ -> false) l with
    | Some (`CategoryLabel l) -> Some l
    | _ -> None
  in { term; scheme; label; }

let category_of_xml ctx (tag, datas) =
  let rec aux acc = function
    | attr :: r when attr_is attr "term" ->
      aux ((`CategoryTerm (get_value attr)) :: acc) r
    | attr :: r when attr_is attr "scheme" ->
      aux ((`CategoryScheme (url_of_string (make_opts_neturl ()) (get_value attr))) :: acc) r
    | attr :: r when attr_is attr "label" ->
      aux ((`CategoryLabel (get_value attr)) :: acc) r
    | _ :: r -> aux acc r
    | [] -> acc
  in make_category (aux [] (get_attr tag))

(* RFC Compliant (or raise error) *)

let make_contributor = make_author
let contributor_of_xml = author_of_xml

(* RFC Compliant (or raise error) *)

let make_generator (l : [< `GeneratorURI of Neturl.url | `GeneratorVersion of string | `GeneratorContent of string] list) =
  let content = match find (function `GeneratorContent _ -> true | _ -> false) l with
    | Some ((`GeneratorContent c)) -> c
    | _ -> raise ExpectedGeneratorContent
  in let version = match find (function `GeneratorVersion _ -> true | _ -> false) l with
    | Some ((`GeneratorVersion v)) -> Some v
    | _ -> None
  in let uri = match find (function `GeneratorURI _ -> true | _ -> false) l with
    | Some ((`GeneratorURI u)) -> Some u
    | _ -> None
  in { content; version; uri; }

let generator_of_xml ctx (tag, datas) =
  let rec catch_attr acc = function
    | attr :: r when attr_is attr "version" ->
      catch_attr ((`GeneratorVersion (get_value attr)) :: acc) r
    | attr :: r when attr_is attr "uri" ->
      catch_attr ((`GeneratorURI (url_of_string (make_opts_neturl ()) (get_value attr))) :: acc) r
    | _ :: r -> catch_attr acc r
    | [] -> acc
  in let catch_data = if datas_has_leaf datas then [`GeneratorContent (get_leaf datas)] else [] in
  make_generator (catch_attr catch_data (get_attr tag))

(* RFC Compliant (or raise error) *)

let make_icon (l : [< `IconURI of Neturl.url] list) =
  let uri = match find (function `IconURI _ -> true | _ -> false) l with
    | Some (`IconURI u) -> u
    | _ -> raise ExpectedIconURI
  in uri

let icon_of_xml ctx (tag, datas) =
  let catch_data =
    if datas_has_leaf datas
    then [`IconURI (url_of_string (make_opts_neturl ()) (get_leaf datas))]
    else []
  in make_icon catch_data

(* RFC Compliant (or raise error) *)

let make_id (l : [< `IdURI of Neturl.url] list) =
  let uri = match find (function `IdURI _ -> true | _ -> false) l with
    | Some (`IdURI u) -> u
    | _ -> raise ExpectedIdURI
  in uri

let id_of_xml ctx (tag, datas) =
  let catch_data =
    if datas_has_leaf datas
    then [`IdURI (url_of_string (make_opts_neturl ()) (get_leaf datas))]
    else []
  in make_id catch_data

(* RFC Compliant (or raise error) *)

let make_link (l : [< `LinkHREF of Neturl.url | `LinkRel of rel | `LinkType of string | `LinkHREFLang of string | `LinkTitle of string | `LinkLength of int] list) =
  let href = match find (function `LinkHREF _ -> true | _ -> false) l with
    | Some (`LinkHREF u) -> u
    | _ -> raise ExpectedLinkHREF
  in let rel = match find (function `LinkRel _ -> true | _ -> false) l with
    | Some (`LinkRel r) -> r
    | _ -> Alternate (* cf. RFC 4287 § 4.2.7.2 *)
  in let type_media = match find (function `LinkType _ -> true | _ -> false) l with
    | Some (`LinkType t) -> Some t
    | _ -> None
  in let hreflang = match find (function `LinkHREFLang _ -> true | _ -> false) l with
    | Some (`LinkHREFLang l) -> Some l
    | _ -> None
  in let title = match find (function `LinkTitle _ -> true | _ -> false) l with
    | Some (`LinkTitle s) -> Some s
    | _ -> None
  in let length = match find (function `LinkLength _ -> true | _ -> false) l with
    | Some (`LinkLength i) -> Some i
    | _ -> None
  in { href; rel; type_media; hreflang; title; length; }

let rel_of_string s = match String.lowercase (String.trim s) with
  | "alternate" -> Alternate
  | "related" -> Related
  | "self" -> Self
  | "enclosure" -> Enclosure
  | "via" -> Via
  | uri -> Link (url_of_string (make_opts_neturl ()) uri)

let link_of_xml' =
  let attr_producer = [
    ("href", (function attr -> `LinkHREF (url_of_string (make_opts_neturl ()) (get_value attr))));
    ("rel", (function attr -> `LinkRel (rel_of_string (get_value attr))));
    ("type", (function attr -> `LinkType (get_value attr)));
    ("hreflang", (function attr -> `LinkHREFLang (get_value attr)));
    ("title", (function attr -> `LinkTitle (get_value attr)));
    ("length", (function attr -> `LinkLength (int_of_string (get_value attr))));
  ] in
  generate_catcher attr_producer None make_link

let make_logo (l : [< `LogoURI of Neturl.url] list) =
  let uri = match find (function `LogoURI _ -> true | _ -> false) l with
    | Some (`LogoURI u) -> u
    | _ -> raise ExpectedLogoURI
  in uri

let logo_of_xml =
  let attr_producer = [] in
  let data_producer = Some (function data -> `LogoURI (url_of_string (make_opts_neturl ()) data)) in
  generate_catcher attr_producer data_producer make_logo

let rec analyze_tree ctx = function
  | Node (tag, datas) when ctx.state = Root && tag_is tag "entry" ->
    let new_ctx = update_ctx_state ctx Entry in
    List.iter (fun x -> analyze_tree new_ctx x) datas
  | Node (((_, name), _), datas) ->
    List.iter (fun x -> analyze_tree ctx x) datas
  | Leaf data -> ()

let produce_tree ctx =
  let el tag datas = Node (tag, datas) in
  let data data = Leaf data in
  let (_, tree) = Xmlm.input_doc_tree ~el ~data ctx.input
  in tree

let () = let ctx = make_context (`Channel stdin) in
  analyze_tree ctx (produce_tree ctx)

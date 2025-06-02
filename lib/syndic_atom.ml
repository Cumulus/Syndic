open Syndic_common.XML
open Syndic_common.Util
module XML = Syndic_xml
module Error = Syndic_error
module Date = Syndic_date

let atom_ns = "http://www.w3.org/2005/Atom"
let xhtml_ns = "http://www.w3.org/1999/xhtml"
let namespaces = [atom_ns]

type rel = Alternate | Related | Self | Enclosure | Via | Link of Uri.t

type link =
  { href: Uri.t
  ; rel: rel
  ; type_media: string option
  ; hreflang: string option
  ; title: string
  ; length: int option }

let link ?type_media ?hreflang ?(title = "") ?length ?(rel = Alternate) href =
  {href; rel; type_media; hreflang; title; length}

type link' =
  [ `HREF of Uri.t
  | `Rel of string
  | `Type of string
  | `HREFLang of string
  | `Title of string
  | `Length of string ]

(* The actual XML content is supposed to be inside a <div> which is NOT part of
   the content. *)
let rec get_xml_content xml0 = function
  | XML.Data (_, s) :: tl ->
      if only_whitespace s then get_xml_content xml0 tl
      else xml0 (* unexpected *)
  | XML.Node (_pos, tag, data) :: tl when tag_is tag "div" ->
      let is_space =
        List.for_all
          (function XML.Data (_, s) -> only_whitespace s | _ -> false)
          tl
      in
      if is_space then data else xml0
  | _ -> xml0

let no_namespace = Some ""
let rm_namespace _ = no_namespace

(* For HTML, the spec says the whole content needs to be escaped
   http://tools.ietf.org/html/rfc4287#section-3.1.1.2 (some feeds use <![CDATA[
   ]]>) so a single data item should be present. If not, assume the HTML was
   properly parsed and convert it back to a string as it should. *)
let get_html_content html =
  match html with
  | [XML.Data (_, d)] -> d
  | h ->
      (* It is likely that, when the HTML was parsed, the Atom namespace was
         applied. Remove it. *)
      String.concat "" (List.map (XML.to_string ~ns_prefix:rm_namespace) h)

type text_construct =
  | Text of string
  | Html of Uri.t option * string
  | Xhtml of Uri.t option * XML.t list

let text_construct_of_xml ~xmlbase
    ((_pos, (_tag, attr), data) : XML.pos * XML.tag * t list) =
  let xmlbase = xmlbase_of_attr ~xmlbase attr in
  match find (fun a -> attr_is a "type") attr with
  | Some (_, "html") -> Html (xmlbase, get_html_content data)
  | Some (_, "application/xhtml+xml") | Some (_, "xhtml") ->
      Xhtml (xmlbase, get_xml_content data data)
  | _ -> Text (get_leaf data)

type author = {name: string; uri: Uri.t option; email: string option}

let empty_author = {name= ""; uri= None; email= None}
let not_empty_author a = a.name <> "" || a.uri <> None || a.email <> None
let author ?uri ?email name = {uri; email; name}

type person' = [`Name of string | `URI of Uri.t | `Email of string]

let make_person datas ~pos:_ (l : [< person'] list) =
  (* element atom:name { text } *)
  let name =
    match find (function `Name _ -> true | _ -> false) l with
    | Some (`Name s) -> s
    | _ ->
        (* The spec mandates that <author><name>name</name></author> but
           several feeds just do <author>name</author> *)
        get_leaf datas
  in
  (* element atom:uri { atomUri }? *)
  let uri =
    match find (function `URI _ -> true | _ -> false) l with
    | Some (`URI u) -> Some u
    | _ -> None
  in
  (* element atom:email { atomEmailAddress }? *)
  let email =
    match find (function `Email _ -> true | _ -> false) l with
    | Some (`Email e) -> Some e
    | _ -> None
  in
  ({name; uri; email} : author)

let make_author datas ~pos a = `Author (make_person datas ~pos a)

let person_name_of_xml ~xmlbase:_ (_pos, _tag, datas) =
  `Name (try get_leaf datas with Not_found -> "")

(* mandatory ? *)

let person_uri_of_xml ~xmlbase (pos, _tag, datas) =
  try `URI (XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
  with Not_found ->
    raise
      (Error.Error (pos, "The content of <uri> MUST be a non-empty string"))

let person_email_of_xml ~xmlbase:_ (_pos, _tag, datas) =
  `Email (try get_leaf datas with Not_found -> "")

(* mandatory ? *)

(* {[ atomAuthor = element atom:author { atomPersonConstruct } ]} where

   atomPersonConstruct = atomCommonAttributes, (element atom:name { text } &
   element atom:uri { atomUri }? & element atom:email { atomEmailAddress }? &
   extensionElement * )

   This specification assigns no significance to the order of appearance of the
   child elements in a Person construct. *)
let person_data_producer =
  [ ("name", person_name_of_xml)
  ; ("uri", person_uri_of_xml)
  ; ("email", person_email_of_xml) ]

let author_of_xml ~xmlbase ((_, _, datas) as xml) =
  generate_catcher ~namespaces ~data_producer:person_data_producer
    (make_author datas) ~xmlbase xml

type uri = Uri.t option * string
type person = [`Email of string | `Name of string | `URI of uri] list

let person_data_producer' =
  [ ("name", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Name a))
  ; ("uri", dummy_of_xml ~ctor:(fun ~xmlbase a -> `URI (xmlbase, a)))
  ; ("email", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Email a)) ]

let author_of_xml' =
  generate_catcher ~namespaces ~data_producer:person_data_producer'
    (fun ~pos:_ x -> `Author x )

type category = {term: string; scheme: Uri.t option; label: string option}

let category ?scheme ?label term = {scheme; label; term}

type category' = [`Term of string | `Scheme of Uri.t | `Label of string]

let make_category ~pos (l : [< category'] list) =
  (* attribute term { text } *)
  let term =
    match find (function `Term _ -> true | _ -> false) l with
    | Some (`Term t) -> t
    | _ ->
        raise
          (Error.Error (pos, "Category elements MUST have a 'term' attribute"))
  in
  (* attribute scheme { atomUri }? *)
  let scheme =
    match find (function `Scheme _ -> true | _ -> false) l with
    | Some (`Scheme u) -> Some u
    | _ -> None
  in
  (* attribute label { text }? *)
  let label =
    match find (function `Label _ -> true | _ -> false) l with
    | Some (`Label l) -> Some l
    | _ -> None
  in
  `Category ({term; scheme; label} : category)

let scheme_of_xml ~xmlbase a = `Scheme (XML.resolve ~xmlbase (Uri.of_string a))

(* atomCategory = element atom:category { atomCommonAttributes, attribute term
   { text }, attribute scheme { atomUri }?, attribute label { text }?,
   undefinedContent } *)
let category_attr_producer =
  [ ("term", fun ~xmlbase:_ a -> `Term a)
  ; ("label", fun ~xmlbase:_ a -> `Label a) ]

let category_of_xml =
  let attr_producer = ("scheme", scheme_of_xml) :: category_attr_producer in
  generate_catcher ~attr_producer make_category

let category_of_xml' =
  let attr_producer =
    ("scheme", fun ~xmlbase:_ a -> `Scheme a) :: category_attr_producer
  in
  generate_catcher ~attr_producer (fun ~pos:_ x -> `Category x)

let make_contributor datas ~pos a = `Contributor (make_person datas ~pos a)

let contributor_of_xml ~xmlbase ((_, _, datas) as xml) =
  generate_catcher ~namespaces ~data_producer:person_data_producer
    (make_contributor datas) ~xmlbase xml

let contributor_of_xml' =
  generate_catcher ~namespaces ~data_producer:person_data_producer'
    (fun ~pos:_ x -> `Contributor x )

type generator = {version: string option; uri: Uri.t option; content: string}

let generator ?uri ?version content = {uri; version; content}

type generator' = [`URI of Uri.t | `Version of string | `Content of string]

let make_generator ~pos (l : [< generator'] list) =
  (* text *)
  let content =
    match find (function `Content _ -> true | _ -> false) l with
    | Some (`Content c) -> c
    | _ ->
        raise
          (Error.Error
             (pos, "The content of <generator> MUST be a non-empty string"))
  in
  (* attribute version { text }? *)
  let version =
    match find (function `Version _ -> true | _ -> false) l with
    | Some (`Version v) -> Some v
    | _ -> None
  in
  (* attribute uri { atomUri }? *)
  let uri =
    match find (function `URI _ -> true | _ -> false) l with
    | Some (`URI u) -> Some u
    | _ -> None
  in
  `Generator ({version; uri; content} : generator)

(* URI, if present, MUST be an IRI reference [RFC3987]. The definition of "IRI"
   excludes relative references but we resolve it anyway in case this is not
   respected by the generator. *)
let generator_uri_of_xml ~xmlbase a =
  `URI (XML.resolve ~xmlbase (Uri.of_string a))

(* atomGenerator = element atom:generator { atomCommonAttributes, attribute uri
   { atomUri }?, attribute version { text }?, text } *)
let generator_of_xml =
  let attr_producer =
    [("version", fun ~xmlbase:_ a -> `Version a); ("uri", generator_uri_of_xml)]
  in
  let leaf_producer ~xmlbase:_ _pos data = `Content data in
  generate_catcher ~attr_producer ~leaf_producer make_generator

let generator_of_xml' =
  let attr_producer =
    [ ("version", fun ~xmlbase:_ a -> `Version a)
    ; ("uri", fun ~xmlbase a -> `URI (xmlbase, a)) ]
  in
  let leaf_producer ~xmlbase:_ _pos data = `Content data in
  generate_catcher ~attr_producer ~leaf_producer (fun ~pos:_ x -> `Generator x)

type icon = Uri.t

let make_icon ~pos (l : Uri.t list) =
  (* (atomUri) *)
  let uri =
    match l with
    | u :: _ -> u
    | [] ->
        raise
          (Error.Error (pos, "The content of <icon> MUST be a non-empty string"))
  in
  `Icon uri

(* atomIcon = element atom:icon { atomCommonAttributes, } *)
let icon_of_xml =
  let leaf_producer ~xmlbase _pos data =
    XML.resolve ~xmlbase (Uri.of_string data)
  in
  generate_catcher ~leaf_producer make_icon

let icon_of_xml' =
  let leaf_producer ~xmlbase _pos data = `URI (xmlbase, data) in
  generate_catcher ~leaf_producer (fun ~pos:_ x -> `Icon x)

type id = Uri.t

let make_id ~pos (l : string list) =
  (* (atomUri) *)
  let id =
    match l with
    | u :: _ -> Uri.of_string u
    | [] ->
        raise
          (Error.Error (pos, "The content of <id> MUST be a non-empty string"))
  in
  `ID id

(* atomId = element atom:id { atomCommonAttributes, (atomUri) } *)
let id_of_xml, id_of_xml' =
  let leaf_producer ~xmlbase:_ _pos data = data in
  ( generate_catcher ~leaf_producer make_id
  , generate_catcher ~leaf_producer (fun ~pos:_ x -> `ID x) )

let rel_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "alternate" -> Alternate
  | "related" -> Related
  | "self" -> Self
  | "enclosure" -> Enclosure
  | "via" -> Via
  | uri ->
      (* RFC 4287 § 4.2.7.2: the use of a relative reference other than a
         simple name is not allowed. Thus no need to resolve against xml:base. *)
      Link (Uri.of_string uri)

let make_link ~pos (l : [< link'] list) =
  (* attribute href { atomUri } *)
  let href =
    match find (function `HREF _ -> true | _ -> false) l with
    | Some (`HREF u) -> u
    | _ ->
        raise (Error.Error (pos, "Link elements MUST have a 'href' attribute"))
  in
  (* attribute rel { atomNCName | atomUri }? *)
  let rel =
    match find (function `Rel _ -> true | _ -> false) l with
    | Some (`Rel r) -> rel_of_string r
    | _ -> Alternate
    (* cf. RFC 4287 § 4.2.7.2 *)
  in
  (* attribute type { atomMediaType }? *)
  let type_media =
    match find (function `Type _ -> true | _ -> false) l with
    | Some (`Type t) -> Some t
    | _ -> None
  in
  (* attribute hreflang { atomLanguageTag }? *)
  let hreflang =
    match find (function `HREFLang _ -> true | _ -> false) l with
    | Some (`HREFLang l) -> Some l
    | _ -> None
  in
  (* attribute title { text }? *)
  let title =
    match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title s) -> s
    | _ -> ""
  in
  (* attribute length { text }? *)
  let length =
    match find (function `Length _ -> true | _ -> false) l with
    | Some (`Length i) -> Some (int_of_string i)
    | _ -> None
  in
  `Link ({href; rel; type_media; hreflang; title; length} : link)

let link_href_of_xml ~xmlbase a =
  `HREF (XML.resolve ~xmlbase (Uri.of_string a))

(* atomLink = element atom:link { atomCommonAttributes, attribute href {
   atomUri }, attribute rel { atomNCName | atomUri }?, attribute type {
   atomMediaType }?, attribute hreflang { atomLanguageTag }?, attribute title {
   text }?, attribute length { text }?, undefinedContent } *)
let link_attr_producer =
  [ ("rel", fun ~xmlbase:_ a -> `Rel a)
  ; ("type", fun ~xmlbase:_ a -> `Type a)
  ; ("hreflang", fun ~xmlbase:_ a -> `HREFLang a)
  ; ("title", fun ~xmlbase:_ a -> `Title a)
  ; ("length", fun ~xmlbase:_ a -> `Length a) ]

let link_of_xml =
  let attr_producer = ("href", link_href_of_xml) :: link_attr_producer in
  generate_catcher ~attr_producer make_link

let link_of_xml' =
  let attr_producer =
    ("href", fun ~xmlbase:_ a -> `HREF a) :: link_attr_producer
  in
  generate_catcher ~attr_producer (fun ~pos:_ x -> `Link x)

type logo = Uri.t

let make_logo ~pos (l : Uri.t list) =
  (* (atomUri) *)
  let uri =
    match l with
    | u :: _ -> u
    | [] ->
        raise
          (Error.Error (pos, "The content of <logo> MUST be a non-empty string"))
  in
  `Logo uri

(* atomLogo = element atom:logo { atomCommonAttributes, (atomUri) } *)
let logo_of_xml =
  let leaf_producer ~xmlbase _pos data =
    XML.resolve ~xmlbase (Uri.of_string data)
  in
  generate_catcher ~leaf_producer make_logo

let logo_of_xml' =
  let leaf_producer ~xmlbase _pos data = `URI (xmlbase, data) in
  generate_catcher ~leaf_producer (fun ~pos:_ x -> `Logo x)

type published = Date.t
type published' = [`Date of string]

let make_published ~pos (l : [< published'] list) =
  (* atom:published { atomDateConstruct } *)
  let date =
    match find (fun (`Date _) -> true) l with
    | Some (`Date d) -> Date.of_rfc3339 d
    | _ ->
        raise
          (Error.Error
             (pos, "The content of <published> MUST be a non-empty string"))
  in
  `Published date

(* atomPublished = element atom:published { atomDateConstruct } *)
let published_of_xml, published_of_xml' =
  let leaf_producer ~xmlbase:_ _pos data = `Date data in
  ( generate_catcher ~leaf_producer make_published
  , generate_catcher ~leaf_producer (fun ~pos:_ x -> `Published x) )

type rights = text_construct

let rights_of_xml ~xmlbase a = `Rights (text_construct_of_xml ~xmlbase a)

(* atomRights = element atom:rights { atomTextConstruct } *)
let rights_of_xml' ~xmlbase:_
    ((_pos, (_tag, _attr), data) : XML.pos * XML.tag * t list) =
  `Rights data

type title = text_construct

let title_of_xml ~xmlbase a = `Title (text_construct_of_xml ~xmlbase a)

(* atomTitle = element atom:title { atomTextConstruct } *)
let title_of_xml' ~xmlbase:_
    ((_pos, (_tag, _attr), data) : XML.pos * XML.tag * t list) =
  `Title data

type subtitle = text_construct

let subtitle_of_xml ~xmlbase a = `Subtitle (text_construct_of_xml ~xmlbase a)

(* atomSubtitle = element atom:subtitle { atomTextConstruct } *)
let subtitle_of_xml' ~xmlbase:_
    ((_pos, (_tag, _attr), data) : XML.pos * XML.tag * t list) =
  `Subtitle data

type updated = Date.t
type updated' = [`Date of string]

let make_updated ~pos (l : [< updated'] list) =
  (* atom:updated { atomDateConstruct } *)
  let updated =
    match find (fun (`Date _) -> true) l with
    | Some (`Date d) -> Date.of_rfc3339 d
    | _ ->
        raise
          (Error.Error
             (pos, "The content of <updated> MUST be a non-empty string"))
  in
  `Updated updated

(* atomUpdated = element atom:updated { atomDateConstruct } *)
let updated_of_xml, updated_of_xml' =
  let leaf_producer ~xmlbase:_ _pos data = `Date data in
  ( generate_catcher ~leaf_producer make_updated
  , generate_catcher ~leaf_producer (fun ~pos:_ x -> `Updated x) )

type source =
  { authors: author list
  ; categories: category list
  ; contributors: author list
  ; generator: generator option
  ; icon: icon option
  ; id: id
  ; links: link list
  ; logo: logo option
  ; rights: rights option
  ; subtitle: subtitle option
  ; title: title
  ; updated: updated option }

let source ?(categories = []) ?(contributors = []) ?generator ?icon
    ?(links = []) ?logo ?rights ?subtitle ?updated ~authors ~id title =
  { authors
  ; categories
  ; contributors
  ; generator
  ; icon
  ; id
  ; links
  ; logo
  ; rights
  ; subtitle
  ; title
  ; updated }

type source' =
  [ `Author of author
  | `Category of category
  | `Contributor of author
  | `Generator of generator
  | `Icon of icon
  | `ID of id
  | `Link of link
  | `Logo of logo
  | `Subtitle of subtitle
  | `Title of title
  | `Rights of rights
  | `Updated of updated ]

let make_source ~pos (l : [< source'] list) =
  (* atomAuthor* *)
  let authors =
    List.fold_left
      (fun acc -> function `Author x -> x :: acc | _ -> acc)
      [] l
  in
  (* atomCategory* *)
  let categories =
    List.fold_left
      (fun acc -> function `Category x -> x :: acc | _ -> acc)
      [] l
  in
  (* atomContributor* *)
  let contributors =
    List.fold_left
      (fun acc -> function `Contributor x -> x :: acc | _ -> acc)
      [] l
  in
  (* atomGenerator? *)
  let generator =
    match find (function `Generator _ -> true | _ -> false) l with
    | Some (`Generator g) -> Some g
    | _ -> None
  in
  (* atomIcon? *)
  let icon =
    match find (function `Icon _ -> true | _ -> false) l with
    | Some (`Icon u) -> Some u
    | _ -> None
  in
  (* atomId? *)
  let id =
    match find (function `ID _ -> true | _ -> false) l with
    | Some (`ID i) -> i
    | _ ->
        raise
          (Error.Error
             (pos, "<source> elements MUST contains exactly one <id> elements"))
  in
  (* atomLink* *)
  let links =
    List.fold_left (fun acc -> function `Link x -> x :: acc | _ -> acc) [] l
  in
  (* atomLogo? *)
  let logo =
    match find (function `Logo _ -> true | _ -> false) l with
    | Some (`Logo u) -> Some u
    | _ -> None
  in
  (* atomRights? *)
  let rights =
    match find (function `Rights _ -> true | _ -> false) l with
    | Some (`Rights r) -> Some r
    | _ -> None
  in
  (* atomSubtitle? *)
  let subtitle =
    match find (function `Subtitle _ -> true | _ -> false) l with
    | Some (`Subtitle s) -> Some s
    | _ -> None
  in
  (* atomTitle? *)
  let title =
    match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title s) -> s
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<source> elements MUST contains exactly one <title> elements"
             ))
  in
  (* atomUpdated? *)
  let updated =
    match find (function `Updated _ -> true | _ -> false) l with
    | Some (`Updated d) -> Some d
    | _ -> None
  in
  `Source
    ( { authors
      ; categories
      ; contributors
      ; generator
      ; icon
      ; id
      ; links
      ; logo
      ; rights
      ; subtitle
      ; title
      ; updated }
      : source )

(* atomSource = element atom:source { atomCommonAttributes, (atomAuthor* &
   atomCategory* & atomContributor* & atomGenerator? & atomIcon? & atomId? &
   atomLink* & atomLogo? & atomRights? & atomSubtitle? & atomTitle? &
   atomUpdated? & extensionElement * ) } *)
let source_of_xml =
  let data_producer =
    [ ("author", author_of_xml)
    ; ("category", category_of_xml)
    ; ("contributor", contributor_of_xml)
    ; ("generator", generator_of_xml)
    ; ("icon", icon_of_xml); ("id", id_of_xml); ("link", link_of_xml)
    ; ("logo", logo_of_xml); ("rights", rights_of_xml)
    ; ("subtitle", subtitle_of_xml)
    ; ("title", title_of_xml)
    ; ("updated", updated_of_xml) ]
  in
  generate_catcher ~namespaces ~data_producer make_source

let source_of_xml' =
  let data_producer =
    [ ("author", author_of_xml')
    ; ("category", category_of_xml')
    ; ("contributor", contributor_of_xml')
    ; ("generator", generator_of_xml')
    ; ("icon", icon_of_xml'); ("id", id_of_xml'); ("link", link_of_xml')
    ; ("logo", logo_of_xml'); ("rights", rights_of_xml')
    ; ("subtitle", subtitle_of_xml')
    ; ("title", title_of_xml')
    ; ("updated", updated_of_xml') ]
  in
  generate_catcher ~namespaces ~data_producer (fun ~pos:_ x -> `Source x)

type mime = string

type content =
  | Text of string
  | Html of Uri.t option * string
  | Xhtml of Uri.t option * Syndic_xml.t list
  | Mime of mime * string
  | Src of mime option * Uri.t

[@@@warning "-34"]

type content' = [`Type of string | `SRC of string | `Data of Syndic_xml.t list]

(* atomInlineTextContent = element atom:content { atomCommonAttributes,
   attribute type { "text" | "html" }?, (text)* }

   atomInlineXHTMLContent = element atom:content { atomCommonAttributes,
   attribute type { "xhtml" }, xhtmlDiv }

   atomInlineOtherContent = element atom:content { atomCommonAttributes,
   attribute type { atomMediaType }?, (text|anyElement)* }

   atomOutOfLineContent = element atom:content { atomCommonAttributes,
   attribute type { atomMediaType }?, attribute src { atomUri }, empty }

   atomContent = atomInlineTextContent | atomInlineXHTMLContent |
   atomInlineOtherContent | atomOutOfLineContent *)
let content_of_xml ~xmlbase
    ((_pos, (_tag, attr), data) : XML.pos * XML.tag * t list) =
  (* MIME ::= attribute type { "text" | "html" }? | attribute type { "xhtml" }
     | attribute type { atomMediaType }? *)
  (* attribute src { atomUri } | none If src s present, [data] MUST be empty. *)
  match find (fun a -> attr_is a "src") attr with
  | Some (_, src) ->
      let mime =
        match find (fun a -> attr_is a "type") attr with
        | Some (_, ty) -> Some ty
        | None -> None
      in
      `Content (Src (mime, XML.resolve ~xmlbase (Uri.of_string src)))
  | None ->
      (* (text)*
       *  | xhtmlDiv
       *  | (text|anyElement)*
       *  | none *)
      `Content
        ( match find (fun a -> attr_is a "type") attr with
        | Some (_, "text") | None -> Text (get_leaf data)
        | Some (_, "html") -> Html (xmlbase, get_html_content data)
        | Some (_, "xhtml") -> Xhtml (xmlbase, get_xml_content data data)
        | Some (_, mime) -> Mime (mime, get_leaf data) )

let content_of_xml' ~xmlbase:_
    ((_pos, (_tag, attr), data) : XML.pos * XML.tag * t list) =
  let l =
    match find (fun a -> attr_is a "src") attr with
    | Some (_, src) -> [`SRC src]
    | None -> []
  in
  let l =
    match find (fun a -> attr_is a "type") attr with
    | Some (_, ty) -> `Type ty :: l
    | None -> l
  in
  `Content (`Data data :: l)

type summary = text_construct

(* atomSummary = element atom:summary { atomTextConstruct } *)
let summary_of_xml ~xmlbase a = `Summary (text_construct_of_xml ~xmlbase a)

let summary_of_xml' ~xmlbase:_ ((_, (_, _), data) : XML.pos * XML.tag * t list)
    =
  `Summary data

type entry =
  { authors: author * author list
  ; categories: category list
  ; content: content option
  ; contributors: author list
  ; id: id
  ; links: link list
  ; published: published option
  ; rights: rights option
  ; source: source option
  ; summary: summary option
  ; title: title
  ; updated: updated }

let entry ?(categories = []) ?content ?(contributors = []) ?(links = [])
    ?published ?rights ?source ?summary ~id ~authors ~title ~updated () =
  { authors
  ; categories
  ; content
  ; contributors
  ; id
  ; links
  ; published
  ; rights
  ; source
  ; summary
  ; title
  ; updated }

type entry' =
  [ `Author of author
  | `Category of category
  | `Contributor of author
  | `ID of id
  | `Link of link
  | `Published of published
  | `Rights of rights
  | `Source of source
  | `Content of content
  | `Summary of summary
  | `Title of title
  | `Updated of updated ]

module LinkOrder : Set.OrderedType with type t = string * string = struct
  type t = string * string

  let compare (a : t) (b : t) =
    match compare (fst a) (fst b) with 0 -> compare (snd a) (snd b) | n -> n
end

module LinkSet = Set.Make (LinkOrder)

let uniq_link_alternate ~pos (l : link list) =
  let string_of_duplicate_link {href; type_media; hreflang; _}
      (type_media', hreflang') =
    let ty = (function Some a -> a | None -> "(none)") type_media in
    let hl = (function Some a -> a | None -> "(none)") hreflang in
    let ty' = (function "" -> "(none)" | s -> s) type_media' in
    let hl' = (function "" -> "(none)" | s -> s) hreflang' in
    Printf.sprintf
      "Duplicate link between <link href=\"%s\" hreflang=\"%s\" type=\"%s\" \
       ..> and <link hreflang=\"%s\" type=\"%s\" ..>"
      (Uri.to_string href) hl ty hl' ty'
  in
  let raise_error link link' =
    raise (Error.Error (pos, string_of_duplicate_link link link'))
  in
  let rec aux acc = function
    | [] -> l
    | ({rel; type_media= Some ty; hreflang= Some hl; _} as x) :: r
      when rel = Alternate ->
        if LinkSet.mem (ty, hl) acc then
          raise_error x (LinkSet.find (ty, hl) acc)
        else aux (LinkSet.add (ty, hl) acc) r
    | ({rel; type_media= None; hreflang= Some hl; _} as x) :: r
      when rel = Alternate ->
        if LinkSet.mem ("", hl) acc then
          raise_error x (LinkSet.find ("", hl) acc)
        else aux (LinkSet.add ("", hl) acc) r
    | ({rel; type_media= Some ty; hreflang= None; _} as x) :: r
      when rel = Alternate ->
        if LinkSet.mem (ty, "") acc then
          raise_error x (LinkSet.find (ty, "") acc)
        else aux (LinkSet.add (ty, "") acc) r
    | ({rel; type_media= None; hreflang= None; _} as x) :: r
      when rel = Alternate ->
        if LinkSet.mem ("", "") acc then
          raise_error x (LinkSet.find ("", "") acc)
        else aux (LinkSet.add ("", "") acc) r
    | _ :: r -> aux acc r
  in
  aux LinkSet.empty l

type feed' =
  [ `Author of author
  | `Category of category
  | `Contributor of author
  | `Generator of generator
  | `Icon of icon
  | `ID of id
  | `Link of link
  | `Logo of logo
  | `Rights of rights
  | `Subtitle of subtitle
  | `Title of title
  | `Updated of updated
  | `Entry of entry ]

let dummy_name = "\000"

let make_entry ~pos l =
  let authors =
    List.fold_left
      (fun acc -> function `Author x -> x :: acc | _ -> acc)
      [] l
  in
  (* atomSource? *)
  let sources =
    List.fold_left
      (fun acc -> function `Source x -> x :: acc | _ -> acc)
      [] l
  in
  let source =
    match sources with
    | [] -> None
    | [s] -> Some s
    | _ ->
        (* RFC 4287 § 4.1.2 *)
        let msg =
          "<entry> elements MUST NOT contain more than one <source> element."
        in
        raise (Error.Error (pos, msg))
  in
  let authors =
    match (authors, source) with
    | a0 :: a, _ -> (a0, a)
    | [], Some (s : source) -> (
      (* If an atom:entry element does not contain atom:author elements, then
         the atom:author elements of the contained atom:source element are
         considered to apply. http://tools.ietf.org/html/rfc4287#section-4.2.1 *)
      match s.authors with
      | a0 :: a -> (a0, a)
      | [] ->
          let msg =
            "<entry> does not contain an <author> and its <source> neither does"
          in
          raise (Error.Error (pos, msg)) )
    | [], None -> ({name= dummy_name; uri= None; email= None}, [])
    (* unacceptable value, see fix_author below *)
    (* atomCategory* *)
  in
  let categories =
    List.fold_left
      (fun acc -> function `Category x -> x :: acc | _ -> acc)
      [] l
    (* atomContributor* *)
  in
  let contributors =
    List.fold_left
      (fun acc -> function `Contributor x -> x :: acc | _ -> acc)
      [] l
  in
  (* atomId *)
  let id =
    match find (function `ID _ -> true | _ -> false) l with
    | Some (`ID i) -> i
    | _ ->
        raise
          (Error.Error
             (pos, "<entry> elements MUST contains exactly one <id> elements"))
    (* atomLink* *)
  in
  let links =
    List.fold_left (fun acc -> function `Link x -> x :: acc | _ -> acc) [] l
  in
  (* atomPublished? *)
  let published =
    match find (function `Published _ -> true | _ -> false) l with
    | Some (`Published s) -> Some s
    | _ -> None
  in
  (* atomRights? *)
  let rights =
    match find (function `Rights _ -> true | _ -> false) l with
    | Some (`Rights r) -> Some r
    | _ -> None
  in
  (* atomContent? *)
  let content =
    match find (function `Content _ -> true | _ -> false) l with
    | Some (`Content c) -> Some c
    | _ -> None
  in
  (* atomSummary? *)
  let summary =
    match find (function `Summary _ -> true | _ -> false) l with
    | Some (`Summary s) -> Some s
    | _ -> None
  in
  (* atomTitle *)
  let title =
    match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<entry> elements MUST contains exactly one <title> elements" ))
  in
  (* atomUpdated *)
  let updated =
    match find (function `Updated _ -> true | _ -> false) l with
    | Some (`Updated u) -> u
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<entry> elements MUST contains exactly one <updated> elements"
             ))
  in
  `Entry
    ( pos
    , ( { authors
        ; categories
        ; content
        ; contributors
        ; id
        ; links= uniq_link_alternate ~pos links
        ; published
        ; rights
        ; source
        ; summary
        ; title
        ; updated }
        : entry ) )

(* atomEntry = element atom:entry { atomCommonAttributes, (atomAuthor* &
   atomCategory* & atomContent? & atomContributor* & atomId & atomLink* &
   atomPublished? & atomRights? & atomSource? & atomSummary? & atomTitle &
   atomUpdated & extensionElement * ) } *)
let entry_of_xml =
  let data_producer =
    [ ("author", author_of_xml)
    ; ("category", category_of_xml)
    ; ("contributor", contributor_of_xml)
    ; ("id", id_of_xml); ("link", link_of_xml)
    ; ("published", published_of_xml)
    ; ("rights", rights_of_xml); ("source", source_of_xml)
    ; ("content", content_of_xml)
    ; ("summary", summary_of_xml)
    ; ("title", title_of_xml)
    ; ("updated", updated_of_xml) ]
  in
  generate_catcher ~namespaces ~data_producer make_entry

let entry_of_xml' =
  let data_producer =
    [ ("author", author_of_xml')
    ; ("category", category_of_xml')
    ; ("contributor", contributor_of_xml')
    ; ("id", id_of_xml'); ("link", link_of_xml')
    ; ("published", published_of_xml')
    ; ("rights", rights_of_xml'); ("source", source_of_xml')
    ; ("content", content_of_xml')
    ; ("summary", summary_of_xml')
    ; ("title", title_of_xml')
    ; ("updated", updated_of_xml') ]
  in
  generate_catcher ~namespaces ~data_producer (fun ~pos:_ x -> `Entry x)

type feed =
  { authors: author list
  ; categories: category list
  ; contributors: author list
  ; generator: generator option
  ; icon: icon option
  ; id: id
  ; links: link list
  ; logo: logo option
  ; rights: rights option
  ; subtitle: subtitle option
  ; title: title
  ; updated: updated
  ; entries: entry list }

let feed ?(authors = []) ?(categories = []) ?(contributors = []) ?generator
    ?icon ?(links = []) ?logo ?rights ?subtitle ~id ~title ~updated entries =
  { authors
  ; categories
  ; contributors
  ; generator
  ; icon
  ; id
  ; links
  ; logo
  ; rights
  ; subtitle
  ; title
  ; updated
  ; entries }

let make_feed ~pos (l : _ list) =
  (* atomAuthor* *)
  let authors =
    List.fold_left
      (fun acc -> function `Author x -> x :: acc | _ -> acc)
      [] l
  in
  (* atomCategory* *)
  let categories =
    List.fold_left
      (fun acc -> function `Category x -> x :: acc | _ -> acc)
      [] l
  in
  (* atomContributor* *)
  let contributors =
    List.fold_left
      (fun acc -> function `Contributor x -> x :: acc | _ -> acc)
      [] l
  in
  (* atomLink* *)
  let links =
    List.fold_left (fun acc -> function `Link x -> x :: acc | _ -> acc) [] l
  in
  (* atomGenerator? *)
  let generator =
    match find (function `Generator _ -> true | _ -> false) l with
    | Some (`Generator g) -> Some g
    | _ -> None
  in
  (* atomIcon? *)
  let icon =
    match find (function `Icon _ -> true | _ -> false) l with
    | Some (`Icon i) -> Some i
    | _ -> None
  in
  (* atomId *)
  let id =
    match find (function `ID _ -> true | _ -> false) l with
    | Some (`ID i) -> i
    | _ ->
        raise
          (Error.Error
             (pos, "<feed> elements MUST contains exactly one <id> elements"))
  in
  (* atomLogo? *)
  let logo =
    match find (function `Logo _ -> true | _ -> false) l with
    | Some (`Logo l) -> Some l
    | _ -> None
  in
  (* atomRights? *)
  let rights =
    match find (function `Rights _ -> true | _ -> false) l with
    | Some (`Rights r) -> Some r
    | _ -> None
  in
  (* atomSubtitle? *)
  let subtitle =
    match find (function `Subtitle _ -> true | _ -> false) l with
    | Some (`Subtitle s) -> Some s
    | _ -> None
  in
  (* atomTitle *)
  let title =
    match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ ->
        raise
          (Error.Error
             (pos, "<feed> elements MUST contains exactly one <title> elements"))
  in
  (* atomUpdated *)
  let updated =
    match find (function `Updated _ -> true | _ -> false) l with
    | Some (`Updated u) -> u
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<feed> elements MUST contains exactly one <updated> elements"
             ))
  in
  (* atomEntry* *)
  let fix_author pos (e : entry) =
    match e.authors with
    | a, [] when a.name = dummy_name -> (
      (* In an Atom Feed Document, the atom:author elements of the containing
         atom:feed element are considered to apply to the entry if there are no
         atom:author elements in the locations described above.
         http://tools.ietf.org/html/rfc4287#section-4.2.1 *)
      match authors with
      | a0 :: a -> {e with authors= (a0, a)}
      | [] ->
          let msg =
            "<entry> elements MUST contains at least an <author> element or \
             <feed> element MUST contains one or more <author> elements"
          in
          raise (Error.Error (pos, msg)) )
    | _ -> e
  in
  let entries =
    List.fold_left
      (fun acc -> function `Entry (pos, e) -> fix_author pos e :: acc
        | _ -> acc )
      [] l
  in
  ( { authors
    ; categories
    ; contributors
    ; generator
    ; icon
    ; id
    ; links
    ; logo
    ; rights
    ; subtitle
    ; title
    ; updated
    ; entries }
    : feed )

(* atomFeed = element atom:feed { atomCommonAttributes, (atomAuthor* &
   atomCategory* & atomContributor* & atomGenerator? & atomIcon? & atomId &
   atomLink* & atomLogo? & atomRights? & atomSubtitle? & atomTitle &
   atomUpdated & extensionElement * ), atomEntry* } *)

let feed_of_xml =
  let data_producer =
    [ ("author", author_of_xml)
    ; ("category", category_of_xml)
    ; ("contributor", contributor_of_xml)
    ; ("generator", generator_of_xml)
    ; ("icon", icon_of_xml); ("id", id_of_xml); ("link", link_of_xml)
    ; ("logo", logo_of_xml); ("rights", rights_of_xml)
    ; ("subtitle", subtitle_of_xml)
    ; ("title", title_of_xml)
    ; ("updated", updated_of_xml)
    ; ("entry", entry_of_xml) ]
  in
  generate_catcher ~namespaces ~data_producer make_feed

let feed_of_xml' =
  let data_producer =
    [ ("author", author_of_xml')
    ; ("category", category_of_xml')
    ; ("contributor", contributor_of_xml')
    ; ("generator", generator_of_xml')
    ; ("icon", icon_of_xml'); ("id", id_of_xml'); ("link", link_of_xml')
    ; ("logo", logo_of_xml'); ("rights", rights_of_xml')
    ; ("subtitle", subtitle_of_xml')
    ; ("title", title_of_xml')
    ; ("updated", updated_of_xml')
    ; ("entry", entry_of_xml') ]
  in
  generate_catcher ~namespaces ~data_producer (fun ~pos:_ x -> x)

(* Remove all tags *)
let rec add_to_buffer buf = function
  | XML.Node (_, _, subs) -> List.iter (add_to_buffer buf) subs
  | XML.Data (_, d) -> Buffer.add_string buf d

let xhtml_to_string xhtml =
  let buf = Buffer.create 128 in
  List.iter (add_to_buffer buf) xhtml ;
  Buffer.contents buf

let string_of_text_construct = function
  (* FIXME: Once we use a proper HTML library, we probably would like to parse
     the HTML and remove the tags *)
  | (Text s : text_construct) | Html (_, s) -> s
  | Xhtml (_, x) -> xhtml_to_string x

let parse ?self ?xmlbase input =
  let feed =
    match XML.of_xmlm input |> snd with
    | XML.Node (pos, tag, datas) when tag_is tag "feed" ->
        feed_of_xml ~xmlbase (pos, tag, datas)
    | _ ->
        raise
          (Error.Error
             ((0, 0), "document MUST contains exactly one <feed> element"))
  in
  (* FIXME: the spec says that an entry can appear as the top-level element *)
  match self with
  | None -> feed
  | Some self ->
      if List.exists (fun l -> l.rel = Self) feed.links then feed
      else
        let links =
          { href= self
          ; rel= Self
          ; type_media= Some "application/atom+xml"
          ; hreflang= None
          ; title= string_of_text_construct feed.title
          ; length= None }
          :: feed.links
        in
        {feed with links}

let read ?self ?xmlbase fname =
  let fh = open_in fname in
  try
    let x = parse ?self ?xmlbase (XML.input_of_channel fh) in
    close_in fh ; x
  with e -> close_in fh ; raise e

let set_self_link feed ?hreflang ?length url =
  match List.partition (fun l -> l.rel = Self) feed.links with
  | l :: _, links ->
      let hreflang =
        match hreflang with None -> l.hreflang | Some _ -> hreflang
      in
      let length = match length with None -> l.length | Some _ -> length in
      let self = {l with href= url; hreflang; length} in
      {feed with links= self :: links}
  | [], links ->
      let links =
        { href= url
        ; rel= Self
        ; type_media= Some "application/atom+xml"
        ; hreflang
        ; title= string_of_text_construct feed.title
        ; length }
        :: links
      in
      {feed with links}

let get_self_link feed =
  try Some (List.find (fun l -> l.rel = Self) feed.links) with Not_found ->
    None

let unsafe ?xmlbase input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, datas) when tag_is tag "feed" ->
      `Feed (feed_of_xml' ~xmlbase (pos, tag, datas))
  | _ -> `Feed []

let remove_empty_authors a = List.filter not_empty_author a

(* [normalize_authors a authors] returns (a', authors') where [authors'] is
   [authors] where the empty authors and the author [a] have been removed and
   [a'] is [a] possibly completed with the information found for [a] in
   [authors]. *)
let rec normalize_authors (a : author) = function
  | [] -> (a, [])
  | a0 :: tl ->
      if not_empty_author a0 then
        if a0.name = a.name then
          (* Merge [a0] and [a]. *)
          let uri = match a.uri with None -> a0.uri | Some _ -> a.uri in
          let email =
            match a.email with None -> a0.email | Some _ -> a.email
          in
          normalize_authors {name= a.name; uri; email} tl
        else
          let a', authors' = normalize_authors a tl in
          (a', a0 :: authors')
      else normalize_authors a tl

(* drop the empty author *)

let set_main_author_entry author (e : entry) =
  (* If the entry has a source, then [author] should be ignored and the one
     from the [source] should be used instead. *)
  let author, author_ok, source =
    match e.source with
    | None -> (author, true, None)
    | Some s -> (
        let s_authors = remove_empty_authors s.authors in
        let s_contributors = remove_empty_authors s.contributors in
        let s =
          Some {s with authors= s_authors; contributors= s_contributors}
        in
        (* A source exists. If it contains no author, one should not change the
           entry authors with [author] because that may wrongly attribute the
           post. *)
        match s_authors with
        | [] -> (author, false, s)
        | s_author :: _ -> (s_author, true, s) )
  in
  let a0, a = e.authors in
  let authors =
    match remove_empty_authors (a0 :: a) with
    | a0 :: a -> (a0, a)
    | [] -> ((if author_ok then author else empty_author), [])
  in
  let contributors = remove_empty_authors e.contributors in
  {e with authors; contributors; source}

let set_main_author feed author =
  let author, authors = normalize_authors author feed.authors in
  let contributors = remove_empty_authors feed.contributors in
  let entries = List.map (set_main_author_entry author) feed.entries in
  {feed with authors= author :: authors; contributors; entries}

(* Conversion to XML *)

(* Tag with the Atom namespace *)
let atom name : XML.tag = ((atom_ns, name), [])

let add_attr_xmlbase ~xmlbase attrs =
  match xmlbase with
  | Some u -> ((Xmlm.ns_xml, "base"), Uri.to_string u) :: attrs
  | None -> attrs

let text_construct_to_xml tag_name (t : text_construct) =
  match t with
  | Text t ->
      XML.Node
        ( dummy_pos
        , ((atom_ns, tag_name), [(("", "type"), "text")])
        , [XML.Data (dummy_pos, t)] )
  | Html (xmlbase, t) ->
      let attr = add_attr_xmlbase ~xmlbase [(("", "type"), "html")] in
      XML.Node
        (dummy_pos, ((atom_ns, tag_name), attr), [XML.Data (dummy_pos, t)])
  | Xhtml (xmlbase, x) ->
      let div =
        XML.Node
          (dummy_pos, ((xhtml_ns, "div"), [(("", "xmlns"), xhtml_ns)]), x)
      in
      let attr = add_attr_xmlbase ~xmlbase [(("", "type"), "xhtml")] in
      XML.Node (dummy_pos, ((atom_ns, tag_name), attr), [div])

let person_to_xml name (a : author) =
  XML.Node
    ( dummy_pos
    , atom name
    , [node_data (atom "name") a.name]
      |> add_node_uri (atom "uri") a.uri
      |> add_node_data (atom "email") a.email )

let author_to_xml a = person_to_xml "author" a
let contributor_to_xml a = person_to_xml "contributor" a

let category_to_xml (c : category) =
  let attrs =
    [(("", "term"), c.term)]
    |> add_attr_uri ("", "scheme") c.scheme
    |> add_attr ("", "label") c.label
  in
  XML.Node (dummy_pos, ((atom_ns, "category"), attrs), [])

let generator_to_xml (g : generator) =
  let attr =
    [] |> add_attr ("", "version") g.version |> add_attr_uri ("", "uri") g.uri
  in
  XML.Node
    ( dummy_pos
    , ((atom_ns, "generator"), attr)
    , [XML.Data (dummy_pos, g.content)] )

let string_of_rel = function
  | Alternate -> "alternate"
  | Related -> "related"
  | Self -> "self"
  | Enclosure -> "enclosure"
  | Via -> "via"
  | Link l -> Uri.to_string l

let link_to_xml (l : link) =
  let attr =
    [(("", "href"), Uri.to_string l.href); (("", "rel"), string_of_rel l.rel)]
    |> add_attr ("", "type") l.type_media
    |> add_attr ("", "hreflang") l.hreflang
  in
  let attr = if l.title = "" then attr else (("", "title"), l.title) :: attr in
  let attr =
    match l.length with
    | Some len -> (("", "length"), string_of_int len) :: attr
    | None -> attr
  in
  XML.Node (dummy_pos, ((atom_ns, "link"), attr), [])

let add_node_date tag date nodes =
  match date with
  | None -> nodes
  | Some d -> node_data tag (Date.to_rfc3339 d) :: nodes

let source_to_xml (s : source) =
  let nodes =
    node_data (atom "id") (Uri.to_string s.id)
    :: text_construct_to_xml "title" s.title
    :: List.map author_to_xml s.authors
    |> add_nodes_rev_map category_to_xml s.categories
    |> add_nodes_rev_map contributor_to_xml s.contributors
    |> add_node_option generator_to_xml s.generator
    |> add_node_option (node_uri (atom "icon")) s.icon
    |> add_nodes_rev_map link_to_xml s.links
    |> add_node_option (node_uri (atom "logo")) s.logo
    |> add_node_option (text_construct_to_xml "rights") s.rights
    |> add_node_option (text_construct_to_xml "subtitle") s.subtitle
    |> add_node_date (atom "updated") s.updated
  in
  XML.Node (dummy_pos, atom "source", nodes)

let content_to_xml (c : content) =
  match c with
  | Text t ->
      XML.Node
        ( dummy_pos
        , ((atom_ns, "content"), [(("", "type"), "text")])
        , [XML.Data (dummy_pos, t)] )
  | Html (xmlbase, t) ->
      let attrs = add_attr_xmlbase ~xmlbase [(("", "type"), "html")] in
      XML.Node
        (dummy_pos, ((atom_ns, "content"), attrs), [XML.Data (dummy_pos, t)])
  | Xhtml (xmlbase, x) ->
      let div =
        XML.Node
          (dummy_pos, ((xhtml_ns, "div"), [(("", "xmlns"), xhtml_ns)]), x)
      in
      let attrs = add_attr_xmlbase ~xmlbase [(("", "type"), "xhtml")] in
      XML.Node (dummy_pos, ((atom_ns, "content"), attrs), [div])
  | Mime (mime, d) ->
      XML.Node
        ( dummy_pos
        , ((atom_ns, "content"), [(("", "type"), mime)])
        , [XML.Data (dummy_pos, d)] )
  | Src (mime, uri) ->
      let attr =
        [(("", "src"), Uri.to_string uri)] |> add_attr ("", "type") mime
      in
      XML.Node (dummy_pos, ((atom_ns, "content"), attr), [])

let entry_to_xml (e : entry) =
  let a0, a = e.authors in
  let nodes =
    node_data (atom "id") (Uri.to_string e.id)
    :: text_construct_to_xml "title" e.title
    :: node_data (atom "updated") (Date.to_rfc3339 e.updated)
    :: author_to_xml a0
    :: List.map author_to_xml a
    |> add_nodes_rev_map category_to_xml e.categories
    |> add_node_option content_to_xml e.content
    |> add_nodes_rev_map contributor_to_xml e.contributors
    |> add_nodes_rev_map link_to_xml e.links
    |> add_node_date (atom "published") e.published
    |> add_node_option (text_construct_to_xml "rights") e.rights
    |> add_node_option source_to_xml e.source
    |> add_node_option (text_construct_to_xml "summary") e.summary
  in
  XML.Node (dummy_pos, atom "entry", nodes)

let to_xml (f : feed) =
  let nodes =
    node_data (atom "id") (Uri.to_string f.id)
    :: text_construct_to_xml "title" f.title
    :: node_data (atom "updated") (Date.to_rfc3339 f.updated)
    :: List.map entry_to_xml f.entries
    |> add_nodes_rev_map author_to_xml (List.rev f.authors)
    |> add_nodes_rev_map category_to_xml f.categories
    |> add_nodes_rev_map contributor_to_xml f.contributors
    |> add_node_option generator_to_xml f.generator
    |> add_node_option (node_uri (atom "icon")) f.icon
    |> add_nodes_rev_map link_to_xml f.links
    |> add_node_option (node_uri (atom "logo")) f.logo
    |> add_node_option (text_construct_to_xml "rights") f.rights
    |> add_node_option (text_construct_to_xml "subtitle") f.subtitle
  in
  XML.Node (dummy_pos, ((atom_ns, "feed"), [(("", "xmlns"), atom_ns)]), nodes)

(* Atom and XHTML have been declared well in the above XML representation. One
   can remove them. *)
let output_ns_prefix s = if s = atom_ns || s = xhtml_ns then Some "" else None

let output feed dest =
  let o = XML.make_output dest ~ns_prefix:output_ns_prefix in
  XML.to_xmlm (to_xml feed) o

let write feed fname =
  let fh = open_out fname in
  try
    output feed (`Channel fh) ;
    close_out fh
  with e -> close_out fh ; raise e

(* Comparing entries *)

let entry_date e = match e.published with Some d -> d | None -> e.updated

let ascending (e1 : entry) (e2 : entry) =
  Date.compare (entry_date e1) (entry_date e2)

let descending (e1 : entry) (e2 : entry) =
  Date.compare (entry_date e2) (entry_date e1)

(* Feed aggregation *)

let syndic_generator =
  { version= Some Syndic_conf.version
  ; uri= Some Syndic_conf.homepage
  ; content= "OCaml Syndic.Atom feed aggregator" }

let ocaml_icon = Uri.of_string "http://ocaml.org/img/colour-icon-170x148.png"
let default_title : text_construct = Text "Syndic.Atom aggregated feed"

let[@warning "-32"] is_alternate_Atom (l : link) =
  match l.type_media with
  | None -> false
  | Some ty -> ty = "application/atom+xml" && l.rel = Alternate

let add_entries_of_feed entries feed : entry list =
  let source_of_feed =
    Some
      { authors= feed.authors
      ; categories= feed.categories
      ; contributors= feed.contributors
      ; generator= feed.generator
      ; icon= feed.icon
      ; id= feed.id
      ; links= feed.links
      ; logo= feed.logo
      ; rights= feed.rights
      ; subtitle= feed.subtitle
      ; title= feed.title
      ; updated= Some feed.updated }
  in
  let add_entry entries (e : entry) =
    match e.source with
    | Some _ -> e :: entries (* if a source is present, do not overwrite it. *)
    | None -> {e with source= source_of_feed} :: entries
  in
  List.fold_left add_entry entries feed.entries

let entries_of_feeds feeds = List.fold_left add_entries_of_feed [] feeds

let more_recent d1 (e : entry) =
  if Date.compare d1 e.updated >= 0 then d1 else e.updated

let aggregate ?self ?id ?updated ?subtitle ?(title = default_title)
    ?(sort = `Newest_first) ?n feeds : feed =
  let entries = entries_of_feeds feeds in
  let entries =
    match sort with
    | `Newest_first -> List.sort descending entries
    | `Oldest_first -> List.sort ascending entries
    | `None -> entries
  in
  let entries = match n with Some n -> take entries n | None -> entries in
  let id =
    match id with
    | Some id -> id
    | None ->
        (* Collect all ids of the entries and "digest" them. *)
        let b = Buffer.create 4096 in
        let add_id (e : entry) = Buffer.add_string b (Uri.to_string e.id) in
        List.iter add_id entries ;
        let d = Digest.to_hex (Digest.string (Buffer.contents b)) in
        (* FIXME: use urn:uuid *)
        Uri.of_string ("urn:md5:" ^ d)
  in
  let links =
    match self with
    | Some u ->
        [ link u
            ~title:(string_of_text_construct title)
            ~rel:Self ~type_media:"application/atom+xml" ]
    | None -> []
  in
  let updated =
    match updated with
    | Some d -> d
    | None -> (
      (* Use the more recent date of the entries. *)
      match entries with
      | [] -> Date.epoch
      | e0 :: el -> List.fold_left more_recent e0.updated el )
  in
  { authors= []
  ; categories= []
  ; contributors= []
  ; generator= Some syndic_generator
  ; icon= Some ocaml_icon
  ; id
  ; links
  ; logo= None
  ; rights= None
  ; subtitle
  ; title
  ; updated
  ; entries }

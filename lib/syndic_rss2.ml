open Syndic_common.XML
open Syndic_common.Util
module XML = Syndic_xml
module Atom = Syndic_atom

module Date = Syndic_date
module Error = Syndic_error

type story =
  | All of string * Uri.t option * string
  | Title of string
  | Description of Uri.t option * string

module Relax =
struct
  type ('url, 'title, 'link) image =
    {
      url   : pos:Xmlm.pos -> Uri.t option -> 'url;
      title : pos:Xmlm.pos -> string option -> 'title;
      link  : pos:Xmlm.pos -> Uri.t option -> 'link;
    }

  let image : (Uri.t, string, Uri.t) image =
    {
      url   = (fun ~pos -> function
               | Some uri -> uri
               | None -> raise (Error.Error (pos, "<image> elements MUST \
                                                   contains exactly one \
                                                   <url> element")));
      title = (fun ~pos -> function
               | Some title -> title
               | None -> raise (Error.Error (pos, "<image> elements MUST \
                                                   contains exactly one \
                                                   <title> element")));
      link  = (fun ~pos -> function
               | Some link -> link
               | None -> raise (Error.Error (pos, "<image> elements MUST \
                                                   contains exactly one \
                                                   <link> element")));
    }

  type ('domain, 'port, 'path, 'uri, 'procedure, 'protocol) cloud =
    {
      registerProcedure : pos:Xmlm.pos -> string option -> 'procedure;
      protocol          : pos:Xmlm.pos -> string option -> 'protocol;
      domain            : pos:Xmlm.pos -> string option -> 'domain;
      port              : pos:Xmlm.pos -> string option -> 'port;
      path              : pos:Xmlm.pos -> string option -> 'path;
      uri               : pos:Xmlm.pos -> 'domain -> 'port -> 'path -> 'uri;
    }

  let cloud : (string, int, string, Uri.t, string, string) cloud =
    {
      registerProcedure = (fun ~pos -> function
                           | Some p -> p
                           | None -> raise (Error.Error (pos,
                                            "<cloud> elements MUST hava a \
                                             registerProcedure attribute")));
      protocol          = (fun ~pos -> function
                           | Some p -> p
                           | None -> raise (Error.Error (pos,
                                            "<cloud> elements MUST have a protocol \
                                             attribute")));
      domain            = (fun ~pos -> function
                           | Some d -> d
                           | None -> raise (Error.Error (pos,
                                            "<cloud> elements MUST have a domain \
                                             attribute")));
      port              = (fun ~pos -> function
                           | Some p ->
                             (try int_of_string p
                              with exn -> raise (Error.Error (pos,
                                          "attribute port of <cloud> elements MUST be \
                                           an int")))
                           | None -> raise (Error.Error (pos,
                                            "<cloud> elements MUST have a port \
                                             attribute")));
      path              = (fun ~pos -> function
                           | Some p -> p
                           | None -> raise (Error.Error (pos,
                                            "<cloud> elements MUST have a path attribute")));
      uri               = (fun ~pos domain port path -> Uri.make ~host:domain ~port ~path ());
    }

  type ('title, 'description, 'name, 'link) textinput =
    {
      title       : pos:Xmlm.pos -> string option -> 'title;
      description : pos:Xmlm.pos -> string option -> 'description;
      name        : pos:Xmlm.pos -> string option -> 'name;
      link        : pos:Xmlm.pos -> Uri.t option -> 'link;
    }

  let textinput : (string, string, string, Uri.t) textinput =
    {
      title       = (fun ~pos -> function
                     | Some s -> s
                     | None -> raise (Error.Error (pos, "<textinput> elements \
                                                         MUST contains exactly \
                                                         one <title> element")));
      description = (fun ~pos -> function
                     | Some s -> s
                     | None -> raise (Error.Error (pos, "<textinput>
                                                         elements MUST \
                                                         contains exactly \
                                                         one \
                                                         <description> \
                                                         element")));
      name        = (fun ~pos -> function
                     | Some s -> s
                     | None -> raise (Error.Error (pos, "<textinput> elements \
                                                         MUST contains exactly \
                                                         one <name> element")));
      link        = (fun ~pos -> function
                     | Some uri -> uri
                     | None -> raise (Error.Error (pos, "<textinput> elements \
                                                         MUST contains exactly \
                                                         one <link> element")));
    }

  type ('url, 'length, 'mime) enclosure =
    {
      url    : pos:Xmlm.pos -> Uri.t option -> 'url;
      length : pos:Xmlm.pos -> string option -> 'length;
      mime   : pos:Xmlm.pos -> string option -> 'mime;
    }

  let enclosure : (Uri.t, int, string) enclosure =
    {
      url    = (fun ~pos -> function
                | Some uri -> uri
                | None -> raise (Error.Error (pos, "<enclosure> elements \
                                                    MUST have a 'url' \
                                                    attribute")));
      length = (fun ~pos -> function
                | None -> raise (Error.Error (pos, "<enclosure> elements \
                                                    MUST have a 'length'
                                                    attribute"))
                | Some i ->
                  try int_of_string i
                  with exn -> raise (Error.Error (pos, "the attribute \
                                                        'length' for the \
                                                        element \
                                                        <enclosure> MUST \
                                                        be an integer")));
      mime   = (fun ~pos -> function
                | Some m -> m
                | None -> raise (Error.Error (pos, "<enclosure> elements \
                                                    MUST have a 'mime'
                                                    attribute")));
    }

  type ('data, 'url) source =
    {
      data : pos:Xmlm.pos -> string option -> 'data;
      url  : pos:Xmlm.pos -> Uri.t option -> 'url;
    }

  let source : (string, Uri.t) source =
    {
      data = (fun ~pos -> function
              | Some s -> s
              | None -> raise (Error.Error (pos, "The content of <source> \
                                                  MUST be a non-empty \
                                                  string")));
      url  = (fun ~pos -> function
              | Some u -> u
              | None -> raise (Error.Error (pos, "<source> elements MUST \
                                                  have a 'url' attribute")));
    }

  type ('story,
        'url_enclosure, 'length_enclosure, 'mime_enclosure,
        'data_source, 'url_source) item =
    {
      story     : pos:Xmlm.pos -> string option -> (Uri.t option * string) option -> 'story;
      enclosure : ('url_enclosure, 'length_enclosure, 'mime_enclosure) enclosure;
      source    : ('data_source, 'url_source) source;
    }

  let item : (story, Uri.t, int, string, string, Uri.t) item =
    {
      story     = (fun ~pos title description -> match title, description with
                   | Some title, Some (xmlbase, description) ->
                     All (title, xmlbase, description)
                   | Some title, _ ->
                     Title title
                   | _, Some (xmlbase, description) ->
                     Description (xmlbase, description)
                   | _ -> raise (Error.Error (pos, "<item> elements expected <title> \
                                                    or <description> tag")));
      enclosure = enclosure;
      source    = source;
    }

  type ('title, 'link, 'description,
        'domain_cloud, 'port_cloud, 'path_cloud, 'uri_cloud, 'procedure_cloud, 'protocol_cloud,
        'url_image, 'title_image, 'link_image,
        'title_textinput, 'description_textinput, 'name_textinput, 'link_textinput,
        'story_item,
        'url_enclosure, 'length_enclosure, 'mime_enclosure,
        'data_source, 'url_source) channel =
    {
      title       : pos:Xmlm.pos -> string option -> 'title;
      link        : pos:Xmlm.pos -> Uri.t option -> 'link;
      description : pos:Xmlm.pos -> string option -> 'description;
      cloud       : ('domain_cloud, 'port_cloud, 'path_cloud, 'uri_cloud, 'procedure_cloud, 'protocol_cloud) cloud;
      image       : ('url_image, 'title_image, 'link_image) image;
      textInput   : ('title_textinput, 'description_textinput, 'name_textinput, 'link_textinput) textinput;
      item        : ('story_item, 'url_enclosure, 'length_enclosure, 'mime_enclosure, 'data_source, 'url_source) item;
    }

  let channel : (string, Uri.t, string,
                 string, int, string, Uri.t, string, string,
                 Uri.t, string, Uri.t,
                 string, string, string, Uri.t,
                 story,
                 Uri.t, int, string,
                 string, Uri.t) channel =
    {
      title       = (fun ~pos -> function
                     | Some t -> t
                     | None -> raise (Error.Error (pos, "<channel> elements \
                                                         MUST contains exactly \
                                                         one <title> element")));
      link        = (fun ~pos -> function
                     | Some l -> l
                     | None -> raise (Error.Error (pos, "<channel> elements \
                                                         MUST contains exactly \
                                                         one <link> element")));
      description = (fun ~pos -> function
                     | Some d -> d
                     | None -> raise (Error.Error (pos, "<channel> elements \
                                                         MUST contains exactly \
                                                         one <description> \
                                                         element")));
      cloud       = cloud;
      image       = image;
      textInput   = textinput;
      item        = item;
    }
end

type ('url, 'title, 'link) image =
  {
    url         : 'url;   (* Uri.t *)
    title       : 'title; (* string *)
    link        : 'link;  (* Uri.t *)
    width       : int;    (* default 88 *)
    height      : int;    (* default 31 *)
    description : string option;
  }

type image' = [
  | `URL of Uri.t
  | `Title of string
  | `Link of Uri.t
  | `Width of int
  | `Height of int
  | `Description of string
]

let make_image :
  type url title link. Xmlm.pos ->
                       (url, title, link) Relax.image ->
                       [< image' ] list ->
                       [ `Image of (url, title, link) image ] =
  fun pos relax l ->
  let url = match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> relax.Relax.url ~pos (Some u)
    | _ -> relax.Relax.url ~pos None
  in
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> relax.Relax.title ~pos (Some t)
    | _ -> relax.Relax.title ~pos None
  in
  let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link l) -> relax.Relax.link ~pos (Some l)
    | _ -> relax.Relax.link ~pos None
  in
  let width = match find (function `Width _ -> true | _ -> false) l with
    | Some (`Width w) -> w
    | _ -> 88 (* cf. RFC *)
  in
  let height = match find (function `Height _ -> true | _ -> false) l with
    | Some (`Height h) -> h
    | _ -> 31 (* cf. RFC *)
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description s) -> Some s
    | _ -> None
  in
  `Image ({ url; title; link; width; height; description } : (url, title, link) image)

let make_image
  : type url title link.
    pos:Xmlm.pos ->
    (url, title, link) Relax.image ->
    [< image' ] list ->
    [ `Image of (url, title, link) image ]
  = fun ~pos relax l -> make_image pos relax l

let url_of_xml ~xmlbase a =
  `URL (XML.resolve ~xmlbase (Uri.of_string a))
  (* XXX: Uri.of_string would be not fail. *)

let url_of_xml' ~xmlbase a = `URL (xmlbase, a)

let image_url_of_xml ~xmlbase (pos, tag, datas) =
  try url_of_xml ~xmlbase (get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <uri> MUST be \
                             a non-empty string"))

let image_title_of_xml ~xmlbase (pos, tag, datas) =
  `Title (try get_leaf datas
          with Not_found -> "")

let image_link_of_xml ~xmlbase (pos, tag, datas) =
  try `Link (XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <link> MUST be \
                             a non-empty string"))

let image_size_of_xml ~max ~xmlbase (pos, tag, datas) =
  try let size = int_of_string (get_leaf datas) in
    if size > max
    then raise (Error.Error
                  (pos, ("size of "  ^ (get_tag_name tag)
                         ^ " exceeded (max is " ^ (string_of_int max) ^ ")")))
    else size
  with Not_found -> raise (Error.Error (pos,
                            ("The content of <"^(get_tag_name tag)^"> MUST be \
                              a non-empty string")))
     | Failure "int_of_string" -> raise (Error.Error (pos,
                            ("The content of <"^(get_tag_name tag)^"> MUST be \
                              an integer")))

let image_width_of_xml ~xmlbase a =
  `Width (image_size_of_xml ~max:144 ~xmlbase a)
let image_height_of_xml ~xmlbase a =
  `Height (image_size_of_xml ~max:400 ~xmlbase a)

let image_description_of_xml ~xmlbase (pos, tag, datas) =
  try `Description (get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <description> MUST be \
                             a non-empty string"))

let image_of_xml relax_image =
  let data_producer = [
    ("url", image_url_of_xml);
    ("title", image_title_of_xml);
    ("link", image_link_of_xml);
    ("width", image_width_of_xml);
    ("height", image_height_of_xml);
    ("description", image_description_of_xml);
  ] in
  generate_catcher ~data_producer (fun ~pos -> make_image ~pos relax_image)

let image_of_xml' =
  let data_producer = [
    ("url", dummy_of_xml ~ctor:url_of_xml');
    ("title", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Title a));
    ("link", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Link(xmlbase, a)));
    ("width", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Width a));
    ("height", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Height a));
    ("description", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Description a));
  ] in
  generate_catcher ~data_producer (fun ~pos x -> `Image x)

type ('uri, 'procedure, 'protocol) cloud =
  {
    uri               : 'uri;       (* Uri.t *)
    registerProcedure : 'procedure; (* string *)
    protocol          : 'protocol;  (* string *)
  }

type cloud' = [
  | `Domain of string
  | `Port of string
  | `Path of string
  | `RegisterProcedure of string
  | `Protocol of string
]

let make_cloud :
  type domain port path uri procedure protocol.
    Xmlm.pos ->
    (domain, port, path, uri, procedure, protocol) Relax.cloud ->
    [< cloud' ] list ->
    [ `Cloud of (uri, procedure, protocol) cloud ] =
  fun pos relax l ->
  let domain = match find (function `Domain _ -> true | _ -> false) l with
    | Some (`Domain u) -> relax.Relax.domain ~pos (Some u)
    | _ -> relax.Relax.domain ~pos None
  in
  let port = match find (function `Port _ -> true | _ -> false) l with
    | Some (`Port p) -> relax.Relax.port ~pos (Some p)
    | _ -> relax.Relax.port ~pos None
  in
  let path = match find (function `Path _ -> true | _ -> false) l with
    | Some (`Path p) -> relax.Relax.path ~pos (Some p)
    | _ -> relax.Relax.path ~pos None
  in
  let registerProcedure =
    match find (function `RegisterProcedure _ -> true | _ -> false) l with
    | Some (`RegisterProcedure r) -> relax.Relax.registerProcedure ~pos (Some r)
    | _ -> relax.Relax.registerProcedure ~pos None
  in
  let protocol = match find (function `Protocol _ -> true | _ -> false) l with
    | Some (`Protocol p) -> relax.Relax.protocol ~pos (Some p)
    | _ -> relax.Relax.protocol ~pos None
  in
  let uri = relax.Relax.uri ~pos domain port path in
  `Cloud ({ uri; registerProcedure; protocol; } : (uri, procedure, protocol) cloud)

let cloud_attr_producer = [
    ("domain", (fun ~xmlbase a -> `Domain a));
    ("port", (fun ~xmlbase a -> `Port a));
    ("path", (fun ~xmlbase a -> `Path a)); (* XXX: it's RFC compliant ? *)
    ("registerProcedure", (fun ~xmlbase a -> `RegisterProcedure a));
    ("protocol", (fun ~xmlbase a -> `Protocol a));
  ]

let make_cloud
  : type domain port path uri procedure protocol.
    pos:Xmlm.pos ->
    (domain, port, path, uri, procedure, protocol) Relax.cloud ->
    [< cloud' ] list ->
    [ `Cloud of (uri, procedure, protocol) cloud ]
  = fun ~pos relax l -> make_cloud pos relax l

let cloud_of_xml relax_cloud =
  generate_catcher
    ~attr_producer:cloud_attr_producer
    (fun ~pos -> make_cloud ~pos relax_cloud)

let cloud_of_xml' =
  generate_catcher ~attr_producer:cloud_attr_producer (fun ~pos x -> `Cloud x)

type ('title, 'description, 'name, 'link) textinput =
  {
    title       : 'title;       (* string *)
    description : 'description; (* string *)
    name        : 'name;        (* string *)
    link        : 'link;        (* Uri.t *)
  }

type textinput' = [
  | `Title of string
  | `Description of string
  | `Name of string
  | `Link of Uri.t
]

let make_textinput :
  type title description name link. Xmlm.pos ->
                                    (title, description, name, link) Relax.textinput ->
                                    [< textinput'] list ->
                                    [ `TextInput of (title, description, name, link) textinput ] =
  fun pos relax l ->
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> relax.Relax.title ~pos (Some t)
    | _ -> relax.Relax.title ~pos None
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description s) -> relax.Relax.description ~pos (Some s)
    | _ -> relax.Relax.description ~pos None
  in
  let name = match find (function `Name _ -> true | _ -> false) l with
    | Some (`Name s) -> relax.Relax.name ~pos (Some s)
    | _ -> relax.Relax.name ~pos None
  in
  let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link u) -> relax.Relax.link ~pos (Some u)
    | _ -> relax.Relax.link ~pos None
  in
  `TextInput ({ title; description; name; link; } : (title, description, name, link) textinput)

let make_textinput
  : type title description name link.
    pos:Xmlm.pos ->
    (title, description, name, link) Relax.textinput ->
    [< textinput'] list ->
    [ `TextInput of (title, description, name, link) textinput ]
  = fun ~pos relax l -> make_textinput pos relax l

let textinput_title_of_xml ~xmlbase (pos, tag, datas) =
  try `Title(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <title> MUST be \
                             a non-empty string"))

let textinput_description_of_xml ~xmlbase (pos, tag, datas) =
  try `Description(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <description> MUST be \
                             a non-empty string"))

let textinput_name_of_xml ~xmlbase (pos, tag, datas) =
  try `Name(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <name> MUST be \
                             a non-empty string"))

let textinput_link_of_xml ~xmlbase (pos, tag, datas) =
  try `Link(XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
      (* XXX: Uri.of_string would be not fail. *)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <link> MUST be \
                             a non-empty string"))

let textinput_of_xml relax_textinput =
  let data_producer = [
    ("title", textinput_title_of_xml);
    ("description", textinput_description_of_xml);
    ("name", textinput_name_of_xml);
    ("link", textinput_link_of_xml);
  ] in
  generate_catcher ~data_producer (fun ~pos -> make_textinput ~pos relax_textinput)

let textinput_of_xml' =
  let data_producer = [
    ("title", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Title a));
    ("description", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Description a));
    ("name", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Name a));
    ("link", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Link(xmlbase, a)));
  ] in
  generate_catcher ~data_producer (fun ~pos x -> `TextInput x)

type category =
  {
    data   : string; (* TODO: mandatory? *)
    domain : Uri.t option;
  }

type category' = [
  | `Data of string
  | `Domain of Uri.t
]

let make_category ~pos (l : [< category' ] list) =
  let data = match find (function `Data _ -> true | _ -> false) l with
    | Some (`Data s)-> s
    | _ -> ""
  in let domain = match find (function `Domain _ -> true | _ -> false) l with
    | Some (`Domain d) -> Some d
    | _ -> None
  in
  `Category({ data; domain; } : category )

let category_of_xml =
  let attr_producer =
    [ ("domain", (fun ~xmlbase a -> `Domain(Uri.of_string a))) ] in
  let leaf_producer ~xmlbase pos data = `Data data in
  generate_catcher ~attr_producer ~leaf_producer make_category

let category_of_xml' =
  let attr_producer = [ ("domain", (fun ~xmlbase a -> `Domain a)) ] in
  let leaf_producer ~xmlbase pos data = `Data data in
  generate_catcher ~attr_producer ~leaf_producer (fun ~pos x -> `Category x)

type ('url, 'length, 'mime) enclosure =
  {
    url    : 'url;    (* Uri.t *)
    length : 'length; (* int *)
    mime   : 'mime;   (* string *)
  }

type enclosure' = [
  | `URL of Uri.t
  | `Length of string
  | `Mime of string
]

let make_enclosure :
  type url length mime. Xmlm.pos ->
                        (url, length, mime) Relax.enclosure ->
                        [< enclosure'] list ->
                        [ `Enclosure of (url, length, mime) enclosure ] =
  fun pos relax l ->
  let url = match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> relax.Relax.url ~pos (Some u)
    | _ -> relax.Relax.url ~pos None
  in
  let length = match find (function `Length _ -> true | _ -> false) l with
    | Some (`Length l) -> relax.Relax.length ~pos (Some l)
    | _ -> relax.Relax.length ~pos None
  in
  let mime = match find (function `Mime _ -> true | _ -> false) l with
    | Some (`Mime m) -> relax.Relax.mime ~pos (Some m)
    | _ -> relax.Relax.mime ~pos None
  in
  `Enclosure ({ url; length; mime; } : (url, length, mime) enclosure)

let make_enclosure
  : type url length mime.
    pos:Xmlm.pos ->
    (url, length, mime) Relax.enclosure ->
    [< enclosure'] list ->
    [ `Enclosure of (url, length, mime) enclosure ]
  = fun ~pos relax l -> make_enclosure pos relax l

let enclosure_of_xml relax_enclosure =
  let attr_producer = [
    ("url", url_of_xml);
    ("length", (fun ~xmlbase a -> `Length a));
    ("type", (fun ~xmlbase a -> `Mime a));
  ] in
  generate_catcher ~attr_producer (fun ~pos -> make_enclosure ~pos relax_enclosure)

let enclosure_of_xml' =
  let attr_producer = [
    ("url", url_of_xml');
    ("length", (fun ~xmlbase a -> `Length a));
    ("type", (fun ~xmlbase a -> `Mime a));
  ] in
  generate_catcher ~attr_producer (fun ~pos x -> `Enclosure x)

type guid =
  {
    data      : Uri.t; (* Uri.t, must be uniq *)
    permalink : bool;  (* default true *)
  }

type guid' = [
  | `Data of Uri.t option * string
  | `Permalink of string
]

(* Some RSS2 server output <guid isPermaLink="false"></guid> ! *)
let make_guid ~pos (l : [< guid' ] list) =
  let permalink = match find (function `Permalink _ -> true | _ -> false) l with
    | Some (`Permalink b) ->
      (try bool_of_string b with exn -> false)
      (* XXX: it's possible to fail, in this case, we consider permalink = true. *)
    | _ -> true (* cf. RFC *)
  in
  match find (function `Data _ -> true | _ -> false) l with
  | Some (`Data(xmlbase, u)) ->
     if u = "" then `Guid None
     else
       (* When the GUID is declared as a permlink, resolve it using xml:base *)
       let data =
         if permalink then XML.resolve ~xmlbase (Uri.of_string u)
         else Uri.of_string u in
       `Guid(Some({ data;  permalink } : guid))
  | _ ->
     `Guid None
(* XXX: no relax for GUID, this function should not fail. *)

let guid_of_xml, guid_of_xml' =
  let attr_producer = [ ("isPermaLink", (fun ~xmlbase a -> `Permalink a)); ] in
  let leaf_producer ~xmlbase pos data = `Data(xmlbase, data) in
  generate_catcher ~attr_producer ~leaf_producer make_guid,
  generate_catcher ~attr_producer ~leaf_producer (fun ~pos x -> `Guid x)

type ('data, 'url) source =
  {
    data : 'data; (* string *)
    url  : 'url;  (* Uri.t *)
  }

type source' = [
  | `Data of string
  | `URL of Uri.t
]

let make_source :
  type data url. Xmlm.pos ->
                 (data, url) Relax.source ->
                 [< source' ] list ->
                 [ `Source of (data, url) source ] =
  fun pos relax l ->
  let data = match find (function `Data _ -> true | _ -> false) l with
    | Some (`Data s) -> relax.Relax.data ~pos (Some s)
    | _ -> relax.Relax.data ~pos None
  in
  let url = match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> relax.Relax.url ~pos (Some u)
    | _ -> relax.Relax.url ~pos None
  in
  `Source ({ data; url; } : (data, url) source)

let make_source
  : type data url.
    pos:Xmlm.pos ->
    (data, url) Relax.source ->
    [< source'] list ->
    [ `Source of (data, url) source ]
  = fun ~pos relax l -> make_source pos relax l

let source_of_xml relax_source =
  let attr_producer = [ ("url", url_of_xml) ] in
  let leaf_producer ~xmlbase pos data = `Data data in
  generate_catcher ~attr_producer ~leaf_producer (fun ~pos -> make_source ~pos relax_source)

let source_of_xml' =
  let attr_producer = [ ("url", url_of_xml') ] in
  let leaf_producer ~xmlbase pos data = `Data data in
  generate_catcher ~attr_producer ~leaf_producer (fun ~pos x -> `Source x)

(* XXX: lolavicecompliké *)
type ('story,
      'url_enclosure, 'length_enclosure, 'mime_enclosure,
      'data_source, 'url_source) item =
  {
    story     : 'story;
    content   : Uri.t option * string; (* default: (None, "") ? *)
    link      : Uri.t option;
    author    : string option; (* e-mail *)
    category  : category option;
    comments  : Uri.t option;
    enclosure : ('url_enclosure, 'length_enclosure, 'mime_enclosure) enclosure option;
    guid      : guid option;
    pubDate   : Date.t option; (* date *)
    source    : ('data_source, 'url_source) source option;
  }

type ('url_enclosure, 'length_enclosure, 'mime_enclosure,
      'data_source, 'url_source) item' = [
  | `Title of string
  | `Description of Uri.t option * string (* xmlbase, description *)
  | `Content of Uri.t option * string
  | `Link of Uri.t option
  | `Author of string (* e-mail *)
  | `Category of category
  | `Comments of Uri.t
  | `Enclosure of ('url_enclosure, 'length_enclosure, 'mime_enclosure) enclosure
  | `Guid of guid option
  | `PubDate of Date.t
  | `Source of ('data_source, 'url_source) source
]

let make_item :
  type story.
  Xmlm.pos ->
  (story,
   'url_enclosure, 'length_enclosure, 'mime_enclosure,
   'data_source, 'url_source) Relax.item ->
  [< ('url_enclosure, 'length_enclosure, 'mime_enclosure,
      'data_source, 'url_source) item'] list ->
  [ `Item of (story,
              'url_enclosure, 'length_enclosure, 'mime_enclosure,
              'data_source, 'url_source) item ] =
  fun pos relax l ->
  let story : story = match
      find (function `Title _ -> true | _ -> false) l,
      find (function `Description _ -> true | _ -> false) l
    with
    | Some (`Title t), Some (`Description(x, d)) ->
      (relax.Relax.story ~pos (Some t) (Some (x, d)) : story)
    | Some (`Title t), _ ->
      (relax.Relax.story ~pos (Some t) None : story)
    | _, Some (`Description(x, d)) ->
      (relax.Relax.story ~pos None (Some (x, d)) : story)
    | _, _ ->
      (relax.Relax.story ~pos None None : story)
  in
  let content = match find (function `Content _ -> true | _ -> false) l with
    | Some(`Content(x, c)) -> x, c
    | _ -> (None, "") in
  let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link l) -> l
    | _ -> None
  in
  let author = match find (function `Author _ -> true | _ -> false) l with
    | Some (`Author a) -> Some a
    | _ -> None
  in
  let category = match find (function `Category _ -> true | _ -> false) l with
    | Some (`Category c) -> Some c
    | _ -> None
  in
  let comments = match find (function `Comments _ -> true | _ -> false) l with
    | Some (`Comments c) -> Some c
    | _ -> None
  in
  let enclosure = match find (function `Enclosure _ -> true | _ -> false) l with
    | Some (`Enclosure e) -> Some e
    | _ -> None
  in
  let guid = match find (function `Guid _ -> true | _ -> false) l with
    | Some (`Guid g) -> g
    | _ -> None
  in
  let pubDate = match find (function `PubDate _ -> true | _ -> false) l with
    | Some (`PubDate p) -> Some p
    | _ -> None
  in
  let source = match find (function `Source _ -> true | _ -> false) l with
    | Some (`Source s) -> Some s
    | _ -> None
  in
  `Item ({ story;
           content;
           link;
           author;
           category;
           comments;
           enclosure;
           guid;
           pubDate;
           source; } : (story,
                        'url_enclosure, 'length_enclosure, 'mime_enclosure,
                        'data_source, 'url_source) item)

let make_item
  : type story.
    pos:Xmlm.pos ->
    (story,
     'url_enclosure, 'length_enclosure, 'mime_enclosure,
     'data_source, 'url_source) Relax.item ->
    [< ('url_encosure, 'length_enclosure, 'mime_enclosure,
        'data_source, 'url_source) item'] list ->
    [ `Item of (story,
                'url_enclosure, 'length_enclosure, 'mime_enclosure,
                'data_sourc, 'url_source) item ]
  = fun ~pos relax l -> make_item pos relax l

let item_title_of_xml ~xmlbase (pos, tag, datas) =
  try `Title(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <title> MUST be \
                             a non-empty string"))

let item_description_of_xml ~xmlbase (pos, tag, datas) =
  `Description(xmlbase, try get_leaf datas
                        with Not_found -> "")

let item_content_of_xml ~xmlbase (pos, tag, datas) =
  `Content(xmlbase, try get_leaf datas with Not_found -> "")

let item_link_of_xml ~xmlbase (pos, tag, datas) =
  `Link(try Some(XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
        with Not_found -> None)

let item_author_of_xml ~xmlbase (pos, tag, datas) =
  try `Author(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <author> MUST be \
                             a non-empty string"))

let item_comments_of_xml ~xmlbase (pos, tag, datas) =
  try `Comments(XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <comments> MUST be \
                             a non-empty string"))

let item_pubdate_of_xml ~xmlbase (pos, tag, datas) =
  try `PubDate(Date.of_rfc822 (get_leaf datas))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <pubDate> MUST be \
                             a non-empty string"))

let item_namespaces = [""; "http://purl.org/rss/1.0/modules/content/"]

let item_of_xml
  : type url_enclosure length_enclosure mime_enclosure data_source url_source.
    ('story, url_enclosure, length_enclosure, mime_enclosure, data_source, url_source) Relax.item ->
    (xmlbase:Uri.t option -> Syndic_common.XML.node ->
      [ `Item of ('story, url_enclosure, length_enclosure, mime_enclosure, data_source, url_source) item])
  = fun relax_item ->
  let enclosure_of_xml
    :> xmlbase:Uri.t option -> Syndic_common.XML.node ->
       (url_enclosure, length_enclosure, mime_enclosure, data_source, url_source) item'
    = enclosure_of_xml relax_item.Relax.enclosure in
  let source_of_xml
    :> xmlbase:Uri.t option -> Syndic_common.XML.node ->
       (url_enclosure, length_enclosure, mime_enclosure, data_source, url_source) item'
    = source_of_xml relax_item.Relax.source in
  let data_producer
    : (string * (xmlbase:Uri.t option -> Syndic_common.XML.node ->
                 (url_enclosure, length_enclosure, mime_enclosure,
                  data_source, url_source) item')) list
    =
  [
    ("title", item_title_of_xml);
    ("description", item_description_of_xml);
    (* <content:encoded> where
       xmlns:content="http://purl.org/rss/1.0/modules/content/" *)
    ("encoded", item_content_of_xml);
    ("link", item_link_of_xml);
    ("author", item_author_of_xml);
    ("category", category_of_xml);
    ("comments", item_comments_of_xml);
    ("enclosure", enclosure_of_xml);
    ("guid", guid_of_xml);
    ("pubDate", item_pubdate_of_xml);
    ("source", source_of_xml);
  ]
  in
  generate_catcher ~data_producer (fun ~pos -> make_item ~pos relax_item)
                   ~namespaces:item_namespaces

let item_of_xml' =
  let data_producer = [
    ("title", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Title a));
    ("description", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Description a));
    ("encoded", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Content a));
    ("link", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Link(xmlbase, a)));
    ("author", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Author a));
    ("category", category_of_xml');
    ("comments", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Comments a));
    ("enclosure", enclosure_of_xml');
    ("guid", guid_of_xml');
    ("pubdate", dummy_of_xml ~ctor:(fun ~xmlbase a -> `PubDate a));
    ("source", source_of_xml');
  ] in
  generate_catcher ~data_producer (fun ~pos x -> `Item x)
                   ~namespaces:item_namespaces

type ('title, 'link, 'description,
      'uri_cloud, 'procedure_cloud, 'protocol_cloud,
      'url_image, 'title_image, 'link_image,
      'title_textinput, 'description_textinput, 'name_textinput, 'link_textinput,
      'story_item,
      'url_enclosure, 'length_enclosure, 'mime_enclosure,
      'data_source, 'url_source) channel =
  {
    title          : 'title;       (* string *)
    link           : 'link;        (* Uri.t *)
    description    : 'description; (* string *)
    language       : string option;
    copyright      : string option;
    managingEditor : string option;
    webMaster      : string option;
    pubDate        : Date.t option;
    lastBuildDate  : Date.t option;
    category       : string option;
    generator      : string option;
    docs           : Uri.t option;
    cloud          : ('uri_cloud, 'procedure_cloud, 'protocol_cloud) cloud option;
    ttl            : int option;
    image          : ('url_image, 'title_image, 'link_image) image option;
    rating         : int option;
    textInput      : ('title_textinput, 'description_textinput, 'name_textinput, 'link_textinput) textinput option;
    skipHours      : int option;
    skipDays       : int option;
    items          : ('story_item, 'url_enclosure, 'length_enclosure, 'mime_enclosure, 'data_source, 'url_source) item list;
  }

type ('uri_cloud, 'procedure_cloud, 'protocol_cloud,
      'url_image, 'title_image, 'link_image,
      'title_textinput, 'description_textinput, 'name_textinput, 'link_textinput,
      'story_item,
      'url_enclosure, 'length_enclosure, 'mime_enclosure,
      'data_source, 'url_source) channel' = [
  | `Title of string
  | `Link of Uri.t
  | `Description of string
  | `Language of string
  | `Copyright of string
  | `ManagingEditor of string
  | `WebMaster of string
  | `PubDate of Date.t
  | `LastBuildDate of Date.t
  | `Category of string
  | `Generator of string
  | `Docs of Uri.t
  | `Cloud of ('uri_cloud, 'procedure_cloud, 'protocol_cloud) cloud
  | `TTL of int
  | `Image of ('url_image, 'title_image, 'link_image) image
  | `Rating of int
  | `TextInput of ('title_textinput, 'description_textinput, 'name_textinput, 'link_textinput) textinput
  | `SkipHours of int
  | `SkipDays of int
  | `Item of ('story_item, 'url_enclosure, 'length_enclosure, 'mime_enclosure, 'data_source, 'url_source) item
]

let make_channel :
  type title link description.
  Xmlm.pos ->
  (title, link, description,
   'domain_cloud, 'port_cloud, 'path_cloud, 'uri_cloud, 'procedure_cloud, 'protocol_cloud,
   'url_image, 'title_image, 'link_image,
   'title_textinput, 'description_textinput, 'name_textinput, 'link_textinput,
   'story_item,
   'url_enclosure, 'length_enclosure, 'mime_enclosure,
   'data_source, 'url_source) Relax.channel ->
  [< ('uri_cloud, 'procedure_cloud, 'protocol_cloud,
      'url_image, 'title_image, 'link_image,
      'title_textinput, 'description_textinput, 'name_textinput, 'link_textinput,
      'story_item,
      'url_enclosure, 'length_enclosure, 'mime_enclosure,
      'data_source, 'url_source) channel' ] list ->
  (title, link, description,
   'uri_cloud, 'procedure_cloud, 'protocol_cloud,
   'url_image, 'title_image, 'link_image,
   'title_textinput, 'description_textinput, 'name_textinput, 'link_textinput,
   'story_item,
   'url_enclosure, 'length_enclosure, 'mime_enclosure,
   'data_source, 'url_source) channel =
  fun pos relax l ->
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> relax.Relax.title ~pos (Some t)
    | _ -> relax.Relax.title ~pos None
  in
  let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link l) -> relax.Relax.link ~pos (Some l)
    | _ -> relax.Relax.link ~pos None
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description l) -> relax.Relax.description ~pos (Some l)
    | _ -> relax.Relax.description ~pos None
  in
  let language = match find (function `Language _ -> true | _ -> false) l with
    | Some (`Language a) -> Some a
    | _ -> None
  in
  let copyright = match find (function `Copyright _ -> true | _ -> false) l with
    | Some (`Copyright a) -> Some a
    | _ -> None
  in
  let managingEditor =
    match find (function `ManagingEditor _ -> true | _ -> false) l with
    | Some (`ManagingEditor a) -> Some a
    | _ -> None
  in
  let webMaster = match find (function `WebMaster _ -> true | _ -> false) l with
    | Some (`WebMaster a) -> Some a
    | _ -> None
  in
  let pubDate = match find (function `PubDate _ -> true | _ -> false) l with
    | Some (`PubDate a) -> Some a
    | _ -> None
  in
  let lastBuildDate =
    match find (function `LastBuildDate _ -> true | _ -> false) l with
    | Some (`LastBuildDate a) -> Some a
    | _ -> None
  in
  let category = match find (function `Category _ -> true | _ -> false) l with
    | Some (`Category a) -> Some a
    | _ -> None
  in
  let generator = match find (function `Generator _ -> true | _ -> false) l with
    | Some (`Generator a) -> Some a
    | _ -> None
  in
  let docs = match find (function `Docs _ -> true | _ -> false) l with
    | Some (`Docs a) -> Some a
    | _ -> None
  in
  let cloud = match find (function `Cloud _ -> true | _ -> false) l with
    | Some (`Cloud a) -> Some a
    | _ -> None
  in
  let ttl = match find (function `TTL _ -> true | _ -> false) l with
    | Some (`TTL a) -> Some a
    | _ -> None
  in
  let image = match find (function `Image _ -> true | _ -> false) l with
    | Some (`Image a) -> Some a
    | _ -> None
  in
  let rating = match find (function `Rating _ -> true | _ -> false) l with
    | Some (`Rating a) -> Some a
    | _ -> None
  in
  let textInput = match find (function `TextInput _ -> true | _ -> false) l with
    | Some (`TextInput a) -> Some a
    | _ -> None
  in
  let skipHours = match find (function `SkipHours _ -> true | _ -> false) l with
    | Some (`SkipHours a) -> Some a
    | _ -> None
  in
  let skipDays = match find (function `SkipDays _ -> true | _ -> false) l with
    | Some (`SkipDays a) -> Some a
    | _ -> None
  in
  let items = List.fold_left
      (fun acc -> function `Item x -> x :: acc | _ -> acc) [] l in
  ({ title;
     link;
     description;
     language;
     copyright;
     managingEditor;
     webMaster;
     pubDate;
     lastBuildDate;
     category;
     generator;
     docs;
     cloud;
     ttl;
     image;
     rating;
     textInput;
     skipHours;
     skipDays;
     items; } : (title, link, description,
                 'uri_cloud, 'procedure_cloud, 'protocol_cloud,
                 'url_image, 'title_image, 'link_image,
                 'title_textinput, 'description_textinput, 'name_textinput, 'link_textinput,
                 'story_item,
                 'url_enclosure, 'length_enclosure, 'mime_enclosure,
                 'data_source, 'url_source) channel)

let channel_title_of_xml ~xmlbase (pos, tag, datas) =
  try `Title(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <title> MUST be \
                             a non-empty string"))

let channel_description_of_xml ~xmlbase (pos, tag, datas) =
  `Description(try get_leaf datas
               with Not_found -> "")

let channel_link_of_xml ~xmlbase (pos, tag, datas) =
  try `Link(XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <link> MUST be \
                             a non-empty string"))

let channel_language_of_xml ~xmlbase (pos, tag, datas) =
  try `Language(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <language> MUST be \
                             a non-empty string"))

let channel_copyright_of_xml ~xmlbase (pos, tag, datas) =
  try `Copyright(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <copyright> MUST be \
                             a non-empty string"))

let channel_managingeditor_of_xml ~xmlbase (pos, tag, datas) =
  try `ManagingEditor(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <managingEditor> MUST be \
                             a non-empty string"))

let channel_webmaster_of_xml ~xmlbase (pos, tag, datas) =
  try `WebMaster(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <webMaster> MUST be \
                             a non-empty string"))

let channel_pubdate_of_xml ~xmlbase (pos, tag, datas) =
  try `PubDate(Date.of_rfc822 (get_leaf datas))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <pubDate> MUST be \
                             a non-empty string"))

let channel_lastbuilddate_of_xml ~xmlbase (pos, tag, datas) =
  try `LastBuildDate(Date.of_rfc822 (get_leaf datas))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <lastBuildDate> MUST be \
                             a non-empty string"))

let channel_category_of_xml ~xmlbase (pos, tag, datas) =
  try `Category(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <category> MUST be \
                             a non-empty string"))

let channel_generator_of_xml ~xmlbase (pos, tag, datas) =
  try `Generator(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <generator> MUST be \
                             a non-empty string"))

let channel_docs_of_xml ~xmlbase (pos, tag, datas) =
  try `Docs(XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <docs> MUST be \
                             a non-empty string"))

let channel_ttl_of_xml ~xmlbase (pos, tag, datas) =
  try `TTL(int_of_string (get_leaf datas))
  with _ -> raise (Error.Error (pos,
                            "The content of <ttl> MUST be \
                             a non-empty string representing an integer"))

let channel_rating_of_xml ~xmlbase (pos, tag, datas) =
  try `Rating(int_of_string (get_leaf datas))
  with _ -> raise (Error.Error (pos,
                            "The content of <rating> MUST be \
                             a non-empty string representing an integer"))

let channel_skipHours_of_xml ~xmlbase (pos, tag, datas) =
  try `SkipHours(int_of_string (get_leaf datas))
  with _ -> raise (Error.Error (pos,
                            "The content of <skipHours> MUST be \
                             a non-empty string representing an integer"))

let channel_skipDays_of_xml ~xmlbase (pos, tag, datas) =
  try `SkipDays(int_of_string (get_leaf datas))
  with _ -> raise (Error.Error (pos,
                            "The content of <skipDays> MUST be \
                             a non-empty string representing an integer"))

let make_channel
  : type title link description.
    pos:Xmlm.pos ->
    (title, link, description,
     'domain_cloud, 'port_cloud, 'path_cloud, 'uri_cloud, 'procedure_cloud, 'protocol_cloud,
     'url_image, 'title_image, 'link_image,
     'title_textinput, 'description_textinput, 'name_textinput, 'link_textinput,
     'story_item,
     'url_enclosure, 'length_enclosure, 'mime_enclosure,
     'data_source, 'url_source) Relax.channel ->
    [< ('uri_cloud, 'procedure_cloud, 'protocol_cloud,
        'url_image, 'title_image, 'link_image,
        'title_textinput, 'description_textinput, 'name_textinput, 'link_textinput,
        'story_item,
        'url_enclosure, 'length_enclosure, 'mime_enclosure,
        'data_source, 'url_source) channel' ] list ->
    (title, link, description,
     'uri_cloud, 'procedure_cloud, 'protocol_cloud,
     'url_image, 'title_image, 'link_image,
     'title_textinput, 'description_textinput, 'name_textinput, 'link_textinput,
     'story_item,
     'url_enclosure, 'length_enclosure, 'mime_enclosure,
     'data_source, 'url_source) channel
  = fun ~pos relax l -> make_channel pos relax l

let channel_of_xml
  : type title link description
         domain_cloud port_cloud path_cloud uri_cloud procedure_cloud protocol_cloud
         url_image title_image link_image
         title_textinput description_textinput name_textinput link_textinput
         story_item
         url_enclosure length_enclosure mime_enclosure
         data_source url_source.
    (title, link, description,
     domain_cloud, port_cloud, path_cloud, uri_cloud, procedure_cloud, protocol_cloud,
     url_image, title_image, link_image,
     title_textinput, description_textinput, name_textinput, link_textinput,
     story_item,
     url_enclosure, length_enclosure, mime_enclosure,
     data_source, url_source) Relax.channel ->
    (xmlbase:Uri.t option -> Syndic_common.XML.node ->
     (title, link, description,
      uri_cloud, procedure_cloud, protocol_cloud,
      url_image, title_image, link_image,
      title_textinput, description_textinput, name_textinput, link_textinput,
      story_item,
      url_enclosure, length_enclosure, mime_enclosure,
      data_source, url_source) channel)
  = fun relax_channel ->
  let cloud_of_xml
    :> xmlbase:Uri.t option -> Syndic_common.XML.node ->
       (uri_cloud, procedure_cloud, protocol_cloud,
        url_image, title_image, link_image,
        title_textinput, description_textinput, name_textinput, link_textinput,
        story_item,
        url_enclosure, length_enclosure, mime_enclosure,
        data_source, url_source) channel'
    = cloud_of_xml relax_channel.Relax.cloud in
  let image_of_xml
    :> xmlbase:Uri.t option -> Syndic_common.XML.node ->
       (uri_cloud, procedure_cloud, protocol_cloud,
        url_image, title_image, link_image,
        title_textinput, description_textinput, name_textinput, link_textinput,
        story_item,
        url_enclosure, length_enclosure, mime_enclosure,
        data_source, url_source) channel'
    = image_of_xml relax_channel.Relax.image in
  let textinput_of_xml
    :> xmlbase:Uri.t option -> Syndic_common.XML.node ->
       (uri_cloud, procedure_cloud, protocol_cloud,
        url_image, title_image, link_image,
        title_textinput, description_textinput, name_textinput, link_textinput,
        story_item,
        url_enclosure, length_enclosure, mime_enclosure,
        data_source, url_source) channel'
    = textinput_of_xml relax_channel.Relax.textInput in
  let item_of_xml
    :> xmlbase:Uri.t option -> Syndic_common.XML.node ->
       (uri_cloud, procedure_cloud, protocol_cloud,
        url_image, title_image, link_image,
        title_textinput, description_textinput, name_textinput, link_textinput,
        story_item,
        url_enclosure, length_enclosure, mime_enclosure,
        data_source, url_source) channel'
    = item_of_xml relax_channel.Relax.item in
  let data_producer = [
    ("title", channel_title_of_xml);
    ("link", channel_link_of_xml);
    ("description", channel_description_of_xml);
    ("Language", channel_language_of_xml);
    ("copyright", channel_copyright_of_xml);
    ("managingeditor", channel_managingeditor_of_xml);
    ("webmaster", channel_webmaster_of_xml);
    ("pubdate", channel_pubdate_of_xml);
    ("lastbuilddate", channel_lastbuilddate_of_xml);
    ("category", channel_category_of_xml);
    ("generator", channel_generator_of_xml);
    ("docs", channel_docs_of_xml);
    ("cloud", cloud_of_xml);
    ("ttl", channel_ttl_of_xml);
    ("image", image_of_xml);
    ("rating", channel_rating_of_xml);
    ("textinput", textinput_of_xml);
    ("skiphours", channel_skipHours_of_xml);
    ("skipdays", channel_skipDays_of_xml);
    ("item", item_of_xml);
  ] in
  generate_catcher ~data_producer (fun ~pos -> make_channel ~pos relax_channel)

let channel_of_xml' =
  let data_producer = [
    ("title", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Title a));
    ("link", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Link(xmlbase, a)));
    ("description", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Description a));
    ("Language", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Language a));
    ("copyright", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Copyright a));
    ("managingeditor", dummy_of_xml ~ctor:(fun ~xmlbase a -> `ManagingEditor a));
    ("webmaster", dummy_of_xml ~ctor:(fun ~xmlbase a -> `WebMaster a));
    ("pubdate", dummy_of_xml ~ctor:(fun ~xmlbase a -> `PubDate a));
    ("lastbuilddate", dummy_of_xml ~ctor:(fun ~xmlbase a -> `LastBuildDate a));
    ("category", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Category a));
    ("generator", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Generator a));
    ("docs", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Docs a));
    ("cloud", cloud_of_xml');
    ("ttl", dummy_of_xml ~ctor:(fun ~xmlbase a -> `TTL a));
    ("image", image_of_xml');
    ("rating", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Rating a));
    ("textinput", textinput_of_xml');
    ("skiphours", dummy_of_xml ~ctor:(fun ~xmlbase a -> `SkipHours a));
    ("skipdays", dummy_of_xml ~ctor:(fun ~xmlbase a -> `SkipDays a));
    ("item", item_of_xml');
  ] in
  generate_catcher ~data_producer (fun ~pos x -> x)

module Strict =
struct
  type nonrec item = (story, Uri.t, int, string, string, Uri.t) item
  type nonrec channel = (string, Uri.t, string,
                         Uri.t, string, string,
                         Uri.t, string, Uri.t,
                         string, string, string, Uri.t,
                         story,
                         Uri.t, int, string,
                         string, Uri.t) channel
end

let find_channel l =
  find (function XML.Node(pos, tag, data) -> tag_is tag "channel"
                | XML.Data _ -> false) l

let parse ?xmlbase input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, data) ->
     if tag_is tag "channel" then
       channel_of_xml ~xmlbase Relax.channel (pos, tag, data)
     else (
       match find_channel data with
       | Some(XML.Node(p, t, d)) -> channel_of_xml ~xmlbase Relax.channel (p, t, d)
       | Some(XML.Data _)
       | _ -> raise (Error.Error ((0, 0),
                              "document MUST contains exactly one \
                               <channel> element")))
  | _ -> raise (Error.Error ((0, 0),
                         "document MUST contains exactly one \
                          <channel> element"))

let read ?xmlbase fname =
  let fh = open_in fname in
  try
    let x = parse ?xmlbase (Xmlm.make_input (`Channel fh)) in
    close_in fh;
    x
  with e ->
    close_in fh;
    raise e

type uri = Uri.t option * string

let unsafe ?xmlbase input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, data) ->
     if tag_is tag "channel" then
       `Channel (channel_of_xml' ~xmlbase (pos, tag, data))
     else (match find_channel data with
           | Some(XML.Node(p, t, d)) ->
              `Channel (channel_of_xml' ~xmlbase (p, t, d))
           | Some(XML.Data _) | None -> `Channel [])
  | _ -> `Channel []


(* Conversion to Atom *)

let map_option o f = match o with
  | None -> None
  | Some v -> Some(f v)

(* Assume ASCII or a superset like UTF-8. *)
let valid_local_part =
  let is_valid c =
    let c = Char.unsafe_chr c in
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')
    || c = '.' (* shouldn't be the 1st char and not appear twice consecutively *)
    || c = '!' || c = '#' || c = '$' || c = '%' || c = '&' || c = '\''
    || c = '*' || c = '+' || c = '-' || c = '/' || c = '=' || c = '?'
    || c = '^' || c = '_' || c = '`' || c = '{' || c = '|' || c =  '}'
    || c = '~' in
  Array.init 256 is_valid

let is_valid_local_part c =
  valid_local_part.(Char.code c)

let valid_domain_part =
  let is_valid c =
    let c = Char.unsafe_chr c in
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')
    || c = '.' || c = '.' in
  Array.init 256 is_valid

let is_valid_domain_part c =
  valid_domain_part.(Char.code c)

(* Valid range [s.[i]], [i0 ≤ i < i1]. *)
let sub_no_braces s i0 i1 =
  let i0 = if s.[i0] = '(' then i0 + 1 else i0 in
  let i1 = if s.[i1 - 1] = ')' then i1 - 1 else i1 in
  String.sub s i0 (i1 - i0)

(* The item author sometimes contains the name and email under the form
   "name <email>" or "email (name)".  Try to extract both compnents. *)
let extract_name_email a =
  try
    let i = String.index a '@' in
    let len = String.length a in
    let i0 = ref(i-1) in
    while !i0 >= 0 && is_valid_local_part a.[!i0] do decr i0 done;
    incr i0; (* !i0 >= 0 is the first char of the possible email. *)
    let i1 = ref(i+1) in
    while !i1 < len && is_valid_domain_part a.[!i1] do incr i1 done;
    if !i0 < i && i + 1 < !i1 then (
      let email = String.sub a !i0 (!i1 - !i0) in
      if !i0 > 0 && a.[!i0 - 1] = '<' then decr i0;
      if !i1 < len && a.[!i1] = '>' then incr i1;
      let name =
        if !i0 <= 0 then
          if !i1 >= len then email (* no name *)
          else sub_no_braces a !i1 len
        else (* !i0 > 0 *)
          let name0 = String.trim(String.sub a 0 !i0) in
          if !i1 >= len then name0
          else name0 ^ String.sub a !i1 (len - !i1)
      in
      (name, Some email)
    )
    else (a, None)
  with Not_found ->
    (a, None)

let looks_like_a_link u =
  (Uri.scheme u = Some "http" || Uri.scheme u = Some "https")
  && (match Uri.host u with None | Some "" -> false | Some _ -> true)

let entry_of_item ch_link ch_updated (it: Strict.item) : Atom.entry =
  let author = match it.author with
    | Some a ->
       let name, email = extract_name_email a in
       { Atom.name = name;  uri = None;  email }
    | None ->
       (* If no author is specified for the item, there is little one
          can do just using the RSS2 feed.  The user will have to set
          it using Atom convenience functions. *)
       { Atom.name = "";  uri = None;  email = None } in
  let categories =
    match it.category with
    | Some c -> [ { Atom.term = c.data;
                   scheme = map_option c.domain (fun d -> d);
                   label = None } ]
    | None -> [] in
  let (title: Atom.title), content = match it.story with
    | All(t, xmlbase, d) ->
       let content = match it.content with
         | (_, "") -> if d = "" then None else Some(Atom.Html(xmlbase, d))
         | (x, c) -> Some(Atom.Html(x, c)) in
       Atom.Text t, content
    | Title t ->
       let content = match it.content with
         | (_, "") -> None
         | (x, c) -> Some(Atom.Html(x, c)) in
       Atom.Text t, content
    | Description(xmlbase, d) ->
       let content = match it.content with
         | (_, "") -> if d = "" then None else Some(Atom.Html(xmlbase, d))
         | (x, c) -> Some(Atom.Html(x, c)) in
       Atom.Text "", content in
  let id = match it.guid with
    | Some g ->
       if g.permalink || looks_like_a_link g.data then g.data
       else
         let d = Digest.to_hex (Digest.string(Uri.to_string g.data)) in
         Uri.with_fragment ch_link (Some d)
    | None ->
       (* The [it.link] may not be a permanent link and may also be
          used by other items.  We use a digest to make it unique. *)
       let link = match it.link with
         | Some l -> l
         | None -> ch_link in
       let s = match it.story with
         | All(t, _, d) -> t ^ d
         | Title t -> t
         | Description(_, d) -> d in
       let d = Digest.to_hex (Digest.string s) in
       Uri.with_fragment link (Some d) in
  let links = match it.guid, it.link with
    | Some g, _ when g.permalink -> [Atom.link g.data ~rel:Atom.Alternate]
    | _, Some l -> [ Atom.link l ~rel:Atom.Alternate ]
    | Some g, _ -> (* Sometimes the guid sets [l.permalink = false] but is
                     nonetheless the only URI we have. *)
       if looks_like_a_link g.data then [Atom.link g.data ~rel:Atom.Alternate]
       else []
    | _, None -> [] in
  let links = match it.comments with
    | Some l -> { Atom.href = l;  rel = Atom.Related;
                 type_media = None;  hreflang = None;  title = "";
                 length = None }
               :: links
    | None -> links in
  let links = match it.enclosure with
    | Some e -> { Atom.href = e.url;  rel = Atom.Enclosure;
                 type_media = Some e.mime;
                 hreflang = None;  title = "";  length = Some e.length }
               :: links
    | None -> links in
  let source = match it.source with
    | Some s ->
       Some
         { Atom.authors = [author]; (* Best guess *)
           categories = [];
           contributors = [];
           generator = None;
           icon = None;
           id = ch_link; (* declared as the ID of the whole channel *)
           links = [ { Atom.href = s.url;  rel = Atom.Related;
                       type_media = None;  hreflang = None;  title = "";
                       length = None} ];
           logo = None;
           rights = None;
           subtitle = None;
           title = Atom.Text s.data;
           updated = None }
    | None -> None in
  { Atom.
    authors = (author, []);
    categories;
    content;
    contributors = [];
    id;
    links;
    published = None;
    rights = None;
    source;
    summary = None;
    title;
    updated = (match it.pubDate with
               | Some d -> d
               | None -> ch_updated);
  }

let more_recent_of_item date (it: Strict.item) =
  match date, it.pubDate with
  | _, None -> date
  | None, Some _ -> it.pubDate
  | Some d, Some di -> if Date.compare d di >= 0 then date else it.pubDate

let max_date_opt d = function
  | None -> d
  | Some d' -> Date.max d d'

let to_atom ?self (ch: Strict.channel) : Atom.feed =
  let contributors = match ch.webMaster with
    | Some p -> [ { Atom.name = "Webmaster";  uri = None;  email = Some p } ]
    | None -> [] in
  let contributors = match ch.managingEditor with
    | Some p -> { Atom.name = "Managing Editor";  uri = None;  email = Some p }
               :: contributors
    | None -> contributors in
  let links = [ { Atom.href = ch.link;  rel = Atom.Related;
                  type_media = Some "text/html";  hreflang = None;
                  title = ch.title;  length = None } ] in
  let links = match self with
    | Some self -> { Atom.href = self;  rel = Atom.Self;
                    type_media = Some "application/rss+xml";  hreflang = None;
                    title = ch.title;  length = None
                  } :: links
    | None -> links in
  let updated =
    match List.fold_left more_recent_of_item None ch.items with
    | None -> max_date_opt Date.epoch ch.lastBuildDate
    | Some d -> max_date_opt d ch.lastBuildDate in
  { Atom.authors = [];
    categories = (match ch.category with
                  | None -> []
                  | Some c -> [ { Atom.term =c;
                                 scheme = None;  label = None} ]);
    contributors;
    generator = map_option ch.generator
                           (fun g -> { Atom.content = g;
                                    version = None;  uri = None });
    icon = None;
    id = ch.link; (* FIXME: Best we can do? *)
    links;
    logo = map_option ch.image (fun i -> i.url);
    rights = map_option ch.copyright (fun c -> (Atom.Text c: Atom.rights));
    subtitle = None;
    title = Atom.Text ch.title;
    updated;
    entries = List.map (entry_of_item ch.link updated) ch.items;
  }

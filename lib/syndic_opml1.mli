(** [Syndic.Opml1]: compliant with {{:http://dev.opml.org/spec1.html} OPML
    1.0}.

    The purpose of the {i Outline Processor Markup Language}, or OPML, is to
    provide a way to exchange information between outliners and Internet
    services that can be browsed or controlled through an outliner. Outlines
    can be used for specifications, legal briefs, product plans, presentations,
    screenplays, directories, diaries, discussion groups, chat systems and
    stories. *)

module Error : module type of Syndic_error

type head =
  { title: string  (** Title of the document. *)
  ; date_created: Syndic_date.t option
        (** A date-time indicating when the document was created. *)
  ; date_modified: Syndic_date.t
        (** A date-time indicating when the document was last modified. *)
  ; owner_name: string  (** Owner of the document. *)
  ; owner_email: string  (** Email address of the owner of the document. *)
  ; expansion_state: int list
        (** A comma-separated list of line numbers that are expanded. The line
            numbers in the list tell you which headlines to expand. The order
            is important. For each element in the list, X, starting at the
            first summit, navigate flatdown X times and expand. Repeat for each
            element in the list. *)
  ; vert_scroll_state: int option
        (** A number saying which line of the outline is displayed on the top
            line of the window. This number is calculated with the expansion
            state already applied. *)
  ; window_top: int option
        (** Pixel location of the top edge of the window. *)
  ; window_left: int option
        (** Pixel location of the left edge of the window. *)
  ; window_bottom: int option
        (** Pixel location of the bottom edge of the window. *)
  ; window_right: int option
        (** Pixel location of the right edge of the window. *) }

val head :
     ?date_created:Syndic_date.t
  -> ?expansion_state:int list
  -> ?vert_scroll_state:int
  -> ?window_top:int
  -> ?window_left:int
  -> ?window_bottom:int
  -> ?window_right:int
  -> date_modified:Syndic_date.t
  -> owner_name:string
  -> owner_email:string
  -> string
  -> head
(** [head ~date_modified ~owner_name ~owner_email title] returns a head. By
    default, all optional arguments leave the corresponding fields empty. *)

type outline =
  { text: string
        (** String that's displayed when the outline is being browsed or
            edited. There is no specific limit on the length of the text
            attribute.*)
  ; typ: string option
        (** "Type" of outline. Says how other attributes of the [outline] are
            interpreted. This is application dependent. For example, for news
            feed, it is common to have "rss" as the value of this field. *)
  ; is_comment: bool
        (** Indicates whether the outline is commented or not. By convention if
            an outline is commented, all subordinate outlines are considered to
            be commented as well. *)
  ; is_breakpoint: bool
        (** Indicates whether a breakpoint is set on this outline. This
            attribute is mainly necessary for outlines used to edit scripts
            that execute. *)
  ; xml_url: Uri.t option
        (** Link to the XML data associated to this outline, typically the RSS
            feed. *)
  ; html_url: Uri.t option
        (** Link to the HTML data associated to this outline, typically the
            HTML pages rendering the news feed. *)
  ; attrs: Xmlm.attribute list
        (** Association list of additional attributes in the outline. *)
  ; outlines: outline list
        (** List of [outline] elements that are considered sub-items of the
            current outline. *) }

val outline :
     ?typ:string
  -> ?is_comment:bool
  -> ?is_breakpoint:bool
  -> ?xml_url:Uri.t
  -> ?html_url:Uri.t
  -> ?attrs:Xmlm.attribute list
  -> ?outlines:outline list
  -> string
  -> outline
(** [outline text] returns an outline.

    @param is_comment Default: [false]. @param is_breakpoint Default: [false].

    All the other parameters are bu default empty. *)

(** List of outline elements. *)
type body = outline list

type t =
  { version: string  (** The version of OPML document (should be 1.0 or 1.1) *)
  ; head: head
  ; body: body }

val parse : ?xmlbase:Uri.t -> Xmlm.input -> t
(** [parse i] takes [i] and returns an opml record which is the OCaml
    representation of the OPML document. *)

val read : ?xmlbase:Uri.t -> string -> t
(** [read fname] reads the file name [fname] and parses it. For the optional
    parameters, see {!parse}. *)

val to_xml : t -> Syndic_xml.t
(** [to_xml opml] converts the OPML document [opml] to an XML tree. *)

val output : t -> Xmlm.dest -> unit
(** [output opml dest] writes the XML tree of the OPML document [opml] to
    [dest]. *)

val write : t -> string -> unit
(** [write opml fname] writes the XML tree of the OPML document [opml] to the
    file named [fname]. *)

val of_atom : head:head -> Syndic_atom.feed list -> t
(** [of_atom ~head feeds] returns the OPML list of authors of the atom feeds.
    The [text] is the name associated to a feed, i.e. the name of the first
    author in the feed authors list or, if empty, the one of the first post. It
    is important that the feeds contain a link entry with [rel = Self] for
    the OPML document to be able to create a [xml_url] entry pointing to the
    feed.

    As a special convention, if the length of the [rel = Self] link is
    present and negative, the property [is_comment] is set to [true]. *)

(**/**)

(** An URI is given by (xmlbase, uri). The value of [xmlbase], if not [None],
    gives the base URI against which [uri] must be resolved if it is relative. *)
type uri = Uri.t option * string

val unsafe :
     ?xmlbase:Uri.t
  -> Xmlm.input
  -> [> `Opml of [> `Body of [> `Outline of ([> `Text of string
                                             | `Type of string
                                             | `IsBreakpoint of string
                                             | `IsComment of string
                                             | `Outline of 'a
                                             | `XML_url of uri
                                             | `HTML_url of uri
                                             | `Attr of string * string ]
                                             list
                                             as
                                             'a) ]
                             list
                 | `Head of [> `DateCreated of string
                            | `DateModified of string
                            | `ExpansionSate of string
                            | `OwnerEmail of string
                            | `OwnerName of string
                            | `Title of string
                            | `VertScrollState of string
                            | `WindowBottom of string
                            | `WindowLeft of string
                            | `WindowRight of string
                            | `WindowTop of string ]
                            list
                 | `Version of string ]
                 list ]
(** Analysis without verification. *)

(** @deprecated Use Syndic.Opml1.t instead. *)
type opml = t

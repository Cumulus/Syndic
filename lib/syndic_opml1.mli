
module Error : module type of Syndic_error

type head =
    {
      title : string; (** <title> is the title of the document.*)
      date_created : CalendarLib.Calendar.t; (** <dateCreated> is a date-time, indicating when the document was created.*)
      date_modified : CalendarLib.Calendar.t; (** <dateModified> is a date-time, indicating when the document was last modified.*)
      owner_name : string; (** <ownerName> is a string, the owner of the document.*)
      owner_email : string; (** <ownerEmail> is a string, the email address of the owner of the document.*)
      expansion_state : int list; (** <expansionState> is a comma-separated list of line numbers that are expanded. The line numbers in the list tell you which headlines to expand. The order is important. For each element in the list, X, starting at the first summit, navigate flatdown X times and expand. Repeat for each element in the list.*)
      vert_scroll_state : int; (** <vertScrollState> is a number, saying which line of the outline is displayed on the top line of the window. This number is calculated with the expansion state already applied.*)
      window_top : int; (** <windowTop> is a number, the pixel location of the top edge of the window.*)
      window_left : int; (** <windowLeft> is a number, the pixel location of the left edge of the window.*)
      window_bottom : int; (** <windowBottom> is a number, the pixel location of the bottom edge of the window.*)
      window_right : int (** <windowRight> is a number, the pixel location of the right edge of the window.*)
    }

type outline =
    {
      text : string option; (** text is the string of characters that's displayed when the outline is being browsed or edited. There is no specific limit on the length of the text attribute.*)
      type_ : string option; (** type is a string, it says how the other attributes of the <outline> are interpreted.*)
      is_comment : bool; (** isComment is a string, either "true" or "false", indicating whether the outline is commented or not. By convention if an outline is commented, all subordinate outlines are considered to be commented as well. If it's not present, the value is false.*)(* see common attributes *)
      is_breakpoint : bool; (** isBreakpoint is a string, either "true" or "false", indicating whether a breakpoint is set on this outline. This attribute is mainly necessary for outlines used to edit scripts that execute. If it's not present, the value is false.*)(* see common attributes *)
      (* attrs : (string * string) list; *)
      outlines : outline list (** List that contains any number of <outline> elements.*)
    }

type body = outline list (** A <body> contains one or more <outline> elements.*)

type opml = 
    {
      version : string; (** version is a string which tells version of OPML document (should be 1.0 or 1.1*)
      head : head; (** <head> element.*)
      body : body (** <body> element.*)
    }

val parse : Xmlm.input -> opml
(** Takes an Xmlm.input and gives opml record which is an OCaml repr of an OPML document.*)

val unsafe : Xmlm.input -> 
	     [> `Opml of
		  [> `Body of
		       [> `Outline of
			    [> `IsBreakpoint of string
			    | `IsComment of string
			    | `Outline of 'a
			    | `Text of string
			    | `Type of string ]
			      list as 'a ]
			 list
		  | `Head of
		      [> `DateCreated of string
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
	       (** Analyse without verification *)

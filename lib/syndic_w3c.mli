(** [Syndic.W3C]: invoke and parse the result of the W3C validator. *)

module Error : module type of Syndic_error

type error
type warning

(** Distinguishes an error from a warning. *)
type 'a kind

val error : error kind
val warning : warning kind

type 'a t =
  { kind: 'a kind  (** Error or warning. *)
  ; line: int
        (** Within the source code of the validated document, refers to the
            line where the error was detected. *)
  ; column: int
        (** Within the source code of the validated document, refers to the
            line where the column was detected. *)
  ; text: string  (** The actual error message. *)
  ; element: string
        (** Element in the feed where the message was triggered. *)
  ; parent: string  (** In the feed, parent of the element. *)
  ; value: string
        (** If applicable the value of the element, attribute or content which
            triggered the message. *) }

val url : [< `Data of string | `Uri of Uri.t] -> Uri.t
(** Generate url for the W3C Feed Validator API returning a SOAP 12 output.
    Thus URL is supposed to be used with GET. *)

val to_error : _ t -> Error.t

val parse : Xmlm.input -> error t list * warning t list
(** [parse i] takes [i] and returns a list of error, result of
    {{:http://validator.w3.org/feed/docs/soap} W3C Feed Validator}. *)

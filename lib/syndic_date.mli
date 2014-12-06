type t = CalendarLib.Calendar.t

val compare : t -> t -> int
val now : unit -> t
val from_unixfloat : float -> t

val of_rfc822 : string -> t
val to_rfc822 : t -> string

val of_rfc3339 : string -> t
val to_rfc3339 : t -> string

(* Convenience functions *)

type month =
  Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
(** Month of the year. *)

val string_of_month : month -> string
(** Return the 3 letters identifying the month in English. *)

val year : t -> int
(** Return the 4 digit year of the date. *)

val month : t -> month
(** Return the month of the date. *)

val day : t -> int
(** Return the day of the month (1..31). *)

val hour : t -> int
val minute : t -> int
val second : t -> float
;;

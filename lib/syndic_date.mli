(** Minimal date module required by Syndic. *)

(** A date with time. *)
type t = Ptime.t

val epoch : t
(** The POSIX time, i.e. Thursday, 1 January 1970 00:00:00 (UTC). *)

val compare : t -> t -> int
(** Compare dates in increasing order. *)

val max : t -> t -> t
(** [max d1 d2] return the maximum (i.e. more recent) of the dates [d1] and
    [d2]. *)

val min : t -> t -> t
(** [min d1 d2] return the minimum (i.e. less recent) of the dates [d1] and
    [d2]. *)

val of_rfc822 : string -> t
val to_rfc822 : t -> string
val of_rfc3339 : string -> t
val to_rfc3339 : t -> string

(** Month of the year. *)
type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec

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

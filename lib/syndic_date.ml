open CalendarLib
open Printf
open Scanf

type t = CalendarLib.Calendar.t

let epoch = Calendar.make 1970 1 1  0 0 0
let compare = CalendarLib.Calendar.compare

let max d1 d2 =
  if compare d1 d2 < 0 then d2 else d1
let min d1 d2 =
  if compare d1 d2 < 0 then d1 else d2

let month_to_int = Hashtbl.create 12

let () =
  let add m i = Hashtbl.add month_to_int m i in
  add "Jan" 1; add "Feb" 2; add "Mar" 3; add "Apr" 4;
  add "May" 5; add "Jun" 6; add "Jul" 7; add "Aug" 8;
  add "Sep" 9; add "Oct" 10; add "Nov" 11; add "Dec" 12

  (* Format: http://www.rssboard.org/rss-specification#ltpubdategtSubelementOfLtitemgt
     Examples: Sun, 19 May 2002 15:21:36 GMT
     Sat, 25 Sep 2010 08:01:00 -0700
     20 Mar 2013 03:47:14 +0000 *)
let of_rfc822 s =
  let make_date day month year h m maybe_s z =
    let month =
      if String.length month <= 3 then month
      else String.sub month 0 3 in
    let month = Hashtbl.find month_to_int month in
    let date = Calendar.Date.make year month day in
    let s =
      if maybe_s <> "" && maybe_s.[0] = ':' then
        float_of_string(String.sub maybe_s 1 (String.length maybe_s - 1))
      else 0. in
    let t = Calendar.Time.(make h m (Second.from_float s)) in
    if z = "" || z = "GMT" || z = "UT" then
      Calendar.(create date t)
    else
      (* FIXME: this should be made more robust. *)
      let tz_offset =
        match z with
        | "EST" -> (-5) * 3600
        | "EDT" -> (-4) * 3600
        | "CST" -> (-6) * 3600
        | "CDT" -> (-5) * 3600
        | "MST" -> (-7) * 3600
        | "MDT" -> (-6) * 3600
        | "PST" -> (-8) * 3600
        | "PDT" -> (-7) * 3600
        | _ ->
          let zh = sscanf (String.sub z 0 3) "%i" (fun i -> i) in
          let zm = sscanf (String.sub z 3 2) "%i" (fun i -> i) in
          let tz_sign = if zh < 0 then -1 else 1 in
          if zh < 0 then
            tz_sign * (((-zh) * 3600) + (zm * 60))
          else tz_sign * ((zh * 3600) + (zm * 60))
      in
      let rt = Calendar.(create date t) in
      (* XXX: We lose minutes with this conversion, but Calendar does not
         propose to handle minutes. *)
      let tz = Time_Zone.UTC_Plus (tz_offset / 3600) in
      Calendar.convert rt tz Time_Zone.UTC
  in
  try
    if 'A' <= s.[0] && s.[0] <= 'Z' then (
      try sscanf s "%_s %i %s %i %i:%i%s %s" make_date
      with _ ->
	sscanf s "%_s %ist %s %i %i:%i%s %s" make_date
    )
    else (
      try sscanf s "%i %s %i %i:%i%s %s" make_date
      with _ ->
	sscanf s "%i %s %i" (fun d m y -> make_date d m y 0 0 "" "UT")
    )
  with _ ->
    invalid_arg(sprintf "Syndic.Date.of_string: cannot parse %S" s)

let to_rfc822 d =
  (* Example: Sat, 25 Sep 2010 08:01:00 -0700 *)
  CalendarLib.Printer.Calendar.sprint "%a, %d %b %Y %0H:%0M:%0S %z" d

(* RFC3339 date *)
let of_rfc3339 s =
  let make_date year month day h m s z =
    let date = Calendar.Date.make year month day in
    let t = Calendar.Time.(make h m (Second.from_float s)) in
    if z = "" || z.[0] = 'Z' then
      Calendar.(create date t)
    else
      let tz_offset =
        let open Calendar.Time in
        sscanf z "%i:%i" (fun h m ->
	  let tz_sign = if h < 0 then -1 else 1 in
	  if h < 0 then tz_sign * (((-h) * 3600) + (m * 60))
	  else tz_sign * ((h * 3600) + (m * 60))) in
      (* XXX: We lose minutes with this conversion, but Calendar does not
         propose to handle minutes. *)
      let tz = Time_Zone.UTC_Plus (tz_offset / 3600) in
      let rt = Calendar.(create date t) in
      Calendar.convert rt tz Time_Zone.UTC
  in
  (* Sometimes, the seconds have a decimal point
     See https://forge.ocamlcore.org/tracker/index.php?func=detail&aid=1414&group_id=83&atid=418 *)
  try sscanf s "%i-%i-%iT%i:%i:%f%s" make_date
  with Scanf.Scan_failure _ ->
    invalid_arg(sprintf "Syndic.Atom.Date.of_string: cannot parse %S" s)

let to_rfc3339 d =
  (* Example: 2014-03-19T15:51:25.050-07:00 *)
  CalendarLib.Printer.Calendar.sprint "%Y-%0m-%0dT%0H:%0M:%0S%:z" d


(* Convenience functions *)

type month =
  CalendarLib.Calendar.month =
    Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

let string_of_month = function
  | Jan -> "Jan"  | Feb -> "Feb"  | Mar -> "Mar"  | Apr -> "Apr"  | May -> "May"
  | Jun -> "Jun"  | Jul -> "Jul"  | Aug -> "Aug"  | Sep -> "Sep"  | Oct -> "Oct"
  | Nov -> "Nov"  | Dec -> "Dec"

let year = CalendarLib.Calendar.year
let month = CalendarLib.Calendar.month
let day = CalendarLib.Calendar.day_of_month
let hour = CalendarLib.Calendar.hour
let minute = CalendarLib.Calendar.minute
let second t =
  CalendarLib.Calendar.Time.Second.to_float(CalendarLib.Calendar.second t)
;;

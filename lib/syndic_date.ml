open CalendarLib
open Printf
open Scanf

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
let of_string s =
  let make_date day month year h m maybe_s z =
    let month = if String.length month <= 3 then month
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
      let zh = sscanf (String.sub z 0 3) "%i" (fun i -> i)
      and zm = sscanf (String.sub z 3 2) "%i" (fun i -> i) in
      let tz = Calendar.Time.(Period.make zh zm (Second.from_int 0)) in
      Calendar.(create date (Time.add t tz))
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
    invalid_arg(sprintf "Syndic.Rss2.Date.of_string: cannot parse %S" s)

open Syndic.Opml1
open Printf

(* let () = *)

(*   Printexc.record_backtrace true; *)

(*   try let _ = Atom.parse (Xmlm.make_input (`Channel stdin)) in () *)
(*   with *)
(*     | Atom.Error.Error (pos, err) -> *)
(*         print_endline err *)
(*     | _ -> Printexc.print_backtrace stderr *)

let print_head h =
  printf "Title : %s\n" h.title;
  printf "Date Creation : %f\n" (CalendarLib.Calendar.to_unixfloat h.date_created);
  printf "Date Modified : %f\n" (CalendarLib.Calendar.to_unixfloat h.date_modified);
  printf "Name %s\n" h.owner_name;
  printf "Email %s\n" h.owner_email;
  printf "Expansion State ["; List.iter (printf "%d,") h.expansion_state; printf "]\n";
  printf "Window Top %d\n" h.window_top;
  printf "Window Left %d\n" h.window_left;
  printf "Window Bottom %d\n" h.window_bottom;
  printf "Window Right %d\n" h.window_right

let rec print_outline level o =
  printf "%s Text %s\n" level (match o.text with Some s -> s | None -> "<None>");
  List.iter (print_outline "Sub") o.outlines

let print_body b =
  List.iter (print_outline "High") b

let print_opml1 o =
  printf "Version : %s\n" o.version;
  print_head o.head;
  print_body o.body

let () =

  Printexc.record_backtrace true;

  try let o = parse (Xmlm.make_input (`Channel stdin)) in print_opml1 o
  with
  | Error.Error (pos, err) ->
    print_endline err
  | _ -> Printexc.print_backtrace stderr


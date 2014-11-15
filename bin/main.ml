open Syndic.Opml1
open Printf

(* let () = *)

(*   Printexc.record_backtrace true; *)

(*   try let _ = Atom.parse (Xmlm.make_input (`Channel stdin)) in () *)
(*   with *)
(*     | Atom.Error.Error (pos, err) -> *)
(*         print_endline err *)
(*     | _ -> Printexc.print_backtrace stderr *)

let iter_opt o f =
  match o with
  | None -> ()
  | Some x -> f x

let print_head h =
  printf "Title : %s\n" h.title;
  iter_opt h.date_created (fun d -> printf "Date Creation : %f\n"
                                        (CalendarLib.Calendar.to_unixfloat d));
  printf "Date Modified : %f\n"
         (CalendarLib.Calendar.to_unixfloat h.date_modified);
  printf "Name %s\n" h.owner_name;
  printf "Email %s\n" h.owner_email;
  printf "Expansion State [%s]\n"
         (String.concat ", " (List.map string_of_int h.expansion_state));
  iter_opt h.window_top (fun i -> printf "Window Top %d\n" i);
  iter_opt h.window_left (fun i -> printf "Window Left %d\n" i);
  iter_opt h.window_bottom (fun i -> printf "Window Bottom %d\n" i);
  iter_opt h.window_right (fun i -> printf "Window Right %d\n" i)

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

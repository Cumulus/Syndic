open Printf

exception Error of Xmlm.pos * string

let to_string = function
  | Error (pos, str) ->
    sprintf "%s at l.%d" str (fst pos)
  | exn -> Printexc.to_string exn

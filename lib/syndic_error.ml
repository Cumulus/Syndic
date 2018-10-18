open Printf

type t = Xmlm.pos * string

exception Error of t

let to_string = function
  | Error (pos, str) -> sprintf "%s at l.%d c.%d" str (fst pos) (snd pos)
  | exn -> Printexc.to_string exn

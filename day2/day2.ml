open Str
open Printf

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []
  
let parse_line l =
  match l with
    | [] -> None
    | first :: rest -> Str.split (Str.regexp "[ \t]+") first

let () =
  let lines = read_lines "input" in
  Printf.printf "%s\n" (parse_line lines)


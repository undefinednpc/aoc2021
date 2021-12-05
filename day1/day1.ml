let read_lines name : int list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (int_of_string s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let result a b = if a < b then 1 else 0

let rec increasing acc l =
  match l with
  | [] -> acc
  | [only] -> acc
  | first :: second :: rest -> increasing (acc + result first second) (second :: rest)

let rec increasing_window acc l =
  match l with
  | [] -> acc
  | [only] -> acc
  | a::b::c::d::rest -> increasing_window (acc + (result (a+b+c) (b+c+d))) (b::c::d::rest)
  | _ -> acc
;;

let lines = read_lines "input" in
Printf.printf "%d %d\n" (increasing 0 lines) (increasing_window 0 lines)

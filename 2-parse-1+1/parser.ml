(* Parser version 2: Basic arithmetic *)

type ast =
  | AstNum of char
  | AstSum of ast * ast

let rec string_of_ast st =
  match st with
  | AstNum c ->
    Printf.sprintf "(number %c)" c
  | AstSum (lhs, rhs) ->
    Printf.sprintf "(sum %s + %s)" (string_of_ast lhs) (string_of_ast rhs)
 

let parse s =
  (* p is the position in the string to parse from *)
  let parseNum p =
    if '0' <= s.[p] && s.[p] <='9'
    then AstNum s.[p]
    else raise (Failure "Cannot parse number")
  in

  let parseSum p =
    let lhs = parseNum p in
    let rhs = parseNum (p + 2) in
    if s.[p + 1] == '+'
    then AstSum (lhs, rhs)
    else raise (Failure "cannot parse number")
  in

  parseSum 0

let () =
  let infile = open_in Sys.argv.(1) in
  let infile_length = in_channel_length infile in
  let s = really_input_string infile infile_length in

  let parsed = parse s in

  print_string (string_of_ast parsed)





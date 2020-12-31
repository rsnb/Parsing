(* # Grammar
 * A <- 'a' A &'b' / 'a' A &'c' / epsilon
*)


let parse s =
  (* This grammar, because it contains epsilon, will always match.
   * Simply returning the index at which it matches.
   *)
  let rec parseA i =
    if s.[i] == 'a'
    then let iend = parseA (i+1) in
      if s.[iend + 1] == 'b'
      then iend
      (* Uncomment the code in the below line to simulate naive parsing.
       * Otherwise, it uses the binding above, as if memoized. *)
      else (* let iend = parseA (i+1) in *)
        if s.[iend + 1] == 'c'
        then iend
        else i
    else i
  in

  parseA 0

let () =
  let infile = open_in Sys.argv.(1) in
  let infile_length = in_channel_length infile in
  let s = really_input_string infile infile_length in

  print_int (parse s)

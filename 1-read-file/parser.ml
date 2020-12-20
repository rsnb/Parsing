(* Parser version 1: Read the file *)

let () =
  (* Hacky, doesn't work with stdin *)
  let infile = open_in Sys.argv.(1) in
  let infile_length = in_channel_length infile in
  let to_parse = really_input_string infile infile_length in
  print_string to_parse;
  print_string "Done\n";


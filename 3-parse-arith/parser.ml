open Printf
open Result

type ast =
  | AstDigit of int
(*  Number-so-far * Digits-in-number  *)
  | AstNumber of int * int
  | AstSum of ast * ast

let rec string_of_ast st =
  match st with
  | AstDigit n ->
    sprintf "(digit %d)" n
  | AstNumber (n, nd)->
    sprintf "(number %d)" n
  | AstSum (lhs, rhs) ->
    sprintf "(sum %s + %s)" (string_of_ast lhs) (string_of_ast rhs)

type parsed = (ast * int, string) result

let rec power_of_10 n =
  if n == 0 then 1 else 10 * (power_of_10 (n-1))

(* # Grammar
 *
 * Additive <- Number '+' Additive / Number
 * Number   <- Digit*
 * Digit    <- [0-9]
 *)
let parse s =
  (* p is the position in the string to parse from *)
  let pDigit p =
    assert (p < (String.length s));
    if '0' <= s.[p] && s.[p] <= '9'
    then let digitValue = (Char.code s.[p]) - 48 in
         Ok ((AstDigit digitValue), p + 1)
    else Error "Cannot parse digit"
  in

  let rec pNumber p =
    match pDigit p with
    | Ok (AstDigit digit, pnext) ->
      (
      match pNumber pnext with
      | Ok (AstNumber (rest, nd), pend) ->
        Ok (AstNumber ((rest + (power_of_10 nd)*digit), nd + 1), pend)
      | Ok _ ->
        Error "pNumber did not return an AstNumber"
      | Error s ->
        Ok (AstNumber (digit, 1), pnext)
      )
    | Ok _ ->
      Error "pDigit did not return an AstDigit"
    | Error s -> Error s
  in

  let rec pAdditive p =
    match pNumber p with
    | Ok (lhs, pplus) ->
      if s.[pplus] == '+'
      then (match pAdditive (pplus + 1) with
            | Ok (rhs, pend) ->
              Ok (AstSum (lhs, rhs), pend + 1)
            | Error s -> Error s)
      else Ok (lhs, pplus)
    | Error s -> Error s
  in

  pAdditive 0

let () =
  let infile = open_in Sys.argv.(1) in
  let infile_length = in_channel_length infile in
  let s = really_input_string infile infile_length in

  let parsed = match parse s with
               | Ok (ast, _) -> ast
               | Error s -> raise (Failure s)
  in
  print_string (string_of_ast parsed)

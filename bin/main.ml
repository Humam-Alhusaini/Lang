open Lang
open Lexer
open Printer
open Parser
open Printf

let interp str =
  try
    let lex = new lexer str in
      let tokens = lex#tokenize [] in 
      let _ = print_tokens (toks_to_tokens tokens) in
        let parse = new parse_exp tokens in
        let _ = parse#parse_expr in ()
  with 
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks;;

let rec repl () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | _ -> interp txt

let () = repl ();;

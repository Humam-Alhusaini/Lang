
type cursor = {
	mutable line_num : int; (*The line number*)
	mutable bol_off : int; (*The offset between the cursor and the start of the line*)
	mutable offset : int; (*The offset between the cursor and the start of the file*)
};;

type position = {
  line_num : int;
  bol_off : int; 
  offset : int; 
};;

let dummy : position = {
  line_num = 0;
  bol_off = 0;
  offset = 0;
};;

let curs_to_pos (curs : cursor) : position = 
  {line_num = curs.line_num; bol_off = curs.bol_off; offset = curs.offset};;

let pos_to_curs (pos : position) : cursor = 
  {line_num = pos.line_num; bol_off = pos.bol_off; offset = pos.offset};;

type token =  (Tokens.t * position);;

exception Lexing_error of string * Tokens.t list * cursor;;

open Tokens
open Printf

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf;;

let string_to_tok str : Tokens.t = 
  match str with
  | "if" -> IF
  | "else" -> ELSE
  | "true" -> TRUE
  | "false" -> FALSE
  | "nat" -> NAT
  | _ -> VAR str;;

class cursor_tracker txt =
  object (self)
    
    val curs : cursor = {line_num = 1; bol_off = 0; offset = 0}
    
    method shiftr () =
      curs.offset <- curs.offset + 1;
      curs.bol_off <- curs.bol_off + 1
   
    method shiftl () =
      curs.offset <- curs.offset - 1;
      curs.bol_off <- curs.bol_off - 1

    method reset_bol_off () =
      curs.bol_off <- 0;
    
    method new_line () =
      curs.line_num <- curs.line_num + 1;
      self#reset_bol_off ();
      self#shiftr ()

    method current_off = curs.offset
    method current_line = curs.line_num
    method current_bol_off = curs.bol_off

    method get_curs = curs

    method at_eof () : bool = curs.offset >= String.length txt
  end

class lexer (str : string) = object (self)
  (*The string being lexed*)
  val txt : string = str
 
  val cursor = new cursor_tracker str 

(*Tokenizes txt*)
method tokenize (tokens : token list) : token list =

  let tokenize_next toks = cursor#shiftr (); self#tokenize toks in

  let tokenize_nline toks = cursor#new_line (); self#tokenize toks in

  (*This tokenizes numbers, it is initiated when the lexer finds a num*)
  let rec tokenize_num (chars : char list) =
    cursor#shiftr ();
    if cursor#at_eof () |> not then begin
      let char = txt.[cursor#current_off] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' ->  
          let (toks, _) = List.split tokens in
          Lexing_error ("Can't end number with letter, add a space or something", toks, cursor#get_curs) |> raise;
      | '0' .. '9' -> chars @ [char] |> tokenize_num 
      | _ -> cursor#shiftl ();
            let final_num = chars |> string_of_chars |> int_of_string in
            let token = NUM final_num in token end
    else
      let final_num = chars |> string_of_chars |> int_of_string in
        let token = NUM final_num in token
          in

  let rec tokenize_word (chars : char list) : Tokens.t = 
    cursor#shiftr ();
    if cursor#at_eof () |> not then begin
      let char = txt.[cursor#current_off] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> chars @ [char] |> tokenize_word
      | _ -> cursor#shiftl (); let token = chars |> string_of_chars |> string_to_tok in token end
    else 
      let token = chars |> string_of_chars |> string_to_tok in token
          in

  let rec skip_comment () : unit = 
    cursor#shiftr ();
    if cursor#at_eof () |> not then begin
      let char = txt.[cursor#current_off] in
      match char with
      | '\\' -> ()
      | _ -> skip_comment ()
    end
    else 
    let (toks, _) = List.split tokens in
      Lexing_error ("Forgot to close comment", toks, cursor#get_curs) |> raise in

  if cursor#at_eof () |> not then begin
    let char = txt.[cursor#current_off] in
    match char with
    | ' ' | '\t' | '\r' -> tokenize_next tokens
    | '\n' -> tokenize_nline tokens
    | '\\' -> skip_comment (); tokenize_next tokens
    | _ -> let pos = cursor#get_curs |> curs_to_pos in
           let t = match char with
                       | 'a' .. 'z' | 'A' .. 'Z' ->  tokenize_word [char]
                       | '0' .. '9' -> tokenize_num [char]
                       | '+' -> PLUS
                       | '*' -> MULT
                       | '-' -> SUB 
                       | '=' -> EQ
                       | '(' -> LPAREN
                       | ')' -> RPAREN
                       | '{' -> LBRACE
                       | '}' -> RBRACE
                       | '[' -> LBRACK
                       | ']' -> RBRACK
                       | ';' -> SEMICOLON
                       | ':' -> COLON
                       | '&' -> AND
                       | '|' -> OR
                       | ',' -> COMMA
                       | '.' -> PERIOD
                       | t -> let (toks, _) = List.split tokens in
                            Lexing_error (sprintf "%c does not match any known char" t, toks, cursor#get_curs) |> raise in
                            let token = (t, pos) in
                            tokenize_next (tokens @ [token]) end
  else
    tokens
end


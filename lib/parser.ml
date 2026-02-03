exception Parsing_error of string

open Lexer
open Tokens

type expr = 
  | Num of int
  | Binop of op * expr * expr

and op = 
  | Add
  | Sub
  | Mult

class parse_exp (tokens : token list) = object (self)
  
  val mutable toks = tokens

  method shift () =
   match toks with
   | [] -> Parsing_error "No more" |> raise
   | _ :: ls -> toks <- ls

  method shift_n num =
    let rec loop num =
      if num > 0 then begin
        self#shift (); loop (num-1) end
      else () in loop num

  method parse_expr : expr = 
    
    let rec parse_binop (start : expr) : expr =
      
        let match_op op = 
          match op with
          | MULT -> Mult
          | PLUS -> Add
          | SUB -> Sub
          | _ -> Parsing_error "I expect an operator between 2 numbers!" |> raise
           in

      match toks with
      | ((MULT | PLUS | SUB) as op, _) :: (NUM y, _) :: _ -> self#shift_n 2; Binop(match_op op, start, Num y) |> parse_binop
      | [] -> start
      | _ -> Parsing_error "What the helly is happening here?" |> raise
       in
    
    match toks with
    | (NUM x, _) :: _ -> self#shift (); parse_binop (Num x) 
    | [] -> Parsing_error "Where the helly are the tokens" |> raise
    | _ -> Parsing_error "Anomalous op" |> raise

end

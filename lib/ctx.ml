exception Map_error of string

open Parser
open Printf

type expr_map = 
  | Empty
  | Elem of string * expr * expr_map

let rec find (key : string) (map : expr_map) : expr =
  match map with 
  | Empty -> Map_error key |> raise
  | Elem (str, expr, map') -> if str = key then expr else find key map' 

let rec remove (key : string) (map: expr_map) : expr_map =
  match map with 
  | Empty -> Map_error key |> raise
  | Elem (str, expr, map') -> if str = key then map' else Elem (str, expr, remove key map')


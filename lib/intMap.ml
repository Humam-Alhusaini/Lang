exception Map_error of string

open Printf

type total_map = 
  | Empty
  | Add of string * int * total_map

let rec find (key : string) (map : total_map) : int =
  match map with 
  | Empty -> Map_error key |> raise
  | Add (str, num, map') -> if str = key then num else find key map' 

let rec remove (key : string) (map: total_map) : total_map =
  match map with 
  | Empty -> Map_error key |> raise
  | Add (str, num, map') -> if str = key then map' else Add (str, num, remove key map')

let rec fmap (map : total_map) : string =
  match map with 
  | Empty -> ""
  | Add (key, num, map') -> sprintf "%s -> %d\n%s" key num (fmap map');;

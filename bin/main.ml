open Lang
open Interp
open Printer
open In_channel

let rec repl ctx =
  print_string ">>> ";
  let txt = read_line () in
  if String.starts_with ~prefix:"file " txt then 
    let chan = String.sub txt 5 (String.length txt - 5) |> open_bin in
      read (input_all chan) ctx |> repl
  else 
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | "pctx" -> print_endline (fmap ctx); repl ctx
  | _ -> read txt ctx |> repl

let () = repl Empty;;

open Lang
open Interp
open Printer

let rec repl ctx =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | "pctx" -> print_endline (fmap ctx); repl  ctx
  | _ -> read txt ctx |> repl

let () = repl Empty;;

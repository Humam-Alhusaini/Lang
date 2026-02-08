open Lang.Interp

let rec repl ctx =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | _ -> read txt ctx |> repl 

let () = repl CTX.empty;;

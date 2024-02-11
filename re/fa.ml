let compile f =
  Faparser.fa Falexer.token (Lexing.from_channel (open_in f))
  |> Regular.dfa_of_nfa

let accepts = Regular.nfa_accepts

let minimize fa = fa |> Regular.minimize_dfa

let print = Regular.print_fa

let do_binop f a b =
  let m = compile a in
  let n = compile b in
  print (f m n)

let do_unop f a =
  let m = compile a in print (f m)

let print_usage () =
  print_endline "run NFA STRING";
  print_endline "eq NFA1 NFA2";
  print_endline "union NFA1 NFA2";
  print_endline "diff NFA1 NFA2";
  print_endline "symdiff NFA1 NFA2";
  print_endline "intersection NFA1 NFA2";
  print_endline "complement NFA";
  print_endline "min NFA"

let () =
  let command = Sys.argv.(1) in
  match command with
  | "help" -> print_usage ()
  | "run" ->
      let fa = compile Sys.argv.(2) in
      let s  = Regular.explode Sys.argv.(3) in
      print_endline (if accepts fa s then "Accept." else "Reject.")
  | "eq" ->
      let m = compile Sys.argv.(2) in
      let n = compile Sys.argv.(3) in
      print_endline (if Regular.dfa_eq m n then "Yes." else "No.")
  | "union"        -> do_binop Regular.union                Sys.argv.(2) Sys.argv.(3)
  | "diff"         -> do_binop Regular.difference           Sys.argv.(2) Sys.argv.(3)
  | "symdiff"      -> do_binop Regular.symmetric_difference Sys.argv.(2) Sys.argv.(3)
  | "intersection" -> do_binop Regular.intersection         Sys.argv.(2) Sys.argv.(3)
  | "complement"   -> do_unop  Regular.complement           Sys.argv.(2)
  | "min"          -> do_unop  minimize                     Sys.argv.(2)
  | _ -> print_endline "Command not recognized."

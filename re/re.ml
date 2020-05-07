let print_usage () =
  print_endline "match REGEXP STRING - test whether REGEXP matches STRING.";
  print_endline "dfa REGEXP - Output the smallest DFA corresponding REGEXP.";
  print_endline "eq REGEXP1 REGEXP2 - test whether REGEXP1 and REGEXP2 are equivalent.";
  print_endline "help - Print this help."

let () =
  let command = Sys.argv.(1) in
  match command with
  | "help" ->
      print_usage ()
  | "match" ->
      let pat = Sys.argv.(2) in
      let s   = Sys.argv.(3) in
      print_endline (if Pattern.pmatch pat s then "Yes." else "No.")
  | "dfa" ->
      let pat = Sys.argv.(2) in
      Regular.print_fa (Regular.dfa_of_re (Pattern.of_string pat))
  | "eq" ->
      let m = Regular.dfa_of_re (Pattern.of_string Sys.argv.(2)) in
      let n = Regular.dfa_of_re (Pattern.of_string Sys.argv.(3)) in
      print_endline (if Regular.dfa_eq m n then "Yes." else "No.")
  | _ -> print_endline "Command not recognized."

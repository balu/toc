let compile file =
  Lexing.from_channel file |> Parser.program Lexer.token |> Lambda.resolve

let reducer = ref Lambda.nf

let set_reducer = function
    "hnf"  -> reducer := Lambda.hnf
  | "whnf" -> reducer := Lambda.whnf
  | "app"  -> reducer := Lambda.app
  | _      -> failwith "Unknown reduction type."

let input = ref ""

let set_input name = input := name

let _ = begin
  Arg.parse
    [("-n", String set_reducer, "Normal form (hnf or whnf or app).")]
    set_input
    "lambda [OPTIONS] FILE";
  open_in !input |> compile |> List.iter (Lambda.eval_print !reducer)
end

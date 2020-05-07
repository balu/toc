open Tm

let explode s = List.init (String.length s) (String.get s)

let compile file = Lexing.from_channel file |> Parser.program Lexer.token

exception Time_out of configuration

let run nsteps op tm input =
  let initial_config = {
    machine = tm;
    state = tm.start;
    tape = init_tape input
  } in
  let rec loop nsteps config =
    if nsteps <= 0 then raise (Time_out config)
    else begin
      op config;
      loop (nsteps-1) (step config)
    end
  in
    loop nsteps initial_config

let nsteps = ref 100

let trace = ref false

let tmfile = ref ""

let set_program p = tmfile := p

let input = ref ""

let () =
  Arg.parse
    [  ("-n", Set_int nsteps, "Maximum number of steps to simulate.");
       ("-t", Arg.Set trace, "Trace each step of execution.");
       ("-i", Set_string input, "The input string.")
    ]
    set_program
    "tm [OPTIONS] PROGRAM";
  let input   = explode !input in
  let tm      = compile (open_in !tmfile) in
  try
    run !nsteps (if !trace then print_config else ignore) tm input
  with Machine_halt c -> (print_endline "Machine halted."; print_config c)
     | Time_out c -> (print_endline "Time out."; print_config c)

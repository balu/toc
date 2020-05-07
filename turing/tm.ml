type state = string

let print_state = print_string

type symbol = char

let print_symbol = print_char

let blank = '$'

type tape = Tape of symbol list * symbol list

type direction = Left | Right | Stay

let blank_tape = Tape ([ blank ], [ blank ])

let init_tape contents =
  match contents with [] -> blank_tape | _ -> Tape ([ blank ], contents @ [ blank ])

let read tape =
  match tape with Tape (_, y :: _) -> y | _ -> failwith "BUG: Invalid tape."

let write tape symbol =
  match tape with
  | Tape (xs, [y]) -> Tape (xs, symbol :: [y])
  | Tape (xs, _ :: ys) -> Tape (xs, symbol :: ys)
  | _ -> failwith "BUG: Invalid tape."

let move tape direction =
  match direction with
  | Stay -> tape
  | Right -> (
      match tape with
      | Tape (xs, [ y ]) -> Tape (y :: xs, [ y ])
      | Tape (xs, y :: ys) -> Tape (y :: xs, ys)
      | _ -> failwith "BUG: Invalid tape." )
  | Left -> (
      match tape with
      | Tape ([ y ], ys) -> Tape ([ y ], y :: ys)
      | Tape (x :: xs, ys) -> Tape (xs, x :: ys)
      | _ -> failwith "BUG: Invalid tape." )

module StateSymbol = struct
  type t = state * symbol

  let compare p1 p2 =
    let i1, i2 = p1 in
    let j1, j2 = p2 in
    if compare i1 j1 = 0 then compare i2 j2 else compare i1 j1
end

module TransitionTable = Map.Make (StateSymbol)
module State = Int
module StateSet = Set.Make (State)

type turing_machine = {
  start : state;
  transition : (state * symbol * direction) TransitionTable.t;
}

type configuration = { machine : turing_machine; state : state; tape : tape }

let print_config config =
  print_state config.state;
  print_string "#";
  let (Tape (left, right)) = config.tape in
  List.iter print_symbol (List.rev left);
  print_string ".";
  List.iter print_symbol right;
  print_newline ()

exception Machine_halt of configuration

let step config =
  let view = (config.state, read config.tape) in
  let tran = config.machine.transition in
  match TransitionTable.find view tran with
  | st, sym, dir ->
      { config with state = st; tape = move (write config.tape sym) dir }
  | exception Not_found -> raise (Machine_halt config)

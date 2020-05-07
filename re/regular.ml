let is_empty xs = match xs with [] -> true | _ -> false

type symbol = char
let epsilon = 'e'

type re =
  | Empty
  | Symbol of symbol
  | Choice of re * re
  | Concat of re * re
  | Star of re

type state = int

module State = Int

module StateSet = Set.Make (State)

let singleton = StateSet.singleton
let to_list s = StateSet.elements s
let intersects a b = not (StateSet.is_empty (StateSet.inter a b))

type table = (state * symbol * state) list

type fa = {
  transition : table;
  start : state;
  accept : StateSet.t
}

let print_fa fa =
  print_int fa.start;
  print_newline ();
  print_string "{";
  StateSet.iter (fun f -> print_int f; print_string ",") fa.accept;
  print_string "}";
  print_newline ();
  List.iter
    (fun (s, a, t) -> Printf.printf "(%d, %c) -> %d\n" s a t)
    fa.transition

let concat_map f xs = List.concat (List.map f xs)

let states nfa =
  concat_map (fun (s, _, t) -> [s; t]) nfa.transition
  |> StateSet.of_list

let symbols nfa =
  List.filter_map
    (fun (_, s, _) -> if s != epsilon then Some s else None)
    nfa.transition

let is_accepting q n = StateSet.mem q n.accept

let lookup table (state, symbol) =
  List.fold_left
    (fun xs (st, sy, t) -> if st = state && sy = symbol then t :: xs else xs)
    []
    table
  |> StateSet.of_list

let dfa_lookup table (state, symbol) =
  StateSet.choose (lookup table (state, symbol))

let step nfa states symbol =
  StateSet.fold
    (fun s ss -> StateSet.union (lookup nfa.transition (s, symbol)) ss)
    states
    StateSet.empty

let rec epsilon_closure nfa s =
  let n = StateSet.union s (step nfa s epsilon) in
  if StateSet.equal n s then s else epsilon_closure nfa n

let run nfa input =
  let eclose = epsilon_closure nfa in
  List.fold_left
    (fun xs i -> eclose (step nfa xs i))
    (eclose (singleton nfa.start))
    input

let nfa_accepts nfa input = intersects nfa.accept (run nfa input)

let only x = [x]

let make_counter () =
  let counter = ref 0 in
  let fresh () = (counter := !counter+1; !counter) in
  fresh

let trans s sym t = (s, sym, t)
let etrans s t = trans s epsilon t
let etrans_all ss s = List.map (fun a -> etrans a s) (to_list ss)

let trans_compare (s, a, t) (s', a', t') =
  let f = Int.compare s s' in
  let s = Char.compare a a' in
  let t = Int.compare t t' in
  if f = 0 then if s = 0 then t else s
  else f

let nfa_of_re re =
  let fresh = make_counter () in
  let rec loop re =
    match re with
    | Empty ->
        let s = fresh () in
        { start = s
        ; accept = StateSet.empty
        ; transition = []
        }
    | Symbol sym ->
        let s = fresh () in
        let t = fresh () in
        { start = s;
          accept = singleton t;
          transition = only (trans s sym t)
        }
    | Concat (re1, re2) ->
        let s = fresh () in
        let t = fresh () in
        let n1 = loop re1
        and n2 = loop re2 in
        { start = s;
          accept = singleton t;
          transition =
              (etrans s n1.start) ::
              etrans_all n1.accept n2.start @
              etrans_all n2.accept t @
              n1.transition @ n2.transition
        }
    | Star re1 ->
        let s = fresh () in
        let t = fresh () in
        let n1 = loop re1 in
        { start = s;
          accept = singleton t;
          transition =
            etrans s t ::
            etrans s n1.start ::
            etrans t s ::
            etrans_all n1.accept t @ n1.transition
        }
    | Choice (re1, re2) ->
        let s = fresh () in
        let t = fresh () in
        let n1 = loop re1
        and n2 = loop re2 in
        { start = s;
          accept = singleton t;
          transition =
            etrans s n1.start ::
            etrans s n2.start ::
            etrans_all n1.accept t @
            etrans_all n2.accept t @
            n1.transition @ n2.transition
        }
  in
    loop re

let split f xs =
  List.fold_left
    (fun (ys, zs) x -> if f x then (x :: ys, zs) else (ys, x :: zs))
    ([], [])
    xs

let partition equiv xs =
  let rec loop remaining partition =
    match remaining with
    | [] -> partition
    | x :: xs ->
        let (ys, zs) = split (equiv x) (x :: xs) in
        loop zs (ys :: partition)
  in loop xs []

let partition_id partition x =
  let rec loop partition i =
    match partition with
    | [] -> raise Not_found
    | y :: ys -> if List.mem x y then i else loop ys (i+1)
  in loop partition 0

let minimize_dfa dfa =
  let final_states = to_list dfa.accept in
  let non_final_states = to_list (StateSet.diff (states dfa) dfa.accept) in
  let initial_partition = [non_final_states; final_states] in
  let symbols = symbols dfa in
  let lookup (state, sym) = StateSet.choose (lookup dfa.transition (state, sym)) in
  let rec loop n p =
    if n = 0 then p
    else
      let id = partition_id p in
      let equiv x y =
        List.for_all
          (fun s -> id (lookup (x, s)) = id (lookup (y, s)))
          symbols in
      loop (n-1) (concat_map (partition equiv) p) in
  let n = List.length final_states + List.length non_final_states in
  let final_partition = loop n initial_partition in
  let fresh = make_counter () in
  let names = List.map (fun p -> (StateSet.of_list p, fresh ())) final_partition in
  let translate name = List.assoc name names in
  let translate_id id = let (_, n) = (List.nth names id) in n in
  let fid = partition_id final_partition in
  { start =
      final_partition
      |> List.find (List.mem dfa.start)
      |> StateSet.of_list
      |> translate;
    accept =
      final_partition
      |> List.map StateSet.of_list
      |> List.filter (intersects dfa.accept)
      |> List.map translate
      |> StateSet.of_list;
    transition =
      final_partition
      |> concat_map
           (fun p ->
              let rep = List.hd p in
              let s   = translate_id (fid rep) in
              List.map
                (fun symbol -> (s, symbol, lookup (rep, symbol) |> fid |> translate_id))
                symbols)
      |> List.sort_uniq trans_compare
  }

type ('a, 'b) translator = ('a * 'b) list

let name_of = List.assoc

let value_exists v t = Option.is_some (List.find_opt (fun (u, _) -> u = v) t)

let rec incremental c update =
  match update c with
  | None -> c
  | Some c -> incremental c update

let dfa_of_nfa nfa =
  let dfa_of_nfa' =
    let eclose    = epsilon_closure nfa in
    let start     = eclose (singleton nfa.start) in
    let fresh     = make_counter () in
    let table     = ref [(start, fresh ())] in
    let pending   = ref [start] in
    let accepting = ref [] in
    let name_of v = name_of v !table in
    let push v t  = t := v :: !t in
    let pop t     = let h = List.hd !t in t := List.tl !t; h in
    let push_if b = if b then push else (fun _ _ -> ()) in
    let s         = name_of start in
    let syms      = symbols nfa in
    let ()        = push_if (intersects start nfa.accept) s accepting in
    incremental
      { start      = s
      ; accept     = StateSet.of_list !accepting
      ; transition = []
      }
      (fun dfa ->
         if is_empty !pending then None
         else
         let current        = pop pending in
         let state          = name_of current in
         let current        = to_list current in
         let outgoing       =
           List.map (fun sym ->
             (sym,
               List.fold_left
                 (fun cur s ->
                    StateSet.union cur (eclose (lookup nfa.transition (s, sym))))
                 StateSet.empty
                 current))
             syms in
         let () =
           List.iter (fun (_, target) ->
             if not (value_exists target !table) then begin
               push (target, fresh ()) table;
               let n = name_of target in
               push_if (intersects target nfa.accept) n accepting;
               push target pending
             end else ())
             outgoing in
         let newtransitions =
           List.map (fun (a, t) ->
             let n = name_of t in (state, a, n))
             outgoing in
         Some {dfa with accept     = StateSet.of_list !accepting;
                        transition = newtransitions @ dfa.transition
         })
  in dfa_of_nfa' |> minimize_dfa

let complement m =
  { start = m.start
  ; transition = m.transition
  ; accept = StateSet.diff (states m) m.accept
  } |> minimize_dfa

let extend_symbols m s =
  let module Symbol    = Char in
  let module SymbolSet = Set.Make (Symbol) in
  let s    = SymbolSet.of_list s in
  let s'   = SymbolSet.of_list (symbols m) in
  let a    = SymbolSet.elements (SymbolSet.union s s') in
  let n    = SymbolSet.elements (SymbolSet.diff s s') in
  let x    = states m in
  let d    = StateSet.max_elt x + 1 in
  let dt   = List.map (fun s -> trans d s d) a in
  let x    = to_list x in
  {m with
     transition = concat_map (fun st ->
       List.map (fun sym ->
         (st, sym, d))
         n)
       x @ m.transition @ dt
  }

let product m' n' is_accepting =
  let product' =
    let m         = extend_symbols m' (symbols n') in
    let n         = extend_symbols n' (symbols m') in
    let start     = (m.start, n.start) in
    let fresh     = make_counter () in
    let table     = ref [(start, fresh ())] in
    let pending   = ref [start] in
    let accepting = ref [] in
    let name_of v = name_of v !table in
    let push v t  = t := v :: !t in
    let pop t     = let h = List.hd !t in t := List.tl !t; h in
    let push_if b = if b then push else (fun _ _ -> ()) in
    let s         = name_of start in
    let syms      = symbols m in
    let ()        = push_if (is_accepting start) s accepting in
    incremental
      { start = s
      ; accept = StateSet.of_list !accepting
      ; transition = []
      }
      (fun d ->
         if is_empty !pending then None
         else
         let (q1, q2) = pop pending in
         let current  = name_of (q1, q2) in
         let outgoing =
           List.map (fun sym ->
             (sym,
              (dfa_lookup m.transition (q1, sym),
               dfa_lookup n.transition (q2, sym))))
             syms in
         let () =
           List.iter (fun (a, t) ->
             if not (value_exists t !table) then
             begin
               let n = fresh () in
               push (t, n) table;
               push t pending;
               push_if (is_accepting t) n accepting;
             end else ()) outgoing in
         let newtransitions = List.map
           (fun (a, t) -> (current, a, name_of t))
           outgoing in
         Some {d with accept     = StateSet.of_list !accepting;
                      transition = newtransitions @ d.transition
         })
  in product' |> minimize_dfa

let union m n = product m n
  (fun (q1, q2) -> is_accepting q1 m || is_accepting q2 n)

let intersection m n = product m n
  (fun (q1, q2) -> is_accepting q1 m && is_accepting q2 n)

let difference m n = product m n
  (fun (q1, q2) -> is_accepting q1 m && not (is_accepting q2 n))

let symmetric_difference m n = product m n
  (fun (q1, q2) ->
     let a = is_accepting q1 m
     and b = is_accepting q2 n in
     a && not b || not a && b)

let dfa_empty d = StateSet.is_empty d.accept

let dfa_eq m n =
  let s = symmetric_difference m n in
  dfa_empty s

let explode s = List.init (String.length s) (String.get s)

let re_accepts re s =
  let n = nfa_of_re re in
  let i = explode s in
  nfa_accepts n i

let dfa_of_re re =
  re
  |> nfa_of_re
  |> dfa_of_nfa
  |> minimize_dfa

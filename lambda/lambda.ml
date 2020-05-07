type var = { label : string; id : int }

module Var = struct
  type t = var

  let compare u v = compare u.id v.id
end

module VarSet = Set.Make (Var)

let var_eq u v = Var.compare u v = 0

let var_neq u v = not (var_eq u v)

let next = ref 0

let unique label =
  next := !next + 1;
  { label; id = !next }

let fresh () = unique "_"

type term = Var of var | Lam of var * term | App of term * term

let rec is_free v t =
  match t with
  | Var u -> var_eq u v
  | Lam (u, x) -> var_neq u v && is_free v x
  | App (x, y) -> is_free v x || is_free v y

let rec subst t v r =
  let rename t =
    let rec rename' x u v =
      match x with
      | Var w -> if var_eq w u then Var v else Var w
      | App (y, z) -> App (rename' y u v, rename' z u v)
      | Lam (w, z) -> if var_eq u w then x else Lam (w, rename' z u v)
    in
    match t with
    | Lam (u, x) ->
        let v = fresh () in
        Lam (v, rename' x u v)
    | _ -> failwith "rename: should be called on a lambda term."
  in
  match t with
  | Var u -> if var_eq u v then r else t
  | App (x, y) -> App (subst x v r, subst y v r)
  | Lam (u, x) ->
      if var_eq u v then t
      else if is_free u r then subst (rename t) v r
      else Lam (u, subst x v r)

exception Normal_form

let rec nf t =
  match t with
  | App (Lam (v, x), y) -> subst x v y
  | Lam (v, x) -> Lam (v, nf x)
  | App (x, y) -> (
      try App (nf x, y) with Normal_form -> App (x, nf y) )
  | _ -> raise Normal_form

let rec hnf t =
  match t with
  | App (Lam (v, x), y) -> subst x v y
  | Lam (v, x) -> Lam (v, hnf x)
  | App (x, y) -> App (hnf x, y)
  | _ -> raise Normal_form

let rec whnf t =
  match t with
  | App (Lam (v, x), y) -> subst x v y
  | App (x, y) -> App (whnf x, y)
  | _ -> raise Normal_form

let rec app t =
  match t with
  | Lam (_, _) -> raise Normal_form
  | App (Lam(v, x), y) ->
      (try App (Lam (v, x), app y) with Normal_form -> subst x v y)
  | App (x, y) -> (try App (app x, y) with Normal_form -> App (x, app y))
  | _ -> raise Normal_form

let rec eval reduce t =
  match reduce t with t -> eval reduce t | exception Normal_form -> t

type expr = Definition of var * term | Term of term

type program = expr list

type symbol = Local of var | Global of var * term

type environment = symbol list

let compare_symbol v s =
  match s with Local u | Global (u, _) -> u.label = v.label

let lookup env v = List.find (compare_symbol v) env

let add_global env v t = Global (v, t) :: env

let add_local env v = Local v :: env

let rec resolve_term env t =
  match t with
  | Var v -> (
      match lookup env v with
      | Local u -> Var u
      | Global (_, t) -> t
      | exception Not_found -> failwith (v.label ^ " not defined.") )
  | Lam (v, x) -> Lam (v, resolve_term (add_local env v) x)
  | App (x, y) -> App (resolve_term env x, resolve_term env y)

let resolve_definition env v t =
  let t' = resolve_term env t in
  (Definition (v, t'), add_global env v t')

let resolve_expr env e =
  match e with
  | Definition (v, t) -> resolve_definition env v t
  | Term t -> (Term (resolve_term env t), env)

let rec resolve_program env p =
  match p with
  | [] -> []
  | e :: es ->
      let e', nenv = resolve_expr env e in
      e' :: resolve_program nenv es

let resolve = resolve_program []

let rec print_term t =
  let print_var u = print_int u.id in
  match t with
  | Var u -> print_var u
  | Lam (u, x) ->
      print_string "\\";
      print_var u;
      print_string ".";
      print_term x;
      print_string ";"
  | App (x, y) ->
      print_string "(";
      print_term x;
      print_string " ";
      print_term y;
      print_string ")"

let eval_print reduce t =
  match t with
  | Term t ->
      eval reduce t |> print_term;
      print_newline ()
  | _ -> ()

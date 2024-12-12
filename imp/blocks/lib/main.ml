open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec eval_expr : state -> expr -> memval =
 fun st e ->
  match e with
  | True -> Bool true
  | False -> Bool false
  | And (e0, e1) -> (
      match (eval_expr st e0, eval_expr st e1) with
      | Bool b1, Bool b2 -> Bool (b1 && b2)
      | _ -> failwith "incompatible value")
  | Or (e0, e1) -> (
      match (eval_expr st e0, eval_expr st e1) with
      | Bool b1, Bool b2 -> Bool (b1 || b2)
      | _ -> failwith "incompatible value")
  | Not e0 -> (
      match eval_expr st e0 with
      | Bool b1 -> Bool (not b1)
      | _ -> failwith "incompatible value")
  | Add (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> failwith "incompatible value")
  | Sub (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> failwith "incompatible value")
  | Mul (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Int (n1 * n2)
      | _ -> failwith "incompatible value")
  | Eq (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Bool (n1 = n2)
      | Bool b1, Bool b2 -> Bool (b1 = b2)
      | _ -> failwith "incompatible value")
  | Leq (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Bool (n1 <= n2)
      | _ -> failwith "incompatible value")
  | Const n -> Int n
  | Var v -> (
      let env1 = topenv st in
      match env1 v with BVar l | IVar l -> (getmem st) l)

let eval_decl : state -> decl list -> state =
 fun st ds ->
  let env, loc =
    List.fold_left
      (fun (env, loc) d ->
        match d with
        | IntVar x -> (bind_env env x (IVar loc), loc + 1)
        | BoolVar x -> (bind_env env x (BVar loc), loc + 1))
      (topenv st, getloc st)
      ds
  in
  let envstack = getenv st in
  make_state (env :: envstack) (getmem st) loc

let rec trace1 : conf -> conf = function
  | Cmd (Skip, st) -> St st
  | Cmd (Assign (x, e), st) ->
      let env, mem = (topenv st, getmem st) in
      let new_mem =
        match eval_expr st e with
        | Int n -> (
            match env x with
            | IVar i -> bind_mem mem i (Int n)
            | _ -> failwith "not compatible")
        | Bool b -> (
            match env x with
            | BVar i -> bind_mem mem i (Bool b)
            | _ -> failwith "not compatible")
      in
      St (make_state (getenv st) new_mem (getloc st))
  | Cmd (Seq (c1, c2), st) -> (
      match trace1 (Cmd (c1, st)) with
      | Cmd (c1', st') -> Cmd (Seq (c1', c2), st')
      | St st' -> Cmd (c2, st'))
  | Cmd (If (e, c1, c2), st) -> (
      match eval_expr st e with
      | Bool b -> if b then Cmd (c1, st) else Cmd (c2, st)
      | _ -> failwith "incompatible value")
  | Cmd (While (e, c), st) -> (
      match eval_expr st e with
      | Bool true -> Cmd (Seq (c, While (e, c)), st)
      | Bool false -> St st
      | _ -> failwith "incompatible value")
  | Cmd (Decl (dl, c), st) -> (
      let new_st = eval_decl st dl in
      match trace1 (Cmd (c, new_st)) with
      | St st' -> St (make_state (popenv st') (getmem st') (getloc st'))
      | Cmd (c', st') -> Cmd (Block c', st'))
  | Cmd (Block c, st) -> (
      match trace1 (Cmd (c, st)) with
      | St st' -> St (make_state (popenv st') (getmem st') (getloc st'))
      | Cmd (c', st') -> Cmd (Block c', st'))
  | _ -> raise NoRuleApplies

let trace (n_steps : int) (c : cmd) : conf list =
  let conf0 = Cmd (c, state0) in
  let rec helper n conf =
    if n > 0 then
      try
        let conf' = trace1 conf in
        conf :: helper (n - 1) conf'
      with NoRuleApplies -> [ conf ]
    else [ conf ]
  in
  helper n_steps conf0

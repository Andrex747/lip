open Ast
open Types

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let eval_decl : state -> decl list -> state =
 fun st ds ->
  let env, loc =
    List.fold_left
      (fun (env, loc) d ->
        match d with
        | IntVar x -> (bind_env env x (IVar loc), loc + 1)
        | Fun (f, x, c, e) -> (bind_env env f (IFun (x, c, e)), loc + 1))
      (topenv st, getloc st)
      ds
  in
  let envstack = getenv st in
  make_state (env :: envstack) (getmem st) loc

let rec trace_expr (st : state) (e : expr) : state * expr =
  match e with
  | True -> (st, True)
  | False -> (st, False)
  | Const n -> (st, Const n)
  | Var x -> (
      match (topenv st) x with
      | IVar l -> (st, Const ((getmem st) l))
      | _ -> failwith "apply error")
  | Not True -> (st, False)
  | Not False -> (st, True)
  | Not e -> ( match trace_expr st e with st', e' -> (st', Not e'))
  | And (True, e) -> (
      match trace_expr st e with
      | st', True -> (st', True)
      | st', False -> (st', False)
      | st', e' -> (st', And (True, e')))
  | And (False, _) -> (st, False)
  | And (e1, e2) -> (
      match trace_expr st e1 with
      | st', True -> (st', And (True, e2))
      | st', False -> (st', And (False, e2))
      | st', e' -> (st', And (e', e2)))
  | Or (True, _) -> (st, True)
  | Or (False, e) -> (
      match trace_expr st e with
      | st', True -> (st', True)
      | st', False -> (st', False)
      | st', e' -> (st', Or (False, e')))
  | Or (e1, e2) -> (
      match trace_expr st e1 with
      | st', True -> (st', Or (True, e2))
      | st', False -> (st', Or (False, e2))
      | st', e1' -> (st', Or (e1', e2)))
  | Add (Const n1, Const n2) -> (st, Const (n1 + n2))
  | Add (Const n, e) -> (
      match trace_expr st e with
      | st', Const n' -> (st', Add (Const n, Const n'))
      | st', e' -> (st', Add (Const n, e')))
  | Add (e1, e2) -> (
      match trace_expr st e1 with st', e' -> (st', Add (e', e2)))
  | Sub (Const n1, Const n2) -> (st, Const (n1 - n2))
  | Sub (Const n, e) -> (
      match trace_expr st e with
      | st', Const n' -> (st', Sub (Const n, Const n'))
      | st', e' -> (st', Sub (Const n, e')))
  | Sub (e1, e2) -> (
      match trace_expr st e1 with
      | st', Const n -> (st', Sub (Const n, e2))
      | st', e' -> (st', Sub (e', e2)))
  | Mul (Const n1, Const n2) -> (st, Const (n1 * n2))
  | Mul (Const n, e) -> (
      match trace_expr st e with
      | st', Const n' -> (st', Mul (Const n, Const n'))
      | st', e' -> (st', Mul (Const n, e')))
  | Mul (e1, e2) -> (
      match trace_expr st e1 with
      | st', Const n -> (st', Mul (Const n, e2))
      | st', e' -> (st', Mul (e', e2)))
  | Eq (Const n, Const n1) -> if n = n1 then (st, True) else (st, False)
  | Eq (Const n, e) -> (
      match trace_expr st e with
      | st', Const n' -> (st', Eq (Const n, Const n'))
      | st', e' -> (st', Eq (Const n, e')))
  | Eq (e1, e2) -> (
      match trace_expr st e1 with
      | st', Const n -> (st', Eq (Const n, e2))
      | st', e1' -> (st', Eq (e1', e2)))
  | Leq (Const n, Const n1) -> if n <= n1 then (st, True) else (st, False)
  | Leq (Const n, e) -> (
      match trace_expr st e with
      | st', Const n' -> (st', Leq (Const n, Const n'))
      | st', e' -> (st', Eq (Const n, e')))
  | Leq (e1, e2) -> (
      match trace_expr st e1 with
      | st', Const n -> (st', Leq (Const n, e2))
      | st', e1' -> (st', Eq (e1', e2)))
  | Call (f, Const n) -> (
      match (topenv st) f with
      | IFun (x, c, e') ->
          let env = bind_env (topenv st) x (IVar (getloc st)) in
          let mem = bind_mem (getmem st) (getloc st) n in
          let loc = getloc st + 1 in
          (make_state (pushenv st env) mem loc, CallExec (c, e'))
      | IVar _ -> raise (TypeError "Is not a function"))
  | Call (f, e) -> (
      match trace_expr st e with
      | _, True | _, False -> failwith "unbound value"
      | st', Const n -> (st', Call (f, Const n))
      | st', e' -> (st', Call (f, e')))
  | CallExec (c, e) -> (
      match trace1 (Cmd (c, st)) with
      | St st' -> (st', CallRet e)
      | Cmd (c', st') -> (st', CallExec (c', e)))
  | CallRet (Const n) ->
      (make_state (popenv st) (getmem st) (getloc st), Const n)
  | CallRet e -> ( match trace_expr st e with st', e' -> (st', CallRet e'))

and trace1 : conf -> conf = function
  | St _ -> raise NoRuleApplies
  | Cmd (Skip, st) -> St st
  | Cmd (Assign (x, Const n), st) -> (
      match (topenv st) x with
      | IVar l ->
          St (make_state (getenv st) (bind_mem (getmem st) l n) (getloc st))
      | _ -> raise (TypeError "not valid symbol"))
  | Cmd (Assign (x, e), st) ->
      let st', e' = trace_expr st e in
      Cmd (Assign (x, e'), st')
  | Cmd (Seq (c1, c2), st) -> (
      match trace1 (Cmd (c1, st)) with
      | Cmd (c1', st') -> Cmd (Seq (c1', c2), st')
      | St st' -> Cmd (c2, st'))
  | Cmd (If (e, c1, c2), st) -> (
      match trace_expr st e with
      | st', True -> Cmd (c1, st')
      | st', False -> Cmd (c2, st')
      | _, Const _ -> raise (TypeError "unbound type")
      | st', e' -> Cmd (If (e', c1, c2), st'))
  | Cmd (While (e, c), st) -> Cmd(If(e, Seq(c,While(e,c)),Skip),st)

let trace (n_steps : int) (Prog (d, c) : prog) : conf list =
  let conf0 = Cmd (c, eval_decl state0 d) in
  let rec helper n conf =
    if n > 0 then
      try
        let conf' = trace1 conf in
        conf :: helper (n - 1) conf'
      with NoRuleApplies -> [ conf ]
    else [ conf ]
  in
  helper n_steps conf0

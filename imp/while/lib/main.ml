open Ast
open Types

let bottom _ = failwith "fail"

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec eval_expr : state -> expr -> exprval =
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
      | Nat n1, Nat n2 -> Nat (n1 + n2)
      | _ -> failwith "incompatible value")
  | Sub (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Nat n1, Nat n2 -> Nat (n1 - n2)
      | _ -> failwith "incompatible value")
  | Mul (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Nat n1, Nat n2 -> Nat (n1 * n2)
      | _ -> failwith "incompatible value")
  | Eq (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Nat n1, Nat n2 -> Bool (n1 = n2)
      | Bool b1, Bool b2 -> Bool (b1 = b2)
      | _ -> failwith "incompatible value")
  | Leq (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Nat n1, Nat n2 -> Bool (n1 <= n2)
      | _ -> failwith "incompatible value")
  | Const n -> Nat n
  | Var v -> st v

let bind st x v y = if x = y then v else st y

let rec trace1 : conf -> conf = function
  | Cmd (Skip, st) -> St st
  | Cmd (Assign (x, e), st) ->
      let st1 = bind st x (eval_expr st e) in
      St st1
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
  | _ -> raise NoRuleApplies


let trace (n_steps : int) (c : cmd) : conf list =
  let conf0 = Cmd (c, bottom) in
  let rec helper n conf =
    if n > 0 then
      try
        let conf' = trace1 conf in
        conf :: helper (n - 1) conf'
      with NoRuleApplies -> [ conf ]
    else [ conf ]
  in
  helper n_steps conf0

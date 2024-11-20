open Ast

let rec string_of_expr = function
  | True -> "True"
  | False -> "False"
  | If (e0, e1, e2) ->
      "If(" ^ string_of_expr e0 ^ "," ^ string_of_expr e1 ^ ","
      ^ string_of_expr e2 ^ ")"
  | Zero -> "Zero"
  | Succ e0 -> "Succ" ^ string_of_expr e0
  | Pred e0 -> "Pred" ^ string_of_expr e0
  | IsZero e0 -> "IsZero" ^ string_of_expr e0
  | _ -> failwith "error"

let string_of_val : exprval -> string = function
  | Bool true -> "True"
  | Bool false -> "False"
  | Nat n -> string_of_int n

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec is_nv = function 
    Zero -> true 
  | Succ e1 ->is_nv e1 
  | _ -> false

exception NoRuleApplies

let rec trace1 = function
  | If (True, e1, _) -> e1
  | If (False, _, e2) -> e2
  | If (e0, e1, e2) -> If (trace1 e0, e1, e2)
  | Not True -> False
  | Not False -> True
  | Not e0 -> Not (trace1 e0)
  | And (True, e0) -> And (True, trace1 e0)
  | And (False, _) -> False
  | And (e0, e1) -> And (trace1 e0, e1)
  | Or (True, _) -> True
  | Or (False, e0) -> Or (False, trace1 e0)
  | Or (e0, e1) -> Or (trace1 e0, e1)
  | Succ e0 ->Succ (trace1 e0) 
  | Pred (Succ e0) -> e0 
  | Pred e0 -> Pred (trace1 e0) 
  | IsZero Zero -> True
  | IsZero (Succ _) ->  False
  | IsZero e0 -> IsZero (trace1 e0)
  | _ -> raise NoRuleApplies

let rec trace e =
  try
    let e' = trace1 e in
    e :: trace e'
  with NoRuleApplies -> [ e ]

let rec eval : expr -> exprval = function
  | True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | If (e0, e1, e2) -> (
      match eval e0 with
      | Bool e -> if e then eval e1 else eval e2
      | Nat _ -> failwith "incompatible value")
  | IsZero e0 -> (
      match eval e0 with
      | Nat n -> if n = 0 then Bool true else Bool false
      | Bool _ -> failwith "incompatible value")
  | Pred e0 -> (
      let v = eval e0 in
      match v with
      | Bool _ -> failwith "incompatible value"
      | Nat n -> if n <= 0 then failwith "incompatible value" else Nat (n - 1))
  | Succ e0 -> (
      let v = eval e0 in
      match v with
      | Bool _ -> failwith "incompatible value"
      | Nat n -> if n < 0 then failwith "incompatible value" else Nat (n + 1))
  | And (e0, e1) -> (
      match (eval e0, eval e1) with
      | Bool b1, Bool b2 -> Bool (b1 && b2)
      | _ -> failwith "incompatible value")
  | Or (e0, e1) -> (
      match (eval e0, eval e1) with
      | Bool b1, Bool b2 -> Bool (b1 || b2)
      | _ -> failwith "incompatible value")
  | Not e0 -> (
      match eval e0 with
      | Bool b1 -> Bool (not b1)
      | _ -> failwith "incompatible value")

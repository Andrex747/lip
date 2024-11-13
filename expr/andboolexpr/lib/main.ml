open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | Not(e0) -> "Not(" ^ (string_of_boolexpr e0) ^ ")"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | And(e0, e1) -> (string_of_boolexpr e0) ^ "and" ^ (string_of_boolexpr e1)
  | Or(e0, e1) -> (string_of_boolexpr e0) ^ "or" ^ (string_of_boolexpr e1)

let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> If((trace1 e0),e1,e2) 
  | Not(True) -> False
  | Not(False) -> True
  | Not(e0) -> Not((trace1 e0))
  | And(True, e0) -> And(True, trace1 e0)
  | And(False, _) -> False
  | And(e0, e1) -> And(trace1 e0, e1)
  | Or(True, _) -> True
  | Or(False, e0) -> Or(False,trace1 e0)
  | Or(e0,e1) -> Or(trace1 e0, e1)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval = function
    True -> true
  | False -> false
  | Not(e0) -> not (eval e0)
  | If(e0,e1,e2) -> if eval e0 then eval e1 else eval e2 
  | And(e0, e1) -> eval e0 && eval e1
  | Or(e0,e1) -> eval e0 || eval e1

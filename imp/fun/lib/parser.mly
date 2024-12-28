%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token START_BLOCK
%token END_BLOCK
%token INT
%token AND
%token OR
%token NOT
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token SKIP
%token ASSIGN
%token SEQ
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token FUN
%token RET
%token <string> CONST
%token <string> VAR 
%token EOF

%left OR
%left AND
%left NOT
%left EQ LEQ
%left ADD SUB
%left MUL
%left SEQ
%nonassoc ELSE
%left DO


%start <prog> prog


%%

prog:
  l=list(decl); c=cmd; EOF { Prog(l,c) }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | NOT; e1 = expr; { Not (e1)} 
  | e1 = expr; AND; e2 = expr; { And(e1,e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
  | e1 = expr; ADD; e2=expr { Add(e1,e2) }
  | e1 = expr; SUB; e2=expr { Sub(e1,e2) }
  | e1 = expr; MUL; e2=expr { Mul(e1,e2) }
  | e1 = expr; EQ; e2=expr { Eq(e1,e2)}
  | e1 = expr; LEQ; e2=expr { Leq(e1,e2)}
  | n = CONST { Const(int_of_string n) }
  | f = VAR; LPAREN; e = expr; RPAREN; {Call(f,e)} 
  | v = VAR { Var(v) }
;

cmd:
  | SKIP { Skip }
  | v = VAR; ASSIGN; e=expr { Assign(v,e) }
  | c1 = cmd; SEQ; c2=cmd; { Seq(c1,c2) }
  | IF; e=expr; THEN c1=cmd; ELSE;c2=cmd; { If(e,c1,c2) }
  | IF; e=expr; THEN c1=cmd; ELSE; LPAREN; c2=cmd; RPAREN { If(e,c1,c2) }
  | IF; e=expr; THEN; LPAREN; c1=cmd; RPAREN; ELSE; c2=cmd { If(e,c1,c2) }
  | WHILE; e=expr; DO; c = cmd { While(e,c)}
  | WHILE; e=expr; DO; LPAREN; c = cmd; RPAREN; { While(e,c)}


decl:
  | INT; v=VAR; SEQ {IntVar(v)}
  | FUN; f=VAR; LPAREN; v=VAR; RPAREN; START_BLOCK; c=cmd; SEQ; RET; ret=expr; END_BLOCK; SEQ; { Fun(f,v,c,ret) }
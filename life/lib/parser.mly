%{
open Rule
%}

%token <string> SEQUENCE
%token S_TOK
%token B_TOK
%token SLASH
%token EOF

%start <rule> prog

%%

prog:
  | e = rule_expr; EOF { e }
;

rule_expr:
  | S_TOK; s_seq = sequence; SLASH; B_TOK; b_seq = sequence { Rule(s_seq, b_seq) }
;

sequence:
  | SEQUENCE { List.map (fun ch -> int_of_char ch - 48) (String.to_seq $1 |> List.of_seq) }
;

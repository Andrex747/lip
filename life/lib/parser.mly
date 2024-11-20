%{
open Rule
%}

%token <string> SEQUENCE
%token <string> EXT_SEQ
%token <string> RANGE_SEQ
%token EXTENDED
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
  | EXTENDED; S_TOK; s_seq = ext_sequence; SLASH; B_TOK; b_seq = ext_sequence { Rule(s_seq, b_seq) }
  | EXTENDED; s_seq = ext_sequence; SLASH; b_seq = ext_sequence { Rule(s_seq, b_seq) }
  | EXTENDED; S_TOK; s_seq = ext_sequence; SLASH; B_TOK; b_seq = ext_sequence { Rule(s_seq, b_seq) }
;

sequence:
  | SEQUENCE { List.map (fun ch -> int_of_char ch - 48) (String.to_seq $1 |> List.of_seq) }
;

ext_sequence:
  | seq = EXT_SEQ { (seq |> String.split_on_char ',' |> List.map( fun x -> int_of_string x))}
  | seq = RANGE_SEQ { 
    seq 
  |> String.split_on_char ','                  (* Step 1: Separazione della stringa per virgole *)
  |> List.map (fun x -> String.split_on_char '.' x)  (* Step 2: Separazione dei numeri per i punti *)
  |> List.map (fun x -> List.filter (fun y -> y <> "") x)  (* Step 3: Rimozione di stringhe vuote *)
  |> List.map (fun x -> List.map (fun y -> int_of_string y) x)  (* Step 4: Conversione in numeri interi *)
  |> List.map (fun x -> match x with
      | [] -> [0]
      | [a; b] -> List.init (b - a + 1) (fun i -> a + i)  (* Step 5: Creazione dell'intervallo da a a b *)
      | _ -> failwith "undefined")  (* Gestisce il caso con formato errato *)
  |> List.flatten
  
  }
  
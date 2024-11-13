{
  open Parser
}

let white = [' ' '\t']+
let seq = ['0'-'9']+
rule read_token =
  parse
  | white {read_token lexbuf}
  | seq {SEQUENCE (Lexing.lexeme lexbuf) }
  | "S" { S_TOK }
  | "B" { B_TOK }
  | "/" { SLASH }
  | eof { EOF }
  

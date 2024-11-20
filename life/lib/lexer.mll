{
  open Parser
}

let white = [' ' '\t']+
let seq = ['0'-'9']+
let ext_seq = ['0'-'9'](','['0'-'9'])*
let range_seq = ['0'-'9']+".."['0'-'9']+(','['0'-'9']+".."['0'-'9']+)*
rule read_token =
  parse
  | white {read_token lexbuf}
  | seq {SEQUENCE (Lexing.lexeme lexbuf) }
  | ext_seq {EXT_SEQ (Lexing.lexeme lexbuf)}
  | range_seq { RANGE_SEQ (Lexing.lexeme lexbuf) }
  | "E" { EXTENDED }
  | "S" { S_TOK }
  | "B" { B_TOK }
  | "/" { SLASH }
  | eof { EOF }
  

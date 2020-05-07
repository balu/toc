{
  open Parser
}

rule token = parse
    [' ' '\t' '\n'] { token lexbuf }
  | '#' [^'\n']*    { token lexbuf }
  | ['0'-'9''a'-'z''A'-'Z''$']+ as lxm { IDENTIFIER(lxm) }
  | '{'  { LBRACE }
  | '}'  { RBRACE }
  | ','  { COMMA }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | "->" { ARROW }
  | ['<' '>' '_'] as dir {
      match dir with
      | '<' -> DIRECTION(Left)
      | '>' -> DIRECTION(Right)
      | '_' -> DIRECTION(Stay)
      | _   -> DIRECTION(Stay)
    }
  | eof  { EOF }

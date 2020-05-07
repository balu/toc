{
  open Parser
}

rule token = parse
    [' ' '\t' '\n'] { token lexbuf }
  | '#' [^'\n']*    { token lexbuf }
  | ['0'-'9''a'-'z''A'-'Z''?''*''\'''$']+ as lxm { IDENTIFIER(lxm) }
  | '\\' { LAMBDA }
  | '.'  { DOT }
  | ';'  { SEMI }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | ":=" { DEF }
  | eof  { EOF }

{
  open Faparser
}

rule token = parse
    [' ''\n''\t'] { token lexbuf }
  | ['a'-'z''A'-'Z'] as lxm { SYMBOL(lxm) }
  | ['0'-'9']+ as lxm { STATE(int_of_string lxm)}
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ',' { COMMA }
  | "->" { ARROW }
  | eof  { EOF }

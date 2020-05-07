{
  open Reparser
}

rule token = parse
    [' '] { token lexbuf }
  | ['0'-'9''a'-'z''A'-'Z'] as lxm { SYMBOL(lxm) }
  | '+' { PLUS }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '*' { STAR }
  | eof  { EOF }

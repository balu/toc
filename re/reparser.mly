%token PLUS LPAREN RPAREN STAR EOF
%token <char> SYMBOL
%start expr
%type <Regular.re> expr
%%

expr: re EOF { $1 }
;

re: sum { $1 }
  | /* empty */ { Empty }
;

sum: term PLUS sum { Choice ($1, $3) }
  | term { $1 }
;

term: factor term { Concat ($1, $2) }
  | factor { $1 }
;

factor: atom STAR { Star $1 }
  | atom { $1 }
;

atom: SYMBOL { Symbol $1 }
  | LPAREN re RPAREN { $2 }
;

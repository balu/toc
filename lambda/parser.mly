%token LAMBDA DOT DEF SEMI LPAREN RPAREN EOF
%token <string> IDENTIFIER
%start program
%type <Lambda.program> program
%%

program: expr program { $1 :: $2 }
       | EOF          { [] }
;

expr: definition SEMI { $1 }
    | term SEMI       { Term($1) }
;

definition: IDENTIFIER DEF term { Definition(Lambda.unique $1, $3) }
;

term: LAMBDA IDENTIFIER DOT term { Lam(Lambda.unique $2, $4) }
    | app                        { $1 }
;

app: atom     { $1 }
   | app atom { App($1, $2) }
;

atom: IDENTIFIER         { Var(Lambda.unique $1) }
    | LPAREN term RPAREN { $2 }
;

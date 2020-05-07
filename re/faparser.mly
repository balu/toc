%token LPAREN RPAREN LBRACE RBRACE ARROW COMMA EOF
%token <int> STATE
%token <char> SYMBOL
%start fa
%type <Regular.fa> fa
%%

fa: start states transitions EOF {
            { start      = $1
            ; accept     = Regular.StateSet.of_list $2
            ; transition = $3
            }
          }
;

start: STATE { $1 }
;

states: LBRACE statelist RBRACE { $2 }
;

statelist: STATE COMMA statelist { $1 :: $3 }
  | STATE { [$1] }
  | /* empty */ { [] }
;

transitions: transition transitions { $1 @ $2 }
  | /* empty */ { [] }
;

transition: LPAREN STATE COMMA SYMBOL RPAREN ARROW STATE { [($2, $4, $7)] }
  | LPAREN STATE COMMA SYMBOL RPAREN ARROW states {
             List.map (fun t -> ($2, $4, t)) $7
           }
;

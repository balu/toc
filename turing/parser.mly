%token LBRACE RBRACE ARROW COMMA LPAREN RPAREN LEFT RIGHT EOF
%token <string> IDENTIFIER
%token <Tm.direction> DIRECTION
%start program
%type <Tm.turing_machine> program
%%

program: IDENTIFIER transitions EOF {
                  {start = $1; transition = $2 }
                }
;

transitions: transition transitions {
                          let (lhs, rhs) = $1 in
                          Tm.TransitionTable.add lhs rhs $2
                        }
  | /* empty */ { Tm.TransitionTable.empty }
;

transition: ltran ARROW rtran { ($1, $3) }
;

ltran: LPAREN IDENTIFIER COMMA IDENTIFIER RPAREN {
                ($2, $4.[0])
              }
;

rtran: LPAREN IDENTIFIER COMMA IDENTIFIER COMMA DIRECTION RPAREN {
                ($2, $4.[0], $6)
              }
;

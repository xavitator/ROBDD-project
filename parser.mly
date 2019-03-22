/* File parser.mly */

%token <int> INT
%token MINUS
%token STRING
%token ZERO
%token P
%token PC
%left ZERO
%start main             /* the entry point */
%type <(int*Implementation.exp)> main
%%
main:
    P STRING INT INT expr PC ZERO                { ($3,$5) }
;
expr:
    elem ZERO expr          { Implementation.Et($1,$3) }
  | elem ZERO               { $1 }
;
elem:
    elem elem               { Implementation.Ou($1,$2) }
  | INT                     { Implementation.Var ($1-1) }
  | MINUS INT               { Implementation.No (Implementation.Var ($2-1)) }
;

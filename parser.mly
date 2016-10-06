%{
  open Syntax
%}
%token <int> INT
%token <float> FLOAT
%token <string> ID
%token IF THEN ELSE TRUE FALSE
%token PLUS MINUS MUL DIV
%token EOF EOL
%token LPAREN RPAREN
%token LET EQUAL LT LE GT GE OR AND NOT NEQ
%token LOAD
%left OR
%left AND
%left LT LE GT GE EQ NEQ
%left PLUS MINUS
%left MUL DIV
%left LPAREN RPAREN
%nonassoc NOT
%type <Syntax.t> main
%start main

%%

main:
 |EOL {Empty}
 |exp EOL {$1}
 |LET ID EQUAL exp EOL {Let($2,$4)}
 |LOAD ID EOL{Load($2)}
;
exp:
 | INT
     {Int($1)}
 | FLOAT
     {Float($1)}
 | TRUE
     {Bool (true)}
 | FALSE
     {Bool (false)}
 | exp EQUAL exp
     {Eq ($1,$3)}
 | exp NEQ exp
     {Neq ($1,$3)}
 | exp LT exp
     {Lt ($1,$3)}
 | exp GT exp
     {Lt ($3,$1)}
 | exp LE exp
     {Le ($1,$3)}
 | exp GE exp
     {Le ($3,$1)}
 | exp PLUS exp
     {Add($1,$3)}
 | exp MINUS exp
     {Sub($1,$3)}
 | exp MUL exp
     {Mul($1,$3)}
 | exp DIV exp
     {Div($1,$3)}
 | LPAREN exp RPAREN
     {$2}
 | ID
     {Var $1}
 | IF exp THEN exp ELSE exp
     {If($2,$4,$6)}
 | NOT exp
     {Not $2}
 | exp OR exp
     {Or ($1,$3)}
 | exp AND exp
     {And ($1,$3)}
;

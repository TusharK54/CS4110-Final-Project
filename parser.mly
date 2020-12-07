%{
  open Ast
  open Printf
  open Lexing
%}

%token UNIT
%token <int> INT
%token <bool> BOOL
%token <string> STR VAR
%token LPAREN RPAREN LSQUARE RSQUARE LCURLY RCURLY
%token ADD SUB MUL DIV EXP MOD
%token EQ NE GT GE LT LE
%token NOT AND OR

%token SEQ
%token ASSIGN
%token IF ELSE
%token EOF

%type <Ast.e> program

%start program

%%
program: c_exp EOF                { $1 }

/* command expressions */
c_exp:
  | c_seq                         { $1 }

c_seq:
  | c_seq SEQ c_control           { Seq($1, $3)}
  | c_seq SEQ                     { $1 }
  | c_control                     { $1 }

c_control:
  | IF b_exp c_block ELSE c_block { If($2, $3, $5) }
  | c                             { $1 }

c_block:
  | LCURLY c_exp RCURLY           { $2 }

c:
  | VAR ASSIGN v                   { Assign($1, $3) }
  | v                             { $1 }

/* values */
v:
  | n_exp                         { $1 }
  | b_exp                         { $1 }
  | UNIT                          { Unit }


/* boolean expressions */
b_exp:
  | b_or                          { $1 }

b_or:
  | b_or OR b_and                 { Bop(Or, $1, $3) }
  | b_and                         { $1 }

b_and:
  | b_and AND b_not               { Bop(And, $1, $3) }
  | b_not                         { $1 }

b_not:
  | NOT b_not                     { Uop(Not, $2) }
  | b_comp                        { $1 }
  | b                             { $1 }

b_comp:
  | n_exp EQ n_exp                { Bop(Eq, $1, $3) }
  | n_exp NE n_exp                { Bop(Ne, $1, $3) }
  | n_exp GT n_exp                { Bop(Gt, $1, $3) }
  | n_exp GE n_exp                { Bop(Ge, $1, $3) }
  | n_exp LT n_exp                { Bop(Lt, $1, $3) }
  | n_exp LE n_exp                { Bop(Le, $1, $3) }

b:
  | LPAREN b_exp RPAREN           { $2 }
  | BOOL                          { Bool($1) }
  | VAR                           { Var($1) }


/* arithmetic expressions */
n_exp:
  | n_add                         { $1 }

n_add:
  | n_add ADD n_mul               { Bop(Add, $1, $3) }
  | n_add SUB n_mul               { Bop(Sub, $1, $3) }
  | n_mul                         { $1 }

n_mul:
  | n_mul MUL n_exp               { Bop(Mul, $1, $3) }
  | n_mul DIV n_exp               { Bop(Div, $1, $3) }
  | n_mul MOD n_exp               { Bop(Mod, $1, $3) }
  | n_exp                         { $1 }

n_exp:
  | n_exp EXP n                   { Bop(Exp, $1, $3) }
  | n                             { $1 }

n:
  | LPAREN n_exp RPAREN           { $2 }
  | INT                           { Int($1) }
  | VAR                           { Var($1) }

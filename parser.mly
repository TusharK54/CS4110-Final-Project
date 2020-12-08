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
%token EQ NE GT GE LT LE NOT AND OR
%token SEQ
%token ASSIGN
%token IF ELSE
%token EOF

/* precedence rules */
%right ASSIGN
%right OR
%right AND
%left EQ NE GT GE LT LE
%left ADD SUB
%left MUL DIV MOD
%left EXP
%right NOT

%type <Ast.e> program
%start program

%%
program: def_exp EOF                    { $1 }

/* top-level definitions */
def_exp:
  | def                                 { $1 }

def:
  | VAR ASSIGN exp                      { Assign($1, $3) }
  | seq_exp                             { $1 }

/* sequence expressions */
block_exp:
  | LCURLY seq_exp RCURLY               { $2 }

seq_exp:
  | exp                                 { $1 }
  | exp SEQ                             { $1 }
  | exp SEQ seq_exp                     { Seq($1, $3) }

/* expressions */
exp:
  | v                                   { $1 }
  | uop                                 { $1 }
  | bop                                 { $1 }
  | IF exp block_exp ELSE block_exp     { If($2, $3, $5) }
  | VAR ASSIGN exp                      { Assign($1, $3) }

uop:
  | SUB exp                             { Uop(Negate, $2) }
  | NOT exp                             { Uop(Not, $2) }

bop:
  | exp ADD exp                         { Bop(Add, $1, $3) }
  | exp SUB exp                         { Bop(Sub, $1, $3) }
  | exp MUL exp                         { Bop(Mul, $1, $3) }
  | exp DIV exp                         { Bop(Div, $1, $3) }
  | exp EXP exp                         { Bop(Exp, $1, $3) }
  | exp MOD exp                         { Bop(Mod, $1, $3) }
  | exp EQ exp                          { Bop(Eq, $1, $3) }
  | exp NE exp                          { Bop(Ne, $1, $3) }
  | exp GT exp                          { Bop(Gt, $1, $3) }
  | exp GE exp                          { Bop(Ge, $1, $3) }
  | exp LT exp                          { Bop(Lt, $1, $3) }
  | exp LE exp                          { Bop(Le, $1, $3) }
  | exp AND exp                         { Bop(And, $1, $3) }
  | exp OR exp                          { Bop(Or, $1, $3) }

v:
  | UNIT                                { Unit }
  | INT                                 { Int($1) }
  | BOOL                                { Bool($1) }
  | STR                                 { Str($1) }
  | VAR                                 { Var($1) }
  | LPAREN seq_exp RPAREN               { $2 }
/*  | F LPAREN arglist RPAREN block_exp   {  }  */

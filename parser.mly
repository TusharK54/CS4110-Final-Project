%{
  open Ast
  open Printf
  open Lexing
%}

%token UNIT
%token <int> INT
%token <bool> BOOL
%token <string> STR ID
%token T_INT T_BOOL T_STR
%token LPAREN RPAREN LSQUARE RSQUARE LCURLY RCURLY
%token ADD SUB MUL DIV EXP MOD
%token EQ NE GT GE LT LE NOT AND OR
%token QUOTE DQUOTE COLON

%token SEQ
%token ASSIGN
%token IF ELSE
%token ASSERT
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
  | var ASSIGN exp                      { Assign($1, $3) }
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
  | var ASSIGN exp                      { Assign($1, $3) }
  | IF exp block_exp ELSE block_exp     { If($2, $3, $5) }

uop:
  | ASSERT exp                          { Uop(Assert, $2) }
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

/* values */
v:
  | var                                 { Var($1, None) }
  | var COLON typ                       { Var($1, Some $3) }
  | UNIT                                { Unit }
  | INT                                 { Int($1) }
  | SUB INT                             { Int($2 * -1) }
  | BOOL                                { Bool($1) }
  | QUOTE STR QUOTE                     { Str($2) }
  | DQUOTE STR DQUOTE                   { Str($2) }
  | LPAREN seq_exp RPAREN               { $2 }

var:
  | STR                                 { $1 }
  | ID                                  { $1 }

/* types */
typ:
  | T_INT                               { T_int }
  | T_BOOL                              { T_bool }
  | T_STR                               { T_str }


/*  | F LPAREN arglist RPAREN block_exp   {  }  */

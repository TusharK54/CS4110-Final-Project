{
open Parser
open Printf
exception Eof
exception Err
}

let int = ['0'-'9']+
let str = ['A'-'Z' 'a'-'z']+
let var = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9']*
let ws = [' ' '\t']

rule token = parse
| "("               { LPAREN }
| ")"               { RPAREN }
| "["               { LSQUARE }
| "]"               { RSQUARE }
| "{"               { LCURLY }
| "}"               { RCURLY }

| "+"               { ADD }
| "-"               { SUB }
| "*"               { MUL }
| "/"               { DIV }
| "^"               { EXP }
| "%"               { MOD }

| "=="              { EQ }
| "!="              { NE }
| ">"               { GT }
| ">="              { GE }
| "<"               { LT }
| "<="              { LE }

| "&&"              { AND }
| "||"              { OR }
| "!"               { NOT }

| "'"               { QUOTE }
| "\""              { DQUOTE }

| "="               { ASSIGN }
| ";"               { SEQ }

| "if"              { IF }
| "else"            { ELSE }

| ":"               { COLON }
| "int"             { T_INT }
| "bool"            { T_BOOL }
| "str"             { T_STR }

| "assert"          { ASSERT }

| "()"              { UNIT }
| int as n          { INT(int_of_string n) }
| "true"            { BOOL(true) }
| "false"           { BOOL(false) }
| str as s          { STR(s) }
| var as x          { ID(x) }

| ws                { token lexbuf }
| '\n'              { Lexing.new_line lexbuf; token lexbuf }

| eof               { EOF }

| _ as c            {
                        let pos = lexbuf.Lexing.lex_curr_p in
                        printf "Error at line %d\n" pos.Lexing.pos_lnum;
                        printf "Unrecognized character: [%c]\n" c;
                        exit 1
                    }

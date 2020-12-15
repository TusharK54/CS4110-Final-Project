{
open Parser
open Printf
exception Eof
exception Err
}

let int = ['0'-'9']+
let str_q = "'" ['A'-'Z' 'a'-'z' '0'-'9' '\\' ' ' '\t']* "'"
let str_qq = '"' ['A'-'Z' 'a'-'z' '0'-'9' '\\' ' ' '\t']* '"'
let var = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9']*
let ws = [' ' '\t']
let comment = '#' _* '\n'

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

| "="               { ASSIGN }
| ";"               { SEQ }

| "if"              { IF }
| "else"            { ELSE }

| ":"               { COLON }
| "int"             { T_INT }
| "bool"            { T_BOOL }
| "str"             { T_STR }

| "f"               { F }
| "assert"          { ASSERT }
| "assertfail"      { ASSERTFAIL }

| ","               { COMMA }
| "."               { DOT }
| "'"               { QUOTE }
| "\""              { DQUOTE }

| "()"              { UNIT }
| int as n          { INT(int_of_string n) }
| "true"            { BOOL(true) }
| "false"           { BOOL(false) }
| str_q as s        { STR(s) }
| str_qq as s       { STR(s) }
| var as x          { ID(x) }

| ws                { token lexbuf }
| comment           { Lexing.new_line lexbuf; token lexbuf }
| '\n'              { Lexing.new_line lexbuf; token lexbuf }

| eof               { EOF }

| _ as c            {
                        let pos = lexbuf.Lexing.lex_curr_p in
                        printf "Error at line %d\n" pos.Lexing.pos_lnum;
                        printf "Unrecognized character: [%c]\n" c;
                        exit 1
                    }

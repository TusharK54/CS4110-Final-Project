{
open Parser
open Printf
exception Eof
exception Err
}

let digit = ['0'-'9']
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
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

| "="               { ASSIGN }
| ";"               { SEQ }

| "if"              { IF }
| "else"            { ELSE }

| digit+ as n       { INT(int_of_string n) }
| "true"            { BOOL(true) }
| "false"           { BOOL(false) }
| id as x           { VAR(x) }
| "()"              { UNIT }

| ws                { token lexbuf }
| '\n'              { Lexing.new_line lexbuf; token lexbuf }

| eof               { EOF }

| _ as c            {
                        let pos = lexbuf.Lexing.lex_curr_p in
                        printf "Error at line %d\n" pos.Lexing.pos_lnum;
                        printf "Unrecognized character: [%c]\n" c;
                        exit 1
                    }

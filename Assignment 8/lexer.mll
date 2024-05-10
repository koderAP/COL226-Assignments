{
  open Parser;;
  exception InvalidToken of char ;;
}

rule token = parse
    eof                                                             {EOF}
  | [' ' '\t' '\n' '\r']+                                           {token lexbuf}
  | ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as v                    {VAR(v)}
  | ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as c                    {CONS(c)} 
  | '0'|['1'-'9']['0'-'9']* as n                                    {NUM(int_of_string n)}
  | '('                                                             {LP}
  | ')'                                                             {RP}
  | '['                                                             {LB}
  | ']'                                                             {RB}
  | '('                                                             {LP}
  | ')'                                                             {RP}
  | ','                                                             {COMMA}
  | '='                                                             {EQ}
  | '+'                                                             {PLUS}
  | '-'                                                             {MINUS}
  | '*'                                                             {MULT}
  | '/'                                                             {DIV}
  | '>'                                                             {GT}
  | '<'                                                             {LT}
  | "\\="                                                           {NOT_EQ}
  | '|'                                                             {PIPE}
  | ['i']['s']                                                      {IS}
  | '!'                                                             {CUT}
  | "fail"                                                          {FAIL}
  | "true"                                                          {TRUE}
  | "false"                                                         {FALSE}
  | '_'                                                            {VAR("_")}
  | '.'                                                             {ENDL}
  | ":-"                                                            {COND}
  | '%'                                                             {single_line_comment lexbuf}
  | "/*"                                                            {multi_line_comment 0 lexbuf}
  | _ as s                                                          {raise (InvalidToken s)}

and single_line_comment = parse
    eof                   {EOF}
  | '\n'                  {token lexbuf}
  |   _                   {single_line_comment lexbuf}

and multi_line_comment depth = parse
    eof                   {failwith "Syntax error: End of file in /* ... */ comment"}
  | "*/"                  {if depth = 0 then token lexbuf else multi_line_comment (depth-1) lexbuf}
  | "/*"                  {multi_line_comment (depth+1) lexbuf}
  |  _                    {multi_line_comment depth lexbuf}

{
open Parser
}

let var = ['A'-'Z']['a'-'z''A'-'Z''_''0'-'9']*


rule token = parse
  | [' ' '\t' '\r' '\n']  { token lexbuf }
  | '.'              { DOT }
  | ','              { COMMA }
  | '?'              { QMARK }
  | '('              { LPAREN }
  | ')'              { RPAREN }
  | '|'              { PIPE }
  | '!'              { CUT }
  | '['              { LBRACKET }
  | ']'              { RBRACKET } 
  | ":-"            { IF }
  | "is"            { IS }
  | var|['_'] as lxm { VARIABLE lxm }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* as lxm { CONSTANT lxm }
  | ['N']['U']['M']['E']['R']['A']['L']['(']['1'-'9']['0'-'9']*[')'] as lxm { CONSTANT lxm }
  | ['N']['U']['M']['E']['R']['A']['L']['(']['0'][')'] as lxm { CONSTANT lxm }
  | ['1'-'9']['0'-'9']* as lxm { CONSTANT lxm }
  | eof              { EOF }
  | _       { failwith ("Unknown token: " ^ Lexing.lexeme lexbuf) }

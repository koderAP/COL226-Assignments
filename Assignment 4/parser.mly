%{
open Ast
%}

%token <string> VARIABLE CONSTANT NUMBER
%token LPAREN RPAREN COMMA DOT IF LBRACKET RBRACKET QMARK CUT PIPE IS
%token EOF
%start program
%type <Ast.program> program
%%

program:
  | clauses EOF { $1 }

clauses:
  | clause DOT clauses { $1 :: $3 }
  | clause DOT        { [$1] }
  | QMARK query DOT     { [$2] }

clause:
  | atomic_formula IF atomic_formulas { Rule($1, $3) }
  | atomic_formula             { Fact($1) }

query:
  | goal                 { Goal($1) }

goal:
  | atomic_formula                 { [$1] }
  | atomic_formula COMMA goal     { $1 :: $3 }

atomic_formula:
  | CONSTANT LPAREN terms RPAREN { ($1, $3) }
  | CONSTANT                     { ($1, []) }
  | CUT                          { ("!", []) }


terms:
  | term COMMA terms { $1 :: $3 }
  | term PIPE term   { $1 :: [$3] }
  | term             { [$1] }

term:
  | VARIABLE  { Variable($1) }
  | CONSTANT  { Constant($1) }
  | CONSTANT LPAREN terms RPAREN { Function($1, $3) }
  | VARIABLE LPAREN terms RPAREN { Function($1, $3) }
  | LBRACKET terms RBRACKET { List($2) }
  | LPAREN terms RPAREN { Tuple($2) }



atomic_formulas:
  | atomic_formula COMMA atomic_formulas { $1 :: $3 }
  | atomic_formula                       { [$1] }
%%


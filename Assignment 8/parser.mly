%{
    open Ast;;
%}

%token <string> VAR CONS
%token <int> NUM
%token LP RP LB RB COMMA EQ NOT_EQ ENDL CUT COND PIPE PLUS MINUS MULT DIV GT LT 
EOF IS FAIL TRUE FALSE

%left COMMA
%nonassoc EQ PIPE LT GT IS
%left PLUS MINUS
%left MULT DIV
%nonassoc ENDL

%start program goal
%type <Ast.program> program
%type <Ast.goal> goal
%%

program:
    EOF                                 {[]}
  | clauses EOF                     {$1}
;

clauses:
    clause                              {[$1]}
  | clause clauses                  {($1)::$2}
;

clause:
    atom ENDL                           {Fact($1)}
  | atom COND atom_list ENDL            {Rule($1, $3)}
;

goal:
    atom_list ENDL                      {GOAL($1)}
;

atom_list:
    atom                                {[$1]}
  | atom COMMA atom_list                {($1)::$3}
;

atom:
  | CONS                                {ATOM($1, [])}
  | CONS LP term_list RP                {ATOM($1, $3)}
  | term EQ term                        {ATOM("=", [$1; $3])}
  | term NOT_EQ term                    {ATOM("!=", [$1; $3])}
  | term LT term                        {ATOM("<", [$1; $3])}
  | term GT term                        {ATOM(">", [$1; $3])}
  | TRUE                                {ATOM("true", [])}
  | FALSE                               {ATOM("false", [])}
  | CUT                                 {ATOM("!", [])}
;

term_list:
    term                                {[$1]}
  | term COMMA term_list                {($1)::$3}
;

term:
    LP term RP                          {$2}
  | VAR                                 {Var($1)}
  | CONS                                {Node($1, [])}
  | NUM                                 {Num($1)}
  | CONS LP term_list RP                {Node($1, $3)}
  | term PLUS term                      {Node("+", [$1; $3])}
  | term MINUS term                     {Node("-", [$1; $3])}
  | term MULT term                      {Node("*", [$1; $3])}
  | term DIV term                       {Node("/", [$1; $3])}
  | list                                {$1}
  | tuple                               {$1}
;

list:
    LB RB                               {Node("[]", [])}
  | LB list_body RB                     {$2}
;

tuple:
    LP RP                               {Node("()", [])}
  | LP term COMMA term_list RP           {Node("()", [$2] @ $4)}


list_body:
    term                                 {Node("[ _ ]", [$1; Node("[]", [])])}
  | term COMMA list_body                 {Node("[ _ ]", [$1; $3])}
  | term PIPE term                       {Node("[ _ ]", [$1; $3])}
;





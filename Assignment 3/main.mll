{
  type token =
    | Identifier of string
    | Keyword of string
    | BoolOp of string
    | BoolConst of bool
    | BitOp of string
    | ArithOp of string
    | IntConst of int
    | CompOp of string
    | StringOp of string
    | TupleOp of string
    | StringConst of string
    | ParenOpen
    | ParenClose
    | Comma
    | EOF

}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ['(']                { ParenOpen }
  | [')']                { ParenClose }
  | [',']                { Comma }
  | ['+' ]               { ArithOp "+" }
  | ['-']                { ArithOp "-" }
  | ['*']                { ArithOp "*" }
  | ['/']                { ArithOp "/" }
  | ['%']                { ArithOp "%" }
  | ['!']['&']           { BoolOp "!&" }
  | ['.']['f']['i']['r']['s']['t']  { TupleOp ".first" }
  | ['.']['s']['e']['c']['o']['n']['d']  { TupleOp ".second" }
  | ['<'][ '<']           { BitOp "<<" }
  | ['>'][ '>']           { BitOp ">>" }
  | ['<'][ '=' ]           { CompOp "<=" }  
  | ['>'][ '=' ]           { CompOp ">=" }
  | ['=']                { CompOp "=" }
  | ['!']['=']                { CompOp "!=" }
  | ['<']                { CompOp "<" }
  | ['>']                { CompOp ">" }
  | ['^']                 { StringOp "^ (Concatenation)"}
  | ['|']                 { BitOp "|"}
  | ['&']['&']                 { BitOp "&&"}
  | ['i'] ['f']            { Keyword "if" }
  | ['t']['h']['e']['n']    { Keyword "then" }
  | ['e']['l']['s']['e']    { Keyword "else" }
  | ['a'] ['n']['d']        { Keyword "and" }
  | ['o'] ['r']            { Keyword "or" }
  | ['n'] ['o'] ['t']        { Keyword "not" }
  | ['t'] ['r'] ['u'] ['e']    { BoolConst true }
  | ['f'] ['a'] ['l'] ['s'] ['e']{ BoolConst false }
  | ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* as id  { Identifier (id) }
  | ['0']  { IntConst (int_of_string "0") }  
  | ['1'-'9']['0'-'9']* as num    { IntConst (int_of_string num) }
  | ['\"']['a'-'z' 'A'-'Z' '0'-'9' '_' ' ' '\'' '$' '#' '@' '^' '&' '*']* ['\"'] as id { StringConst (id) }
  | eof                  { EOF }
  | _ as c               { print_endline ("Invalid literal: " ^ Char.escaped c); token lexbuf }


{

let parse input =
  let lexbuf = Lexing.from_string input in
  let rec parse_tokens acc =
    match token lexbuf with
    | EOF -> List.rev acc
    | tok -> parse_tokens (tok :: acc)
  in
  parse_tokens []


let read_test_cases filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []

let test_cases = read_test_cases "test_cases.txt"


let () =
  List.iter (fun input ->
      let tokens = parse input in
      Printf.printf "Input: %s\nTokens: [%s]\n\n" input (String.concat "; " (List.map (function
          | Identifier s -> "Identifier : " ^ s ^ ""
          | Keyword s -> "Keyword : " ^ s ^ "" 
          | BoolOp s -> "BoolOp : " ^ s ^ ""
          | BoolConst b -> "BoolConst : " ^ string_of_bool b ^ ""
          | ArithOp s -> "ArithOp : " ^ s ^ ""
          | IntConst i -> "IntConst : " ^ string_of_int i ^ ""
          | CompOp s -> "CompOp : " ^ s ^ ""
          | StringOp s -> "StringOp : " ^ s ^ ""
          | StringConst s -> "StringConst : " ^ s ^ ""
          | ParenOpen -> "ParenOpen"
          | ParenClose -> "ParenClose"
          | BitOp s -> "BitOp : " ^ s ^ ""
          | Comma -> "Comma"
          |TupleOp s -> "TupleOp : " ^ s ^ ""
          | EOF -> "EOF") tokens))) test_cases

}




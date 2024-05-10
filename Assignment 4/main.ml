open Ast

let parse_program input =
  Parser.program Lexer.token input 

let rec string_of_term = function
  | Constant c -> "Constant \"" ^ c ^ "\""
  | Variable v -> "Variable \"" ^ v ^ "\""
  | Function (name, args) ->
    "Function : \"" ^ name  ^ "\"" ^ ", args : [" ^ String.concat "; " (List.map string_of_term args) ^ "]"
  | List l -> "List [" ^ String.concat "; " (List.map string_of_term l) ^ "]"
  | Tuple t -> "Tuple (" ^ String.concat "; " (List.map string_of_term t) ^ ")"

let rec string_of_atomic_formula (name, terms) =
  let name_str = "Atom_Symbol: \"" ^ name ^ "\"" in
  let terms_str = String.concat "; " (List.map string_of_term terms) in
  name_str ^ ", [" ^ terms_str ^ "]"

let string_of_predicate (terms) =
  let terms_str = String.concat ",\n            " (List.map string_of_atomic_formula terms) in "[ " ^ terms_str ^ "]"

let string_of_clause = function
  | Fact predicate -> 
    "        Fact (\n" ^ 
    "            Head (" ^ 
    string_of_atomic_formula predicate ^ 
    ")" ^
    "\n          )"
  | Rule (head, body) ->
    "        Rule (\n" ^ 
    "            Head (" ^ 
    string_of_atomic_formula head ^ 
    "),\n            Body [\n              " ^ 
    String.concat ",\n              " (List.map string_of_atomic_formula body) 
    ^ "\n            ]\n          )"
  | Goal goal ->
    "        Goal (\n" ^ 
    "            " ^ 
    string_of_predicate goal ^ 
    "\n          )"

   

let string_of_program program =
  "    Clause [\n" ^
  String.concat ";\n  " (List.map string_of_clause program) ^ "\n" ^
  "        ]\n"

let () =
  let filename = "testcases.txt" in
  let output_file = "output.txt" in
  let file = open_in filename in
  let output_channel = open_out output_file in
  try
    output_string output_channel "Program (\n";
    while true do
      let input_line = input_line file in
      let input_program = Lexing.from_string input_line in
      let ast_program = parse_program input_program in
      let formatted_program = string_of_program ast_program in
      output_string output_channel (formatted_program ^ "\n");
    done

  with End_of_file ->
    output_string output_channel ")\n";
    close_in file;
    close_out output_channel

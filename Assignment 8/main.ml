open Ast;;
open Lexer;;
open Parser;;


let rec updateClause (cl:clause) (i:int): clause =
  let rec updateAtom (i:int) (a:atom): atom =
    match a with
    | ATOM(s, l) -> ATOM(s, List.map (updateTerm i) l)
  and updateTerm (i:int) (t:term): term =
    match t with
    | Var(v) -> Var((string_of_int i) ^ v)
    | Node(s, l) -> Node(s, List.map (updateTerm i) l)
    | _ -> t
  in
  match cl with
  | Fact(a) -> Fact((updateAtom i a))
  | Rule(a, l) -> Rule((updateAtom i a), (List.map (updateAtom i) l))
  

let rec update_program (prog:program) (i:int): program = match prog with
    [] -> []
  | cl::ps -> (updateClause cl i)::update_program ps (i+1)


let rec subst (s:substitution) (t:term): term =
  match t with
      Node(s', l) -> Node(s', List.map (subst s) l)
    | Num(_) -> t
    | Var(x) -> match s with
                  [] -> t
                | s'::xs -> if fst s' = x then snd s' else subst xs t

let rec subst_atom (s:substitution) (ATOM(s', l): atom): atom = ATOM(s', List.map (subst s) l)

let rec variableInTerm (v:variable) (t:term): bool =
  match t with
      Var(x) -> x = v
    | Node(s, l) -> List.fold_left (||) false (List.map (variableInTerm v) l)
    | _ -> false

let compose (s1:substitution) (s2:substitution): substitution =
  let f s x = (fst x, subst s (snd x)) in (List.map (f s2) s1) @ s2

let rec mgu_term (t1:term) (t2:term): substitution =
  match (t1, t2) with
      (Var(x), Var(y)) -> if x = y then []
                      else [(x, Var(y))]
    | (Var(x), Node(_, _)) -> if variableInTerm x t2 then raise NOT_UNIFIABLE
                            else [(x, t2)]
    | (Node(_, _), Var(y)) -> if variableInTerm y t1 then raise NOT_UNIFIABLE
                            else [(y, t1)]
    | (Num(n1), Num(n2)) -> if n1 = n2 then [] else raise NOT_UNIFIABLE
    | (Num(n1), Var(x)) -> [(x, t1)]
    | (Var(x), Num(n2)) -> [(x, t2)] 
    | (Node(s1, l1), Node(s2, l2)) ->
        if s1 <> s2 || (List.length l1 <> List.length l2) then raise NOT_UNIFIABLE
        else
          let f s tt = compose s (mgu_term (subst s (fst tt)) (subst s (snd tt))) in
          List.fold_left f [] (List.combine l1 l2)
    | _ -> raise NOT_UNIFIABLE

let mgu_atom (ATOM(s1, l1): atom) (ATOM(s2, l2): atom): substitution = mgu_term (Node(s1, l1)) (Node(s2, l2))

let rec print_term_list (tl:term list) = match tl with
    [] -> Printf.printf ""
  | [t] -> print_term t
  | t::tls -> (
      print_term t;
      Printf.printf ",";
      print_term_list tls;
    )

and print_list_body (t:term) = match t with
    Node("[]", []) -> Printf.printf ""
  | Node("()", [t1; t2]) -> (
      print_term t1;
      Printf.printf ",";
      print_list_body t2;
    )
  | Node("[ _ ]", [t1; Node("[]", [])]) -> print_term t1
  | Node("[ _ ]", [t1; t2]) -> (
      print_term t1;
      Printf.printf ",";
      print_list_body t2;
    )
  | _ -> print_string " _ "

and print_term (t:term) = match t with
    Var(v) -> Printf.printf " %s " v
  | Node("[]", []) -> Printf.printf " [] "
  | Node(s, []) -> Printf.printf " %s " s
  | Node("[ _ ]", _) -> (
      Printf.printf " [";
      print_list_body t;
      Printf.printf "] ";
    )
  | Node("()", [t1; t2]) -> (
      Printf.printf " (";
      print_term t1;
      Printf.printf ",";
      print_term t2;
      Printf.printf ") ";
    )
  | Node(s, l) -> (
      Printf.printf " %s ( " s;
      print_term_list l;
      Printf.printf " ) ";
    )
  | Num(n) -> Printf.printf " %d " n

let rec getSolution (unif:substitution) (vars:variable list) = match vars with
    [] -> []
  | v::vs ->
      let rec occurs l = match l with
          [] -> raise NotFound
        | x::xs -> if (fst x) = v then x
                    else occurs xs
      in
      try (occurs unif)::getSolution unif vs
      with NotFound -> getSolution unif vs

let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
          { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

let rec printSolution (unif:substitution) = match unif with
    [] -> Printf.printf "true. "
  | [(v, t)] -> (
      Printf.printf "%s =" v;
      print_term t;
    )
  | (v, t)::xs -> (
      Printf.printf "%s =" v;
      print_term t;
      Printf.printf ", ";
      printSolution xs;
    )

let solve_atom_atom (a1:atom) (a2:atom) (unif:substitution): substitution =
  compose unif (mgu_atom (subst_atom unif a1) (subst_atom unif a2))

let solve_term_term (t1:term) (t2:term) (unif:substitution): substitution =
  compose unif (mgu_term (subst unif t1) (subst unif t2))

let rec simplify_term (t:term): term = match t with
    Num(_) -> t
  | Node("+", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Num(n1), Num(n2)) -> Num(n1 + n2)
        | _ -> raise NOT_UNIFIABLE
    )
  | Node("-", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Num(n1), Num(n2)) -> Num(n1 - n2)
        | _ -> raise NOT_UNIFIABLE
    )
  | Node("*", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Num(n1), Num(n2)) -> Num(n1 * n2)
        | _ -> raise NOT_UNIFIABLE
    )
  | Node("/", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Num(n1), Num(n2)) -> Num(n1 / n2)
        | _ -> raise NOT_UNIFIABLE
      )
  | _ -> t



let rec evaluate_goal (prog:program) (g:goal) (unif:substitution) (vars:variable list): (bool * substitution) =
  let eval (a:atom) (unif:substitution): substitution = match a with
    ATOM("=", [t1; t2])
  | ATOM("!=", [t1; t2]) -> compose unif (mgu_term (simplify_term (subst unif t1)) (simplify_term (subst unif t2)))
  | ATOM(">", [t1; t2]) -> (
        match (simplify_term (subst unif t1), simplify_term (subst unif t2)) with
            (Num(n1), Num(n2)) -> if n1 > n2 then unif else raise NOT_UNIFIABLE
          | _ -> raise NOT_UNIFIABLE
    )
  | ATOM("<", [t1; t2]) -> (
      match (simplify_term (subst unif t1), simplify_term (subst unif t2)) with
          (Num(n1), Num(n2)) -> if n1 < n2 then unif else raise NOT_UNIFIABLE
        | _ -> raise NOT_UNIFIABLE
    )
  | _ -> unif in
  match g with
      GOAL([]) -> (
        printSolution (getSolution unif vars);
        flush stdout;
        Printf.printf "\n";
        (true, [])
      )
    | GOAL(a::gs) -> match a with
          ATOM("=", _) | ATOM(">", _) | ATOM("<", _) -> (
            try evaluate_goal prog (GOAL(gs)) (eval a unif) vars
            with NOT_UNIFIABLE -> (false, [])
          )
        | ATOM("!=", _) -> (
            try (false, eval a unif)
            with NOT_UNIFIABLE -> evaluate_goal prog (GOAL(gs)) unif vars
          )
        | ATOM("!", _) -> let _ = evaluate_goal prog (GOAL(gs)) unif vars in (true, [])
        | _ ->
          let rec update_prog_local (prog:program) (ATOM(s, _): atom): program = match prog with
            [] -> []
          | cl::ps -> match cl with Fact(ATOM(s', _)) | Rule(ATOM(s', _), _) ->
                        if s = s' then (updateClause cl 0)::update_prog_local ps (ATOM(s, []))
                        else cl::update_prog_local ps (ATOM(s, [])) in
          let new_prog = update_prog_local prog a in
          let rec iter prog' = match prog' with
              [] -> (false, [])
            | cl::ps -> match cl with
                Fact(a') -> (
                  try
                    let u = (solve_atom_atom a' a unif) in
                    match (evaluate_goal new_prog (GOAL(gs)) u vars) with
                        (true, u') -> (true, u')
                      | _ -> iter ps
                  with NOT_UNIFIABLE -> iter ps
                )
              | Rule(a', al) -> (
                  try
                    let u = (solve_atom_atom a' a unif) in
                    match (evaluate_goal new_prog (GOAL(al @ gs)) u vars) with
                        (true, u') -> (true, u')
                      | _ -> iter ps
                  with NOT_UNIFIABLE -> iter ps
                )
        in iter prog



let fstream = open_in "inputfile.txt";;
let init_prog = Parser.program Lexer.token (Lexing.from_channel fstream);;
let _ = 
  let rec is_valid_prog (prog:program): bool = match prog with
    [] -> true
  | (Fact(a))::xs | (Rule(a, _))::xs -> match a with
          ATOM("=", _) | ATOM("!=", _) | ATOM("!", _)
        | ATOM(">", _) | ATOM("<", _)-> raise InvalidProgram
        | _ -> is_valid_prog xs in 
  is_valid_prog init_prog;;
let prog = update_program init_prog 1;;

print_string "Program loaded successfully\n Type \"halt.\" to escape \n";;

try
  while(true) do
    print_string "?- ";
    let line = read_line() in
    if line = "halt." then exit 0
    else try
      let g = Parser.goal Lexer.token (Lexing.from_string line) in
        let interpret_goal (prog:program) (g:goal) = 
          let rec vars_goal (GOAL(g): goal): variable list = List.fold_left (fun acc x -> acc @ x) [] (List.map vars_atom g) 
        and  vars_atom (ATOM(s, l): atom): variable list = 
          let rec vars_term (t:term): variable list =
            match t with
                Var(v) -> [v]
              | Node(s, l) -> List.fold_left (fun acc x -> acc @ x) [] (List.map vars_term l)
              | _ -> [] in
            vars_term (Node(s, l)) in
          evaluate_goal prog g [] (vars_goal g) in
      match (interpret_goal prog g) with
          (true, _) -> print_string "true.\n"
        | (false, _) -> print_string "false.\n"
    with e -> Printf.printf "%s\n" (Printexc.to_string e)
  done

with _ -> print_string "\n% halt\n"





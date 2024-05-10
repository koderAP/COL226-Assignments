type exp =
  | V of string
  | Abs of string * exp
  | App of exp * exp
  | Num of int
  | Bool of bool
  | Plus of exp * exp
  | Mult of exp * exp
  | Sub of exp * exp
  | Div of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Eq of exp * exp
  | Gt of exp * exp
  | Not of exp
  | IfTE of exp * exp * exp
  | Def of string * exp
  | Let of exp list
  | Tuple of exp list
  | Projection of exp * int
  | Case of exp * ((exp * exp) list)
  
type opcode = 
  | NUM of int
  | BOOL of bool
  | ADD
  | MULT
  | SUB
  | DIV
  | AND
  | OR
  | EQ
  | GT
  | NOT
  | IFTE of opcode list * opcode list
  | APP
  | DEF of string
  | LET of opcode list
  | LOOKUP of string
  | RET
  | MKCLOS of string * opcode list
  | TUPLE of (opcode list) list
  | PROJECTION of int
  | CASE of opcode list list list

                


type answer =
  | Closure of string * opcode list * environment
  | NumVal of int
  | BoolVal of bool
  | TupleVal of answer list

and environment = (string * answer) list

type stack = answer list
type dump = (stack * environment * opcode list) list
type config = stack * environment * opcode list * dump


let rec compile (e : exp) : opcode list =
  match e with
  | V x -> [LOOKUP x]
  | Abs (x, e') -> [MKCLOS (x, compile e' @ [RET])]
  | App (e1, e2) -> compile e1 @ compile e2 @ [APP]
  | Num n -> [NUM n]
  | Bool b -> [BOOL b]
  | Plus (e1, e2) -> compile e1 @ compile e2 @ [ADD]
  | Mult (e1, e2) -> compile e1 @ compile e2 @ [MULT]
  | Sub (e1, e2) -> compile e1 @ compile e2 @ [SUB]
  | Div (e1, e2) -> compile e1 @ compile e2 @ [DIV]
  | And (e1, e2) -> compile e1 @ compile e2 @ [AND]
  | Or (e1, e2) -> compile e1 @ compile e2 @ [OR]
  | Eq (e1, e2) -> compile e1 @ compile e2 @ [EQ]
  | Gt (e1, e2) -> compile e1 @ compile e2 @ [GT]
  | Not e' -> compile e' @ [NOT]
  | IfTE (e1, e2, e3) -> compile e1 @ [IFTE (compile e2, compile e3)]
  | Def (x, e') -> compile e' @ [DEF x]
  | Tuple e' -> [TUPLE (List.map compile e')]
  | Projection (e1, e2)  -> compile e1 @ [PROJECTION(e2)]
  | Case (e1, e2) ->
      let compiledPatterns = List.map (fun (pattern, exp) -> [compile pattern; compile exp]) e2 in
      compile e1 @ [CASE compiledPatterns]
  | Let e' ->
    let rec compile' (e' : exp list) : opcode list =
      match e' with
      | [] -> []
      | e1 :: e2 -> compile e1 @ compile' e2 
    in [LET (compile' e'@ [RET])] 
  

let rec run (s : stack) (e : environment) (c : opcode list) (d : dump) : stack =
  match c with
  | [] -> s
  | NUM n :: c' -> run (NumVal n :: s) e c' d
  | BOOL b :: c' -> run (BoolVal b :: s) e c' d
  | ADD :: c' -> (match s with
      | NumVal n2 :: NumVal n1 :: s' -> run (NumVal (n1 + n2) :: s') e c' d
      | TupleVal l :: NumVal n :: s' -> run (TupleVal (List.map (fun x -> match x with NumVal n' -> NumVal (n' + n) | _ -> x) l) :: s') e c' d
      | TupleVal l :: TupleVal l' :: s' -> run (TupleVal (List.map2 (fun x y -> match x, y with NumVal n, NumVal n' -> NumVal (n + n') | _ -> x) l l') :: s') e c' d
      | _ -> failwith "type error add run")
  | MULT :: c' -> (match s with
      | NumVal n2 :: NumVal n1 :: s' -> run (NumVal (n1 * n2) :: s') e c' d
      | TupleVal l :: NumVal n :: s' -> run (TupleVal (List.map (fun x -> match x with NumVal n' -> NumVal (n' * n) | _ -> x) l) :: s') e c' d
      | TupleVal l :: TupleVal l' :: s' -> run (TupleVal (List.map2 (fun x y -> match x, y with NumVal n, NumVal n' -> NumVal (n * n') | _ -> x) l l') :: s') e c' d
      | _ -> failwith "type error mult run")
  | SUB :: c' -> (match s with
      | NumVal n2 :: NumVal n1 :: s' -> run (NumVal (n1 - n2) :: s') e c' d
      | TupleVal l :: NumVal n :: s' -> run (TupleVal (List.map (fun x -> match x with NumVal n' -> NumVal (n' - n) | _ -> x) l) :: s') e c' d
      | TupleVal l :: TupleVal l' :: s' -> run (TupleVal (List.map2 (fun x y -> match x, y with NumVal n, NumVal n' -> NumVal (n - n') | _ -> x) l l') :: s') e c' d
      | _ -> failwith "type error sub run")
  | DIV :: c' -> (match s with
      | NumVal n2 :: NumVal n1 :: s' -> run (NumVal (n1 / n2) :: s') e c' d
      | TupleVal l :: NumVal n :: s' -> run (TupleVal (List.map (fun x -> match x with NumVal n' -> NumVal (n' / n) | _ -> x) l) :: s') e c' d
      | _ -> failwith "type error  div run")
  | AND :: c' -> (match s with
      | BoolVal b2 :: BoolVal b1 :: s' -> run (BoolVal (b1 && b2) :: s') e c' d
      | TupleVal l :: BoolVal b :: s' -> run (TupleVal (List.map (fun x -> match x with BoolVal b' -> BoolVal (b' && b) | _ -> x) l) :: s') e c' d
      | TupleVal l :: TupleVal l' :: s' -> run (TupleVal (List.map2 (fun x y -> match x, y with BoolVal b, BoolVal b' -> BoolVal (b && b') | _ -> x) l l') :: s') e c' d
      | _ -> failwith "type error and run")
  | OR :: c' -> (match s with
      | BoolVal b2 :: BoolVal b1 :: s' -> run (BoolVal (b1 || b2) :: s') e c' d
      | TupleVal l :: BoolVal b :: s' -> run (TupleVal (List.map (fun x -> match x with BoolVal b' -> BoolVal (b' || b) | _ -> x) l) :: s') e c' d
      | TupleVal l :: TupleVal l' :: s' -> run (TupleVal (List.map2 (fun x y -> match x, y with BoolVal b, BoolVal b' -> BoolVal (b || b') | _ -> x) l l') :: s') e c' d
      | _ -> failwith "type error or run")
  | GT :: c' -> (match s with
      | NumVal n2 :: NumVal n1 :: s' -> run (BoolVal (n1 > n2) :: s') e c' d
      | _ -> failwith "type error gt run")
  | EQ :: c' -> (match s with
      | NumVal n2 :: NumVal n1 :: s' -> run (BoolVal (n1 = n2) :: s') e c' d
      | TupleVal l :: TupleVal l' :: s' -> run (TupleVal (List.map2 (fun x y -> match x, y with NumVal n, NumVal n' -> BoolVal (n = n') | _ -> x) l l') :: s') e c' d
      | _ -> failwith "type error eq run")
  | NOT :: c' -> (match s with
      | BoolVal b :: s' -> run (BoolVal (not b) :: s') e c' d
      | TupleVal l :: s' -> run (TupleVal (List.map (fun x -> match x with BoolVal b -> BoolVal (not b) | _ -> x) l) :: s') e c' d
      | _ -> failwith "type error not run")
  | IFTE (c1, c2) :: c' -> (match s with
      | BoolVal true :: s' -> run s' e (c1 @ c') d
      | BoolVal false :: s' -> run s' e (c2 @ c') d
      | _ -> failwith "type error ifte run")
  | CASE c1 :: c' -> (match c1 with
    | [] -> run s e c' d
    | c1' :: c1'' -> (
      match c1' with 
      | [a1 ; a2] -> (match s with
        | a :: s' -> if [a] = run [] e a1 d then run s' e (a2 @ c') d else run s e (CASE c1'' :: c') d
        | _ -> failwith "type error case run"
    ) | _ -> failwith "type error case run"

  ))

  | PROJECTION c1 :: c' -> (match s with
      | TupleVal l :: s' -> run (List.nth l c1 :: s') e c' d
      | _ -> failwith "type error projection run"
  )
  
  | DEF x :: c' -> (match s with 
      | v :: s' -> run s' ((x, v) :: e) c' d
      | _ -> failwith "type error  def run")

  | LET c1 :: c' -> run s e c1 ((s, e, c') :: d)
  | TUPLE c1 :: c' -> 
    let rec run' (c1 : (opcode list) list) (s' : stack) : stack =
      match c1 with
      | [] -> s'
      | c1' :: c1'' -> run' c1'' (run s' e c1' d)
    in run (TupleVal (List.rev(run' c1 [])) :: s) e c' d   
    
  | APP :: c' -> (match s with
      | v :: Closure (x, c1, e1) :: s' -> run [] ((x, v) :: e1) c1 ((s', e, c') :: d)
      | _ -> failwith "type error app run")
  | RET :: c' -> (match s with
      | [] -> (match d with
          | (s'', e', c'') :: d' -> run s'' e' c'' d'
          | _ -> failwith "empty dump or type error")
      | a :: s' -> (match d with
          | (s'', e', c'') :: d' -> run (a :: s'') e' c'' d'
          | _ -> failwith "empty dump or type error")
      | _ -> failwith "empty stack or type error")
  
  | LOOKUP x :: c' -> (match List.assoc_opt x e with
      | Some v -> run (v :: s) e c' d
      | None -> failwith (x ^ " not found in environment"))
  
  | MKCLOS (x, c1) :: c' -> run (Closure (x, c1, e) :: s) e c' d
  | _ -> failwith "type error run"


let rec run1 (s : stack) (e : environment) (c : opcode list) (d : dump) : stack * environment =
  match c with
  | [] -> (s , e)
  | NUM n :: c' -> run1 (NumVal n :: s) e c' d
  | BOOL b :: c' -> run1 (BoolVal b :: s) e c' d
  | ADD :: c' -> (match s with
      | NumVal n2 :: NumVal n1 :: s' -> run1 (NumVal (n1 + n2) :: s') e c' d
      | _ -> failwith "type error add run1")
  | MULT :: c' -> (match s with
      | NumVal n2 :: NumVal n1 :: s' -> run1 (NumVal (n1 * n2) :: s') e c' d
      | _ -> failwith "type error mult run1")
  | SUB :: c' -> (match s with
      | NumVal n2 :: NumVal n1 :: s' -> run1 (NumVal (n1 - n2) :: s') e c' d
      | _ -> failwith "type error sub run1")
  | DIV :: c' -> (match s with
      | NumVal n2 :: NumVal n1 :: s' -> run1 (NumVal (n1 / n2) :: s') e c' d
      | _ -> failwith "type error div run1")
  | AND :: c' -> (match s with
      | BoolVal b2 :: BoolVal b1 :: s' -> run1 (BoolVal (b1 && b2) :: s') e c' d
      | _ -> failwith "type error and run1")
  | OR :: c' -> (match s with
      | BoolVal b2 :: BoolVal b1 :: s' -> run1 (BoolVal (b1 || b2) :: s') e c' d
      | _ -> failwith "type error or run1")
  | GT :: c' -> (match s with
      | NumVal n2 :: NumVal n1 :: s' -> run1 (BoolVal (n1 > n2) :: s') e c' d
      | _ -> failwith "type error gt run1")
  | EQ :: c' -> (match s with
      | NumVal n2 :: NumVal n1 :: s' -> run1 (BoolVal (n1 = n2) :: s') e c' d
      | _ -> failwith "type error eq run1")

  | CASE c1 :: c' -> (match c1 with
    | [] -> run1 s e c' d
    | c1' :: c1'' -> (
      match c1' with 
      | [a1 ; a2] -> (match s with
        | a :: s' -> if [a] = run [] e a1 d then run1 s' e (a2 @ c') d else run1 s e (CASE c1'' :: c') d
        | _ -> failwith "type error case run1"
    ) | _ -> failwith "type error case run1"

  ))
  | NOT :: c' -> (match s with
      | BoolVal b :: s' -> run1 (BoolVal (not b) :: s') e c' d
      | _ -> failwith "type error not run1")
  | PROJECTION c1 :: c' -> (match s with
      | TupleVal l :: s' -> run1 (List.nth l c1 :: s') e c' d
      | _ -> failwith "type error projection run1"
  )
  | IFTE (c1, c2) :: c' -> (match s with
      | BoolVal true :: s' -> run1 s' e (c1 @ c') d
      | BoolVal false :: s' -> run1 s' e (c2 @ c') d
      | _ -> failwith "type error ifte run1")
  
  | DEF x :: c' -> (match s with 
      | v :: s' -> run1 s' ((x, v) :: e) c' d
      | _ -> failwith "type error def run1")

  | LET c1 :: c' -> run1 s e c1 ((s, e, c') :: d)
  | TUPLE c1 :: c' -> 
    let rec run' (c1 : (opcode list) list) (s' : stack) : stack =
      match c1 with
      | [] -> s'
      | c1' :: c1'' -> run' c1'' (run s' e c1' d)
    in run1 (TupleVal (List.rev(run' c1 [])) :: s) e c' d
    
  | APP :: c' -> (match s with
      | v :: Closure (x, c1, e1) :: s' -> run1 [] ((x, v) :: e1) c1 ((s', e, c') :: d)
      | _ -> failwith "type error app run1")
  | RET :: c' -> (match s with
      | [] -> (match d with
          | (s'', e', c'') :: d' -> run1 s'' e' c'' d'
          | _ -> failwith "empty dump or type error")
      | a :: s' -> (match d with
          | (s'', e', c'') :: d' -> run1 (a :: s'') e' c'' d'
          | _ -> failwith "empty dump or type error")
      | _ -> failwith "empty stack or type error")
  | LOOKUP x :: c'
    -> (match List.assoc_opt x e with
        | Some v -> run1 (v :: s) e c' d
        | None -> failwith (x ^ " not found in environment"))
  | MKCLOS (x, c1) :: c' -> run1 (Closure (x, c1, e) :: s) e c' d
  | _ -> failwith "type error run1"


let execute (e : exp) : stack =
  run [] [] (compile e) []

let execute1 (e : exp list) : stack =
  let rec execute1' (e : exp list) (s : stack) (e' : environment) (c : opcode list) (d : dump) : stack =
    match e with
    | [] -> run s e' c d
    | e1 :: e2 -> 
        let (s', e'') = run1 s e' (compile e1) d in
        execute1' e2 s' e'' c d
  in execute1' e [] [] [] []
    
let execute2 (e : exp) : stack * environment =
  run1 [] [] (compile e) []

let execute3 (e : exp list) : stack * environment =
  let rec execute3' (e : exp list) (s : stack) (e' : environment) (c : opcode list) (d : dump) : stack * environment =
    match e with
    | [] -> run1 s e' c d
    | e1 :: e2 -> 
        let (s', e'') = run1 s e' (compile e1) d in
        execute3' e2 s' e'' c d
  in execute3' e [] [] [] []
  

let test1 = execute1 [Num 1] = [NumVal 1]
let test2 = execute1 [Bool true] = [BoolVal true]
let test3 = execute (Plus (Num 1, Num 2)) = [NumVal 3]
let test4 = execute (Mult (Num 2, Num 3)) = [NumVal 6]
let test5 = execute (Sub (Num 3, Num 2)) = [NumVal 1]
let test6 = execute (Div (Num 6, Num 2)) = [NumVal 3]
let test7 = execute (And (Bool true, Bool false)) = [BoolVal false]
let test8 = execute (Or (Bool true, Bool false)) = [BoolVal true]
let test9 = execute (Not (Bool true)) = [BoolVal false]
let test10 = execute (IfTE (Bool true, Num 1, Num 2)) = [NumVal 1]
let test11 = execute ( Abs ("x", Plus (V "x", Num 1)) ) = [Closure ("x", [LOOKUP "x"; NUM 1; ADD; RET], [])]
let test12 = execute1 [ App (Abs ("x", Plus (V "x", Num 1)), Num 2) ] = [NumVal 3]
let test13 = execute ( Let [Plus (Num 1, Num 2)] ) = [NumVal 3]
let test14 = execute2 ( Let [ Def ( "f", Abs ("x", Plus (V "x", Num 1)) ) ]) 
let test15 = execute ( Let [ App (V "f", Num 2) ] ) = [NumVal 3]
let test16 = execute ( Let [ Def ( "y", Num 2 ) ] ) = [NumVal 2]
let test17 = execute ( Let [App(Abs("x", Plus(Mult(V "x", Num 2), Plus(V "x", Num 1))), Num 24)]) = [NumVal 73]
let test18 = execute ( Def ( "f", Num 1) )  
let test19 = execute1 [ Def ( "f", Num 1) ; App (Abs ("x", Plus (V "x", Num 1)), V "f") ] 
let test20 = execute1 [ Def ( "f", Abs ("x", Plus (V "x", Num 1))); App (V "f", Num 2) ]
let test21 = execute1 [ Def ( "y", Num 5); Def ( "y", Sub (V "y", Num 1)); Def ( "f", Abs ("x", Plus (V "x", V "y"))); App (V "f", Num 2) ] 
let test22 = execute1 [ Def ( "f", Abs("x", Mult(Plus(V "x", Num 1), V "x"))); App (V "f", Num 2); Def("y", Num 10); Let[Def("y", Num 100); App(V "f", V "y")]; App(V "f", V "y") ]

let test23 = execute1 [ Tuple [Num 1; Num 2; Num 3] ] = [TupleVal [NumVal 1; NumVal 2; NumVal 3]]

let test24 = execute1 [ Projection (Tuple [Num 1; Num 2; Num 3], 1) ] 
let test25 = execute1 [ Case (Num 1, [(Num 1, Num 2); Num 2, Num 3]) ]
let test26 = execute1 [ Let [ Case (Num 1, [(Let[Def("f", Abs("x", Mult( V "x", Num 1))); App(V "f", Num 1)], Num 2); Num 2, Num 3]) ] ]

let test27 = execute1 [ Let [ Def ( "y", Num 6); Let [Def("y",Num 3); Def ( "f", Abs ("x", Mult (V "x", V "y")));  App (V "f", Num 2)] ] ]
let test28 = execute1 [ Let [ Def ( "y", Num 6); Let [ Def ( "f", Abs ("x", Mult (V "x", V "y")));  Def("y",Num 3);App (V "f", Num 2)] ] ]
let test29 = execute1 [ Let [ Def ( "x", Num 3); Def ( "y", Num 5); Mult (V "x", V "y") ] ] 

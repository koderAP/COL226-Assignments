type exp =
  | Var of string
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


type answer = INT of int | BOOL of bool | CLOS of string * exp * environment | TUPLE of answer list| Nil
and environment = (string * answer) list
and closure = Closur of exp * envClos | ClosTup of closure list | Null
and envClos = (exp * closure) list

type program = exp list
type programClos = closure list

type stack = answer list
type stackClos = closure list
type dump = (stack * environment * exp) list

let rec lookup x env =
  match env with
  | [] -> raise (Failure ("Variable not found: " ^ x))
  | (Var y, v) :: rest ->
      if x <> y then
        lookup x rest
      else
        match v with
        | Closur (Abs(x, e), env') -> Closur (Abs(x, e), (Var y, v) :: env')
        | _ -> v




let absCal (absC,stkC) = match (absC,stkC) with
  | (Closur (Abs (x, e), env), c1::c) -> (Closur (e, (Var x,c1)::env), c)
  | _ -> raise (Failure "absCal")

let rec krivine_machine c stk = match c with 
  | Closur ( Num n, env) -> Closur (Num n, env)
  | Closur ( Bool b, env) -> Closur (Bool b, env)
  | Closur ( Var x, env) -> krivine_machine (lookup x env) stk
  | Closur ( Abs (x, e), env) -> 
    let (a,b) = absCal (Closur (Abs (x, e), env), stk) in
    krivine_machine a b
  | Closur ( App (e1, e2), env) -> krivine_machine (Closur (e1, env)) (Closur (e2, env)::stk)
  | Closur ( Plus (e1, e2), env) -> 
    let a1 = krivine_machine (Closur (e1, env)) stk in
    let a2 = krivine_machine (Closur (e2, env)) stk in
    (match (a1, a2) with
      | (Closur (Num n1, _), Closur (Num n2, _)) -> Closur (Num (n1 + n2), env)
      | (ClosTup t1, ClosTup t2) -> ClosTup (List.map2 (fun x y -> match (x, y) with
        | (Closur (Num n1, _), Closur (Num n2, _)) -> Closur (Num (n1 + n2), env)
        | _ -> raise (Failure "Invalid input")) t1 t2)
      | _ -> raise (Failure "Invalid input"))
  | Closur ( Mult (e1, e2), env) ->
    let a1 = krivine_machine (Closur (e1, env)) stk in
    let a2 = krivine_machine (Closur (e2, env)) stk in
    (match (a1, a2) with
      | (Closur (Num n1, _), Closur (Num n2, _)) -> Closur (Num (n1 * n2), env)
      | (ClosTup t1, ClosTup t2) -> ClosTup (List.map2 (fun x y -> match (x, y) with
        | (Closur (Num n1, _), Closur (Num n2, _)) -> Closur (Num (n1 * n2), env)
        | _ -> raise (Failure "Invalid input")) t1 t2)
      | _ -> raise (Failure "Invalid input"))
  | Closur ( Sub (e1, e2), env) ->
    let a1 = krivine_machine (Closur (e1, env)) stk in
    let a2 = krivine_machine (Closur (e2, env)) stk in
    (match (a1, a2) with
      | (Closur (Num n1, _), Closur (Num n2, _)) -> Closur (Num (n1 - n2), env)
      | (ClosTup t1, ClosTup t2) -> ClosTup (List.map2 (fun x y -> match (x, y) with
        | (Closur (Num n1, _), Closur (Num n2, _)) -> Closur (Num (n1 - n2), env)
        | _ -> raise (Failure "Invalid input")) t1 t2)
      | _ -> raise (Failure "Invalid input"))
  | Closur ( Div (e1, e2), env) ->
    let Closur (Num n1, _) = krivine_machine (Closur (e1, env)) stk in
    let Closur (Num n2, _) = krivine_machine (Closur (e2, env)) stk in
    Closur (Num (n1 / n2), env)
  | Closur ( And (e1, e2), env) ->
    let a1 = krivine_machine (Closur (e1, env)) stk in
    let a2 = krivine_machine (Closur (e2, env)) stk in
    (match (a1, a2) with
      | (Closur (Bool b1, _), Closur (Bool b2, _)) -> Closur (Bool (b1 && b2), env)
      | (ClosTup t1, ClosTup t2) -> ClosTup (List.map2 (fun x y -> match (x, y) with
        | (Closur (Bool b1, _), Closur (Bool b2, _)) -> Closur (Bool (b1 && b2), env)
        | _ -> raise (Failure "Invalid input")) t1 t2)
      | _ -> raise (Failure "Invalid input"))
  | Closur ( Or (e1, e2), env) ->
    let a1 = krivine_machine (Closur (e1, env)) stk in
    let a2 = krivine_machine (Closur (e2, env)) stk in
    (match (a1, a2) with
      | (Closur (Bool b1, _), Closur (Bool b2, _)) -> Closur (Bool (b1 || b2), env)
      | (ClosTup t1, ClosTup t2) -> ClosTup (List.map2 (fun x y -> match (x, y) with
        | (Closur (Bool b1, _), Closur (Bool b2, _)) -> Closur (Bool (b1 || b2), env)
        | _ -> raise (Failure "Invalid input")) t1 t2)
      | _ -> raise (Failure "Invalid input"))
  | Closur ( Eq (e1, e2), env) ->
    let a1 = krivine_machine (Closur (e1, env)) stk in
    let a2 = krivine_machine (Closur (e2, env)) stk in
    (match (a1, a2) with
      | (Closur (Num n1, _), Closur (Num n2, _)) -> Closur (Bool (n1 = n2), env)
      | (Closur (Bool b1, _), Closur (Bool b2, _)) -> Closur (Bool (b1 = b2), env)
      | (ClosTup t1, ClosTup t2) -> ClosTup (List.map2 (fun x y -> match (x, y) with
        | (Closur (Num n1, _), Closur (Num n2, _)) -> Closur (Bool (n1 = n2), env)
        | (Closur (Bool b1, _), Closur (Bool b2, _)) -> Closur (Bool (b1 = b2), env)
        | _ -> raise (Failure "Invalid input")) t1 t2)
      | _ -> raise (Failure "Invalid input"))
  | Closur ( Gt (e1, e2), env) ->
    let Closur (Num n1, _) = krivine_machine (Closur (e1, env)) stk in
    let Closur (Num n2, _) = krivine_machine (Closur (e2, env)) stk in
    Closur (Bool (n1 > n2), env)
  | Closur ( Not e, env) ->
    let Closur (Bool b, _) = krivine_machine (Closur (e, env)) stk in
    Closur (Bool (not b), env)
  | Closur ( IfTE (e1, e2, e3), env) ->
    let Closur (Bool b, _) = krivine_machine (Closur (e1, env)) stk in
    if b then
      krivine_machine (Closur (e2, env)) stk
    else
      krivine_machine (Closur (e3, env)) stk
  | Closur ( Def (x, e), env) -> krivine_machine (Closur (e, (Var x, Closur (Def (x, e), env))::env)) stk
  | Closur ( Tuple es, env) -> 
    let rec tupleCal es (tu:closure list) = match es with
      | [] -> tu
      | e::rest -> tupleCal rest [krivine_machine (Closur (e, env)) stk]@tu in
    ClosTup (List.rev (tupleCal es []))
  | Closur ( Case (e, cs), env) -> 
    let hehe = krivine_machine (Closur (e, env)) stk in
    let rec caseCal cs = match cs with
    | [] -> Null
    | (e1,e2) :: rest -> if hehe = krivine_machine (Closur (e1, env)) stk then krivine_machine (Closur (e2, env)) stk else caseCal rest in
    caseCal cs
  | Closur ( Projection (e, n), env) ->
    let ClosTup cs = krivine_machine (Closur (e, env)) stk in
    List.nth cs n
  | Closur ( Let es, env) -> 
    let rec letCal (es, env) = match es with
      | [] -> []
      | e::rest -> (Closur (e, env))::(letCal (rest, env)) in
    let stk' = letCal (es, env) in
    krivine_machine (List.hd stk') (List.tl stk' @ stk)
  | _ -> raise (Failure "Invalid input")




let rec compile (p : exp list) : programClos = 
   let rec compile' (p : exp list) (env : envClos) (proCls : programClos) : programClos = match p with
    | [] -> proCls
    | Var x::rest -> compile' rest env (Closur (Var x, env)::proCls)
    | Abs (x, e)::rest -> compile' rest env (Closur (Abs (x, e), env)::proCls)
    | App (e1, e2)::rest -> compile' rest env (Closur (App (e1, e2), env)::proCls)
    | Num n::rest -> compile' rest env (Closur (Num n, env)::proCls)
    | Bool b::rest -> compile' rest env (Closur (Bool b, env)::proCls)
    | Plus (e1, e2)::rest -> compile' rest env (Closur (Plus (e1, e2), env)::proCls)
    | Mult (e1, e2)::rest -> compile' rest env (Closur (Mult (e1, e2), env)::proCls)
    | Sub (e1, e2)::rest -> compile' rest env (Closur (Sub (e1, e2), env)::proCls)
    | Div (e1, e2)::rest -> compile' rest env (Closur (Div (e1, e2), env)::proCls)
    | And (e1, e2)::rest -> compile' rest env (Closur (And (e1, e2), env)::proCls)
    | Or (e1, e2)::rest -> compile' rest env (Closur (Or (e1, e2), env)::proCls)
    | Eq (e1, e2)::rest -> compile' rest env (Closur (Eq (e1, e2), env)::proCls)
    | Gt (e1, e2)::rest -> compile' rest env (Closur (Gt (e1, e2), env)::proCls)
    | Not e::rest -> compile' rest env (Closur (Not e, env)::proCls)
    | IfTE (e1, e2, e3)::rest -> compile' rest env (Closur (IfTE (e1, e2, e3), env)::proCls)
    | Def (x, e)::rest -> compile' rest ((Var x, Closur (Def (x, e), env))::env) (Closur (Def (x, e), env)::proCls)
    | Let es::rest -> compile' rest env (Closur (Let es, env)::proCls)
    | Tuple es::rest -> compile' rest env (Closur (Tuple es, env)::proCls)
    | Projection (e, n)::rest -> compile' rest env (Closur (Projection (e, n), env)::proCls)
    | Case (e, cs)::rest -> compile' rest env (Closur (Case (e, cs), env)::proCls)
    | _ -> raise (Failure "Invalid input") in
  compile' p [] []


let rec clos_to_answer (c : closure) : answer = match c with
  | Closur (Num n, _) -> INT n
  | Closur (Bool b, _) -> BOOL b
  | ClosTup cs -> TUPLE (List.map clos_to_answer cs)
  | Null -> Nil
  | _ -> raise (Failure "Invalid input")

let rec run (p : program) : answer =
  let proCls = compile p in
  let ans = krivine_machine (List.hd proCls) [] in 
  clos_to_answer ans


let test1 = run [App (Abs ("x", Var "x"), Num 3)] 
let test2 = run [App (Abs ("x", App (Abs ("y", Var "y"), Var "x")), Num 3)]
let test3 = run [App (Abs ("x", Plus (Plus (Mult (Var "x", Var "x"), Mult (Num 2, Var "x")), Num 1)), Num 3)]
let test4 = run [App (Abs ("x", Plus (Var "x", Num 1)), Num 2)]
let test5 = run [App (Abs ("x", Mult (Var "x", Num 2)), Num 3)]
let test6 = run [App (Abs ("x", Sub (Var "x", Num 1)), Num 3)]
let test7 = run [App (Abs ("x", Div (Var "x", Num 2)), Num 6)]
let test8 = run [App (Abs ("x", And (Var "x", Bool true)), Bool false)]
let test9 = run [App (Abs ("x", Or (Var "x", Bool false)), Bool true)]
let test10 = run [App (Abs ("x", Eq (Var "x", Num 1)), Num 1)]
let test11 = run [App (Abs ("x", Gt (Var "x", Num 1)), Num 2)]
let test12 = run [App (Abs ("x", Not (Var "x")), Bool true)]
let test13 = run [App (Abs ("x", IfTE (Var "x", Num 1, Num 2)), Bool true)]
let test14 = run [ 
  Def ( "f", Abs("x", Mult(Plus(Var "x", Num 1), Var "x"))); 
    App (Var "f", Num 2);
      Def("y", Num 10); 
      Let[Def("y", Num 100);
        App(Var "f", Var "y")]; 
      App(Var "f", Var "y") ]

let test15 = run [
  Def ( "fact", Abs("x", 
    IfTE(Eq(Var "x", Num 0), Num 1, 
      Mult(Var "x", App(Var "fact", Sub(Var "x", Num 1))))));
  App(Var "fact", Num 5)
]
let test16 = run [ Tuple [Num 1; Num 2; Num 3] ]
let test17 = run [ Tuple [Plus(Num 1, Num 2); Mult(Num 2, Num 3); Sub(Num 3, Num 1)] ]
let test18 = run [ Projection(Tuple [Num 1; Num 2; Num 3], 1) ]
let test19 = run [ Projection(Tuple [Plus(Num 1, Num 2); Mult(Num 2, Num 3); Sub(Num 3, Num 1)], 2) ]
let test20 = run [ Case(Num 1, [(Num 1, Num 2); (Num 2, Num 3); (Num 3, Num 4)]) ]
let test21 = run [
  Def("y", Num 5);
  Def ( "fact", Abs("x", 
    IfTE(Eq(Var "x", Num 0), Num 1, 
      Mult(Var "x", App(Var "fact", Sub(Var "x", Num 1))))));
  Case(Var "y", [(Num 1, App(Var "fact", Num 2)); (Num 2, App(Var "fact", Num 3))]);
  App(Var "fact", Var "y")
]

let test22 = run [
  App (
    Abs ("z", Num 2), App (
      Abs ( "a", App (Var "a", Var "a")), Abs ("a", App (Var "a", Var "a"))
    ))]
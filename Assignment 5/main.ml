type symbol = string * int;;
type signature = symbol list;;
type tree = V of string | C of { node: symbol ; children: tree list };;

let check_sig (s: signature) : bool = 
  let rec check_sig' s = match s with
    | [] -> true
    | (x, y)::xs -> if y < 0 || List.exists (fun (y, _) -> x = y) xs then false else check_sig' xs
  in
  check_sig' s;;

let rec wftree (t: tree) (s: signature) : bool =
  let rec wftree' t s = match t with
    | V x -> List.mem (x, 0) s
    | C {node = x; children = xs} -> 
        let rec wftree'' xs s = match xs with
          | [] -> true
          | x::xs -> if wftree' x s then wftree'' xs s else false
        in
        if List.mem x s then wftree'' xs s else false
  in
  wftree' t s;;


let rec ht (t: tree) : int =
  let rec ht' t h = match t with
    | V x -> h
    | C {node = x; children = xs} -> 
        let rec ht'' xs h = match xs with
          | [] -> h
          | x::xs' -> ht'' xs' (max h (ht' x (h + 1)))
        in
        ht'' xs h
  in
  ht' t 0;;


let rec size (t: tree) : int =
  let rec size' t s = match t with
    | V x -> s + 1
    | C {node = x; children = xs} -> 
        let rec size'' xs s = match xs with
          | [] -> s + 1
          | x::xs' -> size'' xs' (size' x s)
        in
        size'' xs s
  in
  size' t 0;;

let rec vars (t: tree) : string list =
  let rec vars' t acc = match t with
    | V x -> x :: acc
    | C {node = _; children = xs} -> List.fold_left (fun acc' t' -> vars' t' acc') acc xs
  in
  List.sort_uniq compare (vars' t []);;

  
let rec mirror (t: tree) : tree =
  match t with
  | V x -> V x
  | C {node = n; children = xs} ->
      let reversed_children = List.map mirror (List.rev xs) in
      C {node = n; children = reversed_children};;
  
type substitution = (string * tree) list;;
  
let check_subst (s: substitution) : bool =
  let vars_in_subst = List.map fst s in
  List.length vars_in_subst = List.length (List.sort_uniq compare vars_in_subst);;

let rec subst (t: tree) (s: substitution) : tree =
  match t with
  | V x ->
      (try List.assoc x s with Not_found -> t)
  | C {node = n; children = xs} ->
      let substituted_children = List.map (fun t' -> subst t' s) xs in
      C {node = n; children = substituted_children};;
  
let compose_subst (s1: substitution) (s2: substitution) : substitution =
  let apply_subst t s = 
    List.fold_left (fun acc (v, subst_t) -> subst acc [(v, subst_t)]) t s 
  in
  List.map (fun (v, t) -> (v, apply_subst t s2)) s1;; 
exception NOT_UNIFIABLE;;
  
let rec mgu (t1: tree) (t2: tree) : substitution =
  let rec mgu' t1 t2 s =
    match t1, t2 with
    | V x1, V x2 ->
        if x1 = x2 then s else [(x1, subst t2 s)] @ s
    | C {node = n1; children = xs1}, C {node = n2; children = xs2} ->
        if n1 = n2 && List.length xs1 = List.length xs2 then
          mgu_list xs1 xs2 s
        else
          raise NOT_UNIFIABLE
    | V x, t | t, V x ->
        if List.mem x (vars t) then
          raise NOT_UNIFIABLE
        else
          [(x, t)] @ s
  and mgu_list xs1 xs2 s =
    match xs1, xs2 with
    | [], [] -> s
    | t1 :: rest1, t2 :: rest2 ->
        let s' = mgu' (subst t1 s) (subst t2 s) s in
        mgu_list rest1 rest2 s'
    | _ -> raise NOT_UNIFIABLE 
  in
  mgu' t1 t2 [];;
  
  
let t1 = V "x";;
let t2 = V "x";;
let res12 = mgu t1 t2;;
  
let t3 = V "x";;
let t4 = V "y";;
let res34 = mgu t3 t4;;
  
let t5 = C {node = ("f", 0); children = []};;
let t6 = V "x";;
let res56 = mgu t5 t6;;
  
let t7 = C {node = ("f", 2); children = [V "x"; V "y" ]};;
let t8 = C {node = ("f", 2); children = [V "y"; V "x" ]};;
let res78 = mgu t7 t8;;
  
let t9 = C {node = ("f", 0); children = []};;
let t10 = C {node = ("g", 0); children = []};;
let res910 = mgu t9 t10

  
let t11 = C {node = ("f", 1); children = [V "x"]};;
let t12 = C {node = ("f", 2); children = [V "x"; V "y"]};;
let res1112 = mgu t11 t12
  
let t13 = C {node = ("f", 1); children = [C {node = ("g", 0); children = []} ]};;
let t14 = C {node = ("f", 1); children = [C {node = ("g", 0); children = []} ]};;
let res1314 = mgu t13 t14;;

  
let t15 = C {node = ("f", 2); children = [V "x"; C {node = ("g", 1); children = [V "y"]} ]};;
let t16 = C {node = ("f", 2); children = [C {node = ("h", 0); children = []}; C {node = ("g", 1); children = [C {node = ("h", 0); children = []}]} ]};;
let res1516 = mgu t15 t16;;

type symbol = string * int;;
type signature = symbol list;;
type tree = V of string | C of { node: symbol ; children: tree list };;

let check_sig (s: signature) : bool = 
  let rec check_sig' s = match s with
    | [] -> true
    | (x, y)::xs -> if y < 0 || List.exists (fun (y, _) -> x = y) xs then false else check_sig' xs
  in
  check_sig' s;;

let rec wftree (t: tree) (s: signature) : bool =
  let rec wftree' t s = match t with
    | V x -> List.mem (x, 0) s
    | C {node = x; children = xs} -> 
        let rec wftree'' xs s = match xs with
          | [] -> true
          | x::xs -> if wftree' x s then wftree'' xs s else false
        in
        if List.mem x s then wftree'' xs s else false
  in
  wftree' t s;;


let rec ht (t: tree) : int =
  let rec ht' t h = match t with
    | V x -> h
    | C {node = x; children = xs} -> 
        let rec ht'' xs h = match xs with
          | [] -> h
          | x::xs' -> ht'' xs' (max h (ht' x (h + 1)))
        in
        ht'' xs h
  in
  ht' t 0;;


let rec size (t: tree) : int =
  let rec size' t s = match t with
    | V x -> s + 1
    | C {node = x; children = xs} -> 
        let rec size'' xs s = match xs with
          | [] -> s + 1
          | x::xs' -> size'' xs' (size' x s)
        in
        size'' xs s
  in
  size' t 0;;

let rec vars (t: tree) : string list =
  let rec vars' t acc = match t with
    | V x -> x :: acc
    | C {node = _; children = xs} -> List.fold_left (fun acc' t' -> vars' t' acc') acc xs
  in
  List.sort_uniq compare (vars' t []);;

  
let rec mirror (t: tree) : tree =
  match t with
  | V x -> V x
  | C {node = n; children = xs} ->
      let reversed_children = List.map mirror (List.rev xs) in
      C {node = n; children = reversed_children};;
  
type substitution = (string * tree) list;;
  
let check_subst (s: substitution) : bool =
  let vars_in_subst = List.map fst s in
  List.length vars_in_subst = List.length (List.sort_uniq compare vars_in_subst);;

let rec subst (t: tree) (s: substitution) : tree =
  match t with
  | V x ->
      (try List.assoc x s with Not_found -> t)
  | C {node = n; children = xs} ->
      let substituted_children = List.map (fun t' -> subst t' s) xs in
      C {node = n; children = substituted_children};;
  
let compose_subst (s1: substitution) (s2: substitution) : substitution =
  let apply_subst t s = 
    List.fold_left (fun acc (v, subst_t) -> subst acc [(v, subst_t)]) t s 
  in
  List.map (fun (v, t) -> (v, apply_subst t s2)) s1;; 
exception NOT_UNIFIABLE;;
  
let rec mgu (t1: tree) (t2: tree) : substitution =
  let rec mgu' t1 t2 s =
    match t1, t2 with
    | V x1, V x2 ->
        if x1 = x2 then s else [(x1, subst t2 s)] @ s
    | C {node = n1; children = xs1}, C {node = n2; children = xs2} ->
        if n1 = n2 && List.length xs1 = List.length xs2 then
          mgu_list xs1 xs2 s
        else
          raise NOT_UNIFIABLE
    | V x, t | t, V x ->
        if List.mem x (vars t) then
          raise NOT_UNIFIABLE
        else
          [(x, t)] @ s
  and mgu_list xs1 xs2 s = match xs1, xs2 with
    | [], [] -> s
    | x1::xs1', x2::xs2' ->
        let s' = mgu' x1 x2 s in
        let s'' = compose_subst s' s in
        mgu_list (List.map (fun t -> subst t s'') xs1') (List.map (fun t -> subst t s'') xs2') s''
    | _ -> raise NOT_UNIFIABLE
  in
  mgu' t1 t2 [];;
  
  
let t1 = V "x";;
let t2 = V "x";;
let res12 = mgu t1 t2;;
  
let t3 = V "x";;
let t4 = V "y";;
let res34 = mgu t3 t4;;
  
let t5 = C {node = ("f", 0); children = []};;
let t6 = V "x";;
let res56 = mgu t5 t6;;
  
let t7 = C {node = ("f", 2); children = [V "x"; V "y" ]};;
let t8 = C {node = ("f", 2); children = [V "y"; V "x" ]};;
let res78 = mgu t7 t8;;
  
let t9 = C {node = ("f", 0); children = []};;
let t10 = C {node = ("g", 0); children = []};;
let res910 = mgu t9 t10

  
let t11 = C {node = ("f", 1); children = [V "x"]};;
let t12 = C {node = ("f", 2); children = [V "x"; V "y"]};;
let res1112 = mgu t11 t12
  
let t13 = C {node = ("f", 1); children = [C {node = ("g", 0); children = []} ]};;
let t14 = C {node = ("f", 1); children = [C {node = ("g", 0); children = []} ]};;
let res1314 = mgu t13 t14;;

  
let t15 = C {node = ("f", 2); children = [V "x"; C {node = ("g", 1); children = [V "y"]} ]};;
let t16 = C {node = ("f", 2); children = [C {node = ("h", 0); children = []}; C {node = ("g", 1); children = [C {node = ("h", 0); children = []}]} ]};;
let res1516 = mgu t15 t16;;

let test17 = mgu (C {node = ("+",3);children = [V "x"; V "y"; V "z"]}) (C {node = ("+",3);children = [V "y"; V "x"; V "y"]}) ;;
let test18 = mgu (C {node = ("+",3);children = [V "x"; V "y"; V "z"]}) (C {node = ("+",3);children = [V "y"; V "z"; V "x"]}) ;;
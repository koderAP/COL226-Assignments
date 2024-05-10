type variable = string
type symbol = string
type signature = (symbol * int) list

type term = 
  | Var of variable 
  | Num of int 
  | Node of symbol * (term list)

type atom = ATOM of symbol * (term list)

type clause = Fact of atom | Rule of atom * (atom list)

type program = clause list

type goal = GOAL of atom list

type substitution = (variable * term) list

exception NOT_UNIFIABLE
exception NotFound
exception InvalidProgram
exception NotPossible
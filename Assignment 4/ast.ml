(* ast.ml *)

type variable = string
type constant = string
type functor_symbol = string
type predicate_symbol = string

type term =
  | Variable of variable
  | Constant of constant
  | Function of functor_symbol * term list
  | List of term list
  | Tuple of term list

type atomic_formula = predicate_symbol * term list

type clause =
  | Fact of atomic_formula
  | Rule of atomic_formula * atomic_formula list
  | Goal of atomic_formula list


type program = clause list

type exp = 
           IntConst of int
         | Abs of exp
         | Identifier of string
         | Add of exp * exp
         | Sub of exp * exp
         | Mul of exp * exp
         | Div of exp * exp
         | Exp of exp * exp
         | BoolConst of bool
         | Not of exp
         | And of exp * exp
         | Or of exp * exp
         | Implies of exp * exp
         | Eql of exp * exp
         | Grt of exp * exp
         | Lst of exp * exp
         | Gre of exp * exp
         | Lse of exp * exp
         | Tup of int * exp list
         | Proj of int * exp
;;

type answer = IntConst of int | BoolConst of int | Tup of int * exp list ;;


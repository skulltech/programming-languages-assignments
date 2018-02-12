type exp = IntConst of int
         | Abs of exp
         | Idn of string
         | Add of exp * exp
         | Sub of exp * exp
         | Mul of exp * exp
         | Div of exp * exp
         | Exp of exp * exp
         | BoolConst of bool
         | Not of exp
         | And of exp * exp
         | Or of exp * exp
         | Imp of exp * exp
         | Eql of exp * exp
         | Grt of exp * exp
         | Lst of exp * exp
         | Gre of exp * exp
         | Lse of exp * exp
         | Tup of int * exp list
         | Proj of int * exp
;;
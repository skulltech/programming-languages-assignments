(* 
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
*)

type exp = 
          IntExp of intexp
        | BoolExp of boolexp 
        | Tuple of int * exp list
        | Identifier of string
        | Proj of int * exp
        | Eql of exp * exp
;;

type intexp = 
           IntConst of int
         | Abs of intexp
         | Add of intexp * intexp
         | Sub of intexp * intexp
         | Mul of intexp * intexp
         | Div of intexp * intexp
         | Exp of intexp * intexp
         | Grt of intexp * intexp
         | Lst of intexp * intexp
         | Gre of intexp * intexp
         | Lse of intexp * intexp
;;

type boolexp = 
         | BoolConst of bool
         | Not of boolexp
         | And of boolexp * boolexp
         | Or of boolexp * boolexp
         | Implies of boolexp * boolexp
;;

type answer = 
          IntConst of int
        | BoolConst of bool
        | Tuple of int * answer list
;;

let rec eval rho exp = match exp with
                          IntConst n  -> n
                        | Abs e -> abs (eval rho e)
                        | Identifier s -> eval rho (rho s)
                        | Add (e1, e2) -> (eval rho e1) + (eval rho e2)
                        | Sub (e1, e2) -> (eval rho e1) + (eval rho e2)
                        | Mul (e1, e2) -> (eval rho e1) * (eval rho e2)
                        | Div (e1, e2) -> (eval rho e1) / (eval rho e2)
                        | Exp (e1, e2) -> int_of_float (float_of_int (eval rho e1) ** float_of_int (eval rho e2))
                        | BoolConst b -> b
                        | Not b -> not b
                        | And (e1, e2) -> (eval rho e1) && (eval rho e2)
                        | Or (e1, e2) -> (eval rho e1) && (eval rho e2)
                        | Implies (e1, e2) -> (not (eval rho e1)) || (eval rho e2)
                        | Eql (e1, e2) -> (eval rho e1) = (eval rho r2)
                        | Grt (e1, e2) -> (eval rho e1) > (eval rho e2)
                        | Gre (e1, e2) -> (eval rho e1) >= (eval rho e2)
                        | Lst (e1, e2) -> (eval rho e1) < (eval rho e2)
                        | Lse (e1, e2) -> (eval rho e1) <= (eval rho e2)
                        | Tup (n, l) -> l
                        | Proj (n, e) -> List.nth l n
;;

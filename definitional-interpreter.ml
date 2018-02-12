type exp = 
         | IntConst of int
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
         | Ls of exp * exp
         | GrtEql of exp * exp
         | LsEql of exp * exp
         | Tuple of int * exp list
         | Proj of int * exp
;;

type answer = 
        | IntAns of int
        | BoolAns of bool
        | TupleAns of int * answer list
;;

let rec map f l = match l with
                    | [] -> []
                    | x::xs -> (f x)::(map f xs)
;;

let intof e = match e with IntAns i -> i ;;
let boolof b = match b with BoolAns b -> b ;;
let rec listof l = match b with
                    | [] -> []
                    | (IntAns x)::xs -> x::(listof xs)
;;


let rec eval rho exp = match exp with
                        | IntConst n  -> IntAns n
                        | Abs e -> IntAns (abs (intof (eval rho e)))
                        | Identifier s -> (rho s)
                        | Add (e1, e2) -> IntAns ((intof (eval rho e1)) + (intof (eval rho e2)))
                        | Sub (e1, e2) -> IntAns ((intof (eval rho e1)) - (intof (eval rho e2)))
                        | Mul (e1, e2) -> IntAns ((intof (eval rho e1)) * (intof (eval rho e2)))
                        | Div (e1, e2) -> IntAns ((intof (eval rho e1)) / (intof (eval rho e2)))
                        | Exp (e1, e2) -> IntAns (int_of_float ((float_of_int (intof (eval rho e1))) ** float_of_int (intof (eval rho e2))))
                        | BoolConst b -> BoolAns b
                        | Not b -> BoolAns (not (boolof (eval rho b)))
                        | And (e1, e2) -> BoolAns ((boolof (eval rho e1)) && (boolof (eval rho e2)))
                        | Or (e1, e2) -> BoolAns ((boolof (eval rho e1)) || (boolof (eval rho e2)))
                        | Implies (e1, e2) -> BoolAns ((not (boolof (eval rho e1))) || (boolof (eval rho e2)))
                        | Eql (e1, e2) -> BoolAns ((eval rho e1) = (eval rho e2))
                        | Grt (e1, e2) -> BoolAns ((intof (eval rho e1)) > (intof (eval rho e2)))
                        | Ls (e1, e2) -> BoolAns ((intof (eval rho e1)) < (intof (eval rho e2)))
                        | GrtEql (e1, e2) -> BoolAns ((intof (eval rho e1)) >= (intof (eval rho e2)))
                        | LsEql (e1, e2) -> BoolAns ((intof (eval rho e1)) <= (intof (eval rho e2)))
                        | Tuple (n, l) -> TupleAns (n, map (eval rho) l)
                        | Proj (i, Tuple (n, l)) -> eval rho (List.nth l n)
;;

type opcode = INTCONST of int | ABS | ADD | SUB | MUL | DIV | EXP 
            | BOOLCONST of bool | NOT | AND | OR | IMPLIES | GRT | LS | GRTEQL | LSEQL 
            | EQL | TUPLE | PROJ | IDENTIFIER of string | LOOKUP
;;

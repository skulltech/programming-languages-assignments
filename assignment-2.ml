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
         | Tuple of int * (exp list)
         | Proj of int * exp
;;

type answer = 
        | IntAns of int
        | BoolAns of bool
        | TupleAns of int * (answer list)
;;

let rec map f l = match l with
                    | [] -> []
                    | x::xs -> (f x)::(map f xs)
;;

let intof e = match e with IntAns i -> i ;;
let boolof b = match b with BoolAns b -> b ;;
let intexp a b = int_of_float ((float_of_int a) ** (float_of_int b)) ;;

let rec eval rho exp = match exp with
                        | IntConst n  -> IntAns n
                        | Abs e -> IntAns (abs (intof (eval rho e)))
                        | Identifier s -> (rho s)
                        | Add (e1, e2) -> IntAns ((intof (eval rho e1)) + (intof (eval rho e2)))
                        | Sub (e1, e2) -> IntAns ((intof (eval rho e1)) - (intof (eval rho e2)))
                        | Mul (e1, e2) -> IntAns ((intof (eval rho e1)) * (intof (eval rho e2)))
                        | Div (e1, e2) -> IntAns ((intof (eval rho e1)) / (intof (eval rho e2)))
                        | Exp (e1, e2) -> IntAns (intexp (intof (eval rho e1)) (intof (eval rho e2)))
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
                        | Proj (i, Tuple (n, l)) -> eval rho (List.nth l i)
;;

type opcode = INTCONST of int | ABS | ADD | SUB | MUL | DIV | EXP 
            | BOOLCONST of bool | NOT | AND | OR | IMPLIES | GRT | LS | GRTEQL | LSEQL 
            | EQL | TUPLE | PROJ | LOOKUP of string | SEP
;;

let rec join l sep = match l with
                    | [] -> []
                    | x::xs -> x @ [sep] @ (join xs sep)
;; 

let rec compile exp = match exp with
                        | IntConst n  -> [INTCONST n]
                        | Abs e -> (compile e) @ [ABS]
                        | Identifier s -> [LOOKUP s]
                        | Add (e1, e2) -> (compile e1) @ (compile e2) @ [ADD]
                        | Sub (e1, e2) -> (compile e1) @ (compile e2) @ [SUB]
                        | Mul (e1, e2) -> (compile e1) @ (compile e2) @ [MUL]
                        | Div (e1, e2) -> (compile e1) @ (compile e2) @ [DIV]
                        | Exp (e1, e2) -> (compile e1) @ (compile e2) @ [EXP]
                        | BoolConst b -> [BOOLCONST b]
                        | Not b -> (compile b) @ [NOT]
                        | And (e1, e2) -> (compile e1) @ (compile e2) @ [AND]
                        | Or (e1, e2) -> (compile e1) @ (compile e2) @ [OR]
                        | Implies (e1, e2) -> (compile e1) @ (compile e2) @ [IMPLIES]
                        | Eql (e1, e2) -> (compile e1) @ (compile e2) @ [EQL]
                        | Grt (e1, e2) -> (compile e1) @ (compile e2) @ [GRT]
                        | Ls (e1, e2) -> (compile e1) @ (compile e2) @ [LS]
                        | GrtEql (e1, e2) -> (compile e1) @ (compile e2) @ [GRTEQL]
                        | LsEql (e1, e2) -> (compile e1) @ (compile e2) @ [LSEQL]
                        | Tuple (n, l) -> (join (map compile l) SEP) @ [INTCONST n] @ [TUPLE]
                        | Proj (i, t) -> (compile t) @ [(INTCONST i); PROJ]
;;

let rec split k xs = match xs with
                        | [] -> ([], []) 
                        | x::xs -> if k=1 then ([x], xs) else (match split (k-1) xs with (a, b) -> (x::a, b))
;;

let rec execute (s, t, c) = match (s, c) with
                        | (s, []) -> List.hd s
                        | (s, (INTCONST n)::c') -> execute((IntAns n)::s, t, c')
                        | (n::s', ABS::c') -> execute((IntAns (abs (intof n)))::s', t, c')
                        | (n1::n2::s', ADD::c') -> execute((IntAns ((intof n1) + (intof n2)))::s', t, c')
                        | (n1::n2::s', SUB::c') -> execute((IntAns ((intof n1) - (intof n2)))::s', t, c')
                        | (n1::n2::s', MUL::c') -> execute((IntAns ((intof n1) * (intof n2)))::s', t, c')
                        | (n1::n2::s', DIV::c') -> execute((IntAns ((intof n2) / (intof n1)))::s', t, c')
                        | (n1::n2::s', EXP::c') -> execute((IntAns (intexp (intof n2) (intof n1)))::s', t, c')
                        | (s, (BOOLCONST b)::c') -> execute((BoolAns b)::s, t, c')
                        | (b::s', NOT::c') -> execute((BoolAns (not (boolof b)))::s', t, c')
                        | (b1::b2::s', AND::c') -> execute((BoolAns ((boolof b1) && (boolof b2)))::s', t, c')
                        | (b1::b2::s', OR::c') -> execute((BoolAns ((boolof b1) || (boolof b2)))::s', t, c')
                        | (b1::b2::s', IMPLIES::c') -> execute((BoolAns ((not (boolof b2)) || (boolof b1)))::s', t, c')
                        | (n1::n2::s', GRT::c') -> execute((BoolAns ((intof n2) > (intof n1)))::s', t, c')
                        | (n1::n2::s', LS::c') -> execute((BoolAns ((intof n2) < (intof n1)))::s', t, c')
                        | (n1::n2::s', GRTEQL::c') -> execute((BoolAns ((intof n2) >= (intof n1)))::s', t, c')
                        | (n1::n2::s', LSEQL::c') -> execute((BoolAns ((intof n2) <= (intof n1)))::s', t, c')
                        | (e1::e2::s', EQL::c') -> execute((BoolAns (e1 = e2))::s', t, c')
                        | (s, (LOOKUP s1)::c') -> execute((t s1)::s, t, c')
                        | (s, SEP::c') -> execute(s, t, c')
                        | ((IntAns n)::s, TUPLE::c') -> execute((match (split n s) with (a, b) -> TupleAns(n, (List.rev a))::b), t, c')
                        | (i::e::s', PROJ::c') -> execute((match e with TupleAns(n, l) -> (List.nth l (intof i)))::s', t, c')
;;


(* some test cases *)

let rho x = match x with
               | "x" -> IntAns 5
               | "y" -> IntAns 8
;;

let e1 = And(Proj(1, Tuple(2 , [Add(IntConst 5, IntConst 10); GrtEql((Abs (IntConst (-2))), Identifier "x")])), BoolConst false);;
eval rho e1;;
execute ([], rho, (compile e1));;

type symbol = Symbol of (string * int);;
type variable = Var of string;;
type term = V of variable | Node of symbol * (term list);;


let check_sig sign = List.fold_left (fun x y -> x && y) true (List.map (fun x -> match x with Symbol (s, n) -> n >= 0) sign) &&
                     List.fold_left (fun x y -> x && y) true (let seen = Hashtbl.create (List.length sign) in
                                List.map (fun x -> match x with Symbol (s, n) -> let tmp = not (Hashtbl.mem seen s) in Hashtbl.replace seen s (); tmp) sign)
;;

let rec greatest l = match l with
                        | [] -> 0
                        | x::xs -> let tmp = greatest xs in if x > tmp then x else tmp
;;

let rec ht term = match term with
                    | V var -> 0
                    | Node (s, []) -> 0
                    | Node (s, l) -> 1 + greatest (List.map ht l)
;;

let rec size term = match term with
                    | V var -> 1
                    | Node (s, l) -> 1 + (List.fold_left (fun x y -> x + y) 0 (List.map size l))
;;

let uniq lst =
  let seen = Hashtbl.create (List.length lst) in
  List.filter (fun x -> let tmp = not (Hashtbl.mem seen x) in
                        Hashtbl.replace seen x ();
                        tmp) lst
;;

let rec vars term = uniq (match term with
                            | V var -> [var]
                            | Node (s, []) -> []
                            | Node (s, l) -> List.concat (List.map vars l))
;;


let t = Node (Symbol ("c", 0), []);;
let u = Node (Symbol ("b", 1), [t]);;
let v = Node (Symbol ("a", 3), [u; u; t]);;
let w = Node (Symbol ("a", 3), [v; u; V(Var "x")]);;
let x = Node (Symbol ("d", 4), [V(Var "y"); w; u; V(Var "x")]);;


let rec subst s t = match t with
                    | V var -> if Hashtbl.mem s var then Hashtbl.find s var else V var
                    | Node (sm, []) -> Node (sm, [])
                    | Node (sm, l) -> Node (sm, (List.map (subst s) l))
;;


let compose s1 s2 = let composed = Hashtbl.create (Hashtbl.length s1) in
                    Hashtbl.iter (fun k d -> Hashtbl.add composed k (subst s2 d)) s1;
                    composed
;;

let htbl = Hashtbl.create 1;;
Hashtbl.add htbl (Var "x") v;;
subst htbl w;;

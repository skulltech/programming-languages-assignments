type 't estr = T of 't array * (int ref);;

let lgh x = match x with T (a, b) -> Array.length a;;
let nonempty x = match x with T(a, b) -> if a = [||] then false else true;;
let concat x y = match x, y with T(a, b), T(c, d) -> T((Array.append a c), ref 0);;
let reverse x = match x with T(a, b) -> T((Array.of_list (List.rev (Array.to_list a))), b);;

exception Empty;;
let first x = match x with T(a, b) -> if nonempty x then Array.get a 0 else raise Empty;;
let last x = match x with T(a, b) -> if nonempty x then Array.get a ((Array.length a) -1) else raise Empty;;

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []
;;

let create a = T((Array.of_list (explode a)), ref 0);;

exception AtLast;;
let forward x = match x with T(a, b) -> if !b < ((lgh x) - 1) then b:=!b+1 else raise AtLast; x ;;

exception AtFirst;;
let back x = match x with T(a, b) -> if !b > 0 then b:=!b-1 else raise AtFirst; x;;

exception TooShort;;
let moveTo x n = match x with T(a, b) -> if n < (lgh x) then b:= n else raise TooShort; x;;

let replace s w = match s with T(a, b) -> a.(!b) <- w; s;;

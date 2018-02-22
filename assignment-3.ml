type symbol = Symbol of (string * int);;
type variable = Variable of string;;

let check_sig sign = List.fold_left (fun x y -> x && y) true (List.map (fun x -> match x with Symbol (s, n) -> n >= 0) sign) &&
                     List.fold_left (fun x y -> x && y) true (let seen = Hashtbl.create (List.length sign) in
                                List.map (fun x -> match x with Symbol (s, n) -> let tmp = not (Hashtbl.mem seen s) in Hashtbl.replace seen s (); tmp) sign)
;;

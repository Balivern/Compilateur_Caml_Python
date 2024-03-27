let tous_croissants_term (liste : int list) : bool =
  let rec verifier_elements = function
    | [] | [_] -> true
    | x::y::reste -> if x <= y then verifier_elements (y::reste) else false
  in
  verifier_elements liste
;;

tous_croissants_term [-4;-2;-1;4;10];;

tous_croissants_term [];;

tous_croissants_term [-4;-2;-1;4;-10];;
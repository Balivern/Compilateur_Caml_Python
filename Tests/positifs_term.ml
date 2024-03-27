let tous_positifs_term (liste: int list) : bool =
  let rec verifier_elements = function
    | [] -> true
    | t::q -> if t >= 0 then verifier_elements q else false
  in
  verifier_elements liste
;;

tous_positifs_term [];;

tous_positifs_term [2;9;0];;

tous_positifs_term [-2;9;0];;


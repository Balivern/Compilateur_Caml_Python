let rec tous_positifs liste =
  match liste with
  | [] -> true
  | t::q -> if t >= 0 then tous_positifs q else false
;;

tous_positifs [2;-9;0];;

tous_positifs [];;

tous_positifs [1;8;9];;
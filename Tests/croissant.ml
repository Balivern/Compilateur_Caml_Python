let rec tous_croissants (liste : int list) : bool =
  match liste with
  | [] | [_] -> true
  | x::y::reste -> if x <= y then tous_croissants (y::reste) else false
;;

tous_croissants [-4;-2;-1;4;10];;

tous_croissants [];;

tous_croissants [-4;-2;-1;4;-10];;
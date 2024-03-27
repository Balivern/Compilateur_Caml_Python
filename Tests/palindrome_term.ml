let est_palindrome_term (word : string) : bool =
  let longueur = String.length word in
  let rec comparer_caracteres i j =
    if i >= j then true
    else if word.[i] <> word.[j] then false
    else comparer_caracteres (i + 1) (j - 1)
  in
  comparer_caracteres 0 (longueur - 1)
;;

est_palindrome_term "kayak";;

est_palindrome_term "kaak";;

est_palindrome_term "données";;

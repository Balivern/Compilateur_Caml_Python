let taille_paire_term (liste : 'a list) : bool =
  let rec verifier_taille n = function
    | [] -> n mod 2 != 0
    | _::q -> verifier_taille (n + 1) q
  in
  verifier_taille 0 liste
;;

taille_paire_term ['a';'b';'c'] ;;

taille_paire_term [2;3];;

taille_paire_term [];;
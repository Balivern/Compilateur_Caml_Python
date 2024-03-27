let rec taille_impaire (liste : 'a list) : bool =
  match liste with
  | [] -> true
  | _::q -> not (taille_paire q)
;;

taille_impaire ['a';'b';'c'] ;;

taille_impaire [2;3];;

taille_impaire [];;
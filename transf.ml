  (* Transformation of Caml code to Python code *)

open Lang


module StringSet = Set.Make(String)

(* TODO: implement *)
let rec names_expr = StringSet.empty

(* TODO: implement *)
let transf_prog (Prog(fdfs, e)) = Prog(fdfs, e)

(* Test de récursivité termninale *)
let rec is_tailrec_expr func_name expr = match expr with
                            | CallE (liste) -> List.for_all (fun e -> containsRecursiveCall func_name e) liste (* Vérifie que les expression ne contiennent pas la fonction func_name *)
                            | BinOp (_, e1, e2) -> is_tailrec_expr func_name e1 && is_tailrec_expr func_name e2
                            | IfThenElse (_, e1, e2) -> is_tailrec_expr func_name e1 && is_tailrec_expr func_name e2
                            | _ -> false ;;

(* Fonction vérifiant si une expression contient un appel récursif sur f *)
let rec containsRecursiveCall f e = match e with
                            | CallE (exp :: _) -> (match func with 
                                               Const func -> not (exp = f)
                                              | _ -> containsRecursiveCall f exp)
                            | IfThenElse (cond, thenBranch, elseBranch) ->
                               containsRecursiveCall f cond ||
                               containsRecursiveCall f thenBranch ||
                               containsRecursiveCall f elseBranch
                            | BinOp (_, e1, e2) ->
                               containsRecursiveCall f e1 || containsRecursiveCall f e2
                            | _ -> false
                            
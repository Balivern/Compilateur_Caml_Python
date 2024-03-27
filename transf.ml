  (* Transformation of Caml code to Python code *)

open Lang


module StringSet = Set.Make(String)

(* TODO: implement *)
let rec names_expr = StringSet.empty

(* TODO: implement *)
let transf_prog (Prog(fdfs, e)) = Prog(fdfs, e)

(* Fonction vérifiant si une expression contient un appel récursif sur func_name *)
let rec containsRecursiveCall = fun
                                VarE e -> StringSet.singleton e
                                | BinOp (_,e1,e2) -> StringSet.union (containsRecursiveCall e1) (containsRecursiveCall e2)
                                | IfThenElse(cond,thenBranch,elseBranch) -> StringSet.union (containsRecursiveCall cond) (StringSet.union (containsRecursiveCall thenBranch) (containsRecursiveCall elseBranch))
                                | CallE(f::args) -> List.fold_left (fun acc a -> StringSet.union acc (containsRecursiveCall a)) (containsRecursiveCall f) args
                                | _ -> StringSet.empty ;;

(* Test de récursivité termninale *)
let rec is_tailrec_expr func_name expr = match expr with
                            CallE (f::args) -> (List.for_all (fun e -> not (StringSet.mem func_name (containsRecursiveCall e))) args) (* Vérifie que les expression ne contiennent pas la fonction func_name *)
                            | IfThenElse (cond, thenBranch, elseBranch) -> not (containsRecursiveCall cond) && is_tailrec_expr func_name thenBranch && is_tailrec_expr func_name elseBranch
                            | e -> not (StringSet.mem func_name (containsRecursiveCall e))

(* Fonction pour transformer une expression récursive terminale *)
let transf_expr func_name params expr =   if is_tailrec_expr func_name expr
                                          then (let rec aux = fun
                                                IfThenElse (cond, thenBranch, elseBranch) -> Cond(cond, aux thenBranch, aux elseBranch)
                                                | CallE (f :: args) -> Assign(params, args)
                                                | e -> Return e 
                                              in While(Const(BoolV true),(aux expr)))
                                          else Return expr ;;
                                              
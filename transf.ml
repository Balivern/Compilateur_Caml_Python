  (* Transformation of Caml code to Python code *)

open Lang


module StringSet = Set.Make(String)

(* TODO: implement *)
let rec names_expr = StringSet.empty

(* TODO: implement *)
let transf_prog (Prog(fdfs, e)) = Prog(fdfs, e)

(* Fonction vérifiant si une expression contient un appel récursif sur func_name *)
let rec containsRecursiveCall func_name e = match e with
                            | CallE (exp :: _) -> (match exp with 
                                               Const exp -> not (exp = func_name)
                                              | _ -> containsRecursiveCall func_name exp)
                            | IfThenElse (cond, thenBranch, elseBranch) -> containsRecursiveCall func_name cond || containsRecursiveCall func_name thenBranch || containsRecursiveCall func_name elseBranch
                            | BinOp (_, e1, e2) -> containsRecursiveCall func_name e1 || containsRecursiveCall func_name e2
                            | _ -> false ;;

(* Test de récursivité termninale *)
let rec is_tailrec_expr func_name expr = match expr with
                            | CallE (liste) -> List.for_all (fun e -> not (containsRecursiveCall func_name e)) liste (* Vérifie que les expression ne contiennent pas la fonction func_name *)
                            | IfThenElse (cond, thenBranch, elseBranch) -> not (containsRecursiveCall func_name cond) && is_tailrec_expr func_name thenBranch && is_tailrec_expr func_name elseBranch
                            | BinOp (_, e1, e2) -> is_tailrec_expr func_name e1 && is_tailrec_expr func_name e2
                            | Const exp -> not (exp = func_name)
                            | _ -> true ;;

(* Fonction pour transformer une expression récursive terminale *)
let rec transf_expr func_name params expr =   if is_tailrec_expr func_name expr 
                                              then match expr with
                                                    | IfThenElse (cond, thenBranch, elseBranch) -> Cond(cond, transf_expr func_name params thenBranch, transf_expr func_name params elseBranch)
                                                    | CallE (f :: args) when f = VarE func_name ->  let assignments = List.map2 (fun param arg -> (param, arg)) params args
                                                                                                    in Assign(assignments, Skip)
                                                    | _ -> Skip
                                              else expr ;;
                                              
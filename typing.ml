open Lang
  
(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     funbind: (vname * fpdecl) list} ;;

let tp_const = function
    BoolV _ -> BoolT
    | IntV _ -> IntT ;;
    
let rec tp_var env var = match tp_var_local_var env var with 
                            | None -> (match tp_var_funbind env var with 
                                | None -> failwith "tp_var: internal error: var not found in env"
                                | Some t -> t)
                            | Some t -> t ;;

let rec tp_var_local_var env var = List.assoc var env.localvar ;;

let rec tp_var_funbind env var = List.assoc var env.funbind  ;;

(*let function_type_correct tf argsT = match tf with
                            FunT (a,b) (ta :: reste) -> a = ta*)

(* Typage d’expressions simples *)
let rec tp_expr env = function
    Const c -> tp_const c
    | VarE v -> tp_var env v
    | BinOp (binop, e1, e2) ->( match binop with 
                                | BArith bop -> if (tp_expr e1)=IntT && (tp_expr e1)=(tp_expr e2) then IntT else failwith "tp_expr: wrong arithmetical operation form"
                                | BCompar bop -> let aux=(tp_expr e1) in if ((aux=IntT) || (aux=BoolT)) && (tp_expr e1)=(tp_expr e2) then aux else failwith "tp_expr: arguments types are not boolean"
                                | BLogic bop -> if (tp_expr e1)=BoolT && (tp_expr e1)=(tp_expr e2) then BoolT else failwith "tp_expr: this is not a binary operation"
                            )
    | IfThenElse (cond, e1, e2) -> if tp_expr env cond = BoolT && tp_expr env e1 = tp_expr env e2 then tp_expr env e1 else failwith "condition mal formée"
    | CallE (f:args) ->    let ft = tp_expr env f
                            in let argst = List.map(tp_expr env) args 
                            in tp_application ft argst
    | CallE (_) -> failwith "tp_expr: internal error: empty fun-args list in CallE" (* filtrage pour déterminer le type d'une fonction *)(*(let ts = List.map (tp_expr env) es in function_type_correct (List.hd ts) (List.lf ts) IfThenElse cond e1 e2)*)
    | _ -> failwith "a completer" ;;
    | CallE es -> funCallE env [] es;;

let rec funCallE env list_res es = match es with
        | [] -> list_res
        | (a::l) -> funCallE env (list_res@[(tp_expr env a)]) l;;

(* TODO: implement *)
let tp_prog (Prog (fdfs, e)) = IntT ;;

let rec tp_application f_type arg_types = match f_type with
                        | FunT (param_types, return_type) ->
                            (* Vérifier que le nombre d'arguments correspond au nombre de paramètres *)
                            if List.length arg_types <> List.length param_types 
                                then failwith "tp_application: wrong number of arguments"
                            else (* Vérifier que chaque argument a le type attendu *)
                                List.iter2 (fun arg_type param_type ->
                                    if arg_type <> param_type 
                                        then failwith "tp_application: argument type mismatch") arg_types param_types
                                    else return_type (* Renvoi du type de la fonction *)
                        | _ -> failwith "tp_application: not a function type" ;;
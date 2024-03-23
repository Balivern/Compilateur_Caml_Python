open Lang

(* NOTE : fonction Test recursivité terminale is_tailrec_expr dans le fichier transf.ml *)

(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     funbind: (vname * fpdecl) list} ;;

let tp_const = function
    BoolV _ -> BoolT
    | IntV _ -> IntT ;;

let rec tp_var_local_var env var = try List.assoc var env.localvar with _ -> failwith "erreur" ;;

let rec tp_var_funbind env var = try List.assoc var env.funbind with _ -> failwith "erreur" ;;
    
let rec tp_var env var = try tp_var_local_var env var 
                            with _ -> (try (match tp_var_funbind env var with |FPdecl(t, _, _) -> t)
                                            with _ -> failwith "tp_var: internal error: var not found in env");;

(* type fpdecl = FPdecl of tp * vname * (vardecl list)      |            type tp = BoolT | IntT | FunT of tp * tp   *)
    
(*    match tp_var_local_var env var with 
                            | None -> (match tp_var_funbind env var with 
                                | None -> failwith "tp_var: internal error: var not found in env"
                                | Some t -> t)
                            | Some t -> t ;;

let function_type_correct tf argsT = match tf with
                            FunT (a,b) (ta :: reste) -> a = ta*)

let rec tp_application f_type arg_types =   match arg_types with 
                              ([]) -> f_type
                            | (a::l) -> (match f_type with
                                    | FunT (param_types, return_type) -> if param_types = a then tp_application return_type l else failwith "erreur de type"
                                    | _ -> failwith "tp_application: not a function type" ) ;;

(* Typage d’expressions simples *)
let rec tp_expr env = function
    Const c -> tp_const c
    | VarE v -> tp_var env v
    | BinOp (binop, e1, e2) ->( match binop with 
                                | BArith bop -> if (tp_expr env e1)=IntT && (tp_expr env e1)=(tp_expr env e2) then IntT else failwith "tp_expr: wrong arithmetical operation form"
                                | BCompar bop -> let aux=(tp_expr env e1) in if ((aux=IntT) || (aux=BoolT)) && (tp_expr env e1)=(tp_expr env e2) then aux else failwith "tp_expr: arguments types are not boolean"
                                | BLogic bop -> if (tp_expr env e1)=BoolT && (tp_expr env e1)=(tp_expr env e2) then BoolT else failwith "tp_expr: this is not a binary operation")
    | IfThenElse (cond, e1, e2) -> if tp_expr env cond = BoolT && tp_expr env e1 = tp_expr env e2 then tp_expr env e1 else failwith "condition mal formée"
    | CallE (f::args) ->    let ft = tp_expr env f
                            in let argst = List.map(tp_expr env) args 
                            in tp_application ft argst
    | _ -> failwith "a completer" ;;

(* TODO: implement *)
let tp_prog (Prog (fdfs, e)) = IntT ;;

(* Test de récursivité termninale 
let rec is_tailrec_expr func_name expr = match expr with
                            | CallE (f, _) -> f = func_name (* Vérifie si l'appel est à la fonction elle-même *)
                            | BinOp (_, e1, e2) -> is_tailrec_expr func_name e1 && is_tailrec_expr func_name e2
                            | IfThenElse (_, e1, e2) -> is_tailrec_expr func_name e1 && is_tailrec_expr func_name e2
                            | _ -> false ;;*)


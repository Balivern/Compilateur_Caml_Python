
open Lang
  
(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     funbind: (vname * fpdecl) list} ;;

let tp_const = function
    BoolV _ -> BoolT
    | IntV _ -> IntT ;;
    
let tp_var env var = IntT

(*let function_type_correct tf argsT = match tf with
                            FunT (a,b) (ta :: reste) -> a = ta*)
    
(* Typage d’expressions simples *)
let rec tp_expr env = function
    Const c -> tp_const c
    | VarE v -> tp_var env v
    | BinOp (binop, e1, e2) -> failwith "a completer"
    | IfThenElse (cond, e1, e2) -> if tp_expr env cond = BoolT && tp_expr env e1 = tp_expr env e2 then tp_expr env e1 else failwith "condition mal formée"
    | CallE es -> failwith "a completer" (* filtrage pour déterminer le type d'une fonction *)(*(let ts = List.map (tp_expr env) es in function_type_correct (List.hd ts) (List.lf ts) IfThenElse cond e1 e2)*)
    | _ -> failwith "a completer" ;;
    | CallE es -> funCallE env [] es;;

let rec funCallE env list_res es = match es with
        | [] -> list_res
        | (a::l) -> funCallE env (list_res@[(tp_expr env a)]) l;;

(* TODO: implement *)
let tp_prog (Prog (fdfs, e)) = IntT

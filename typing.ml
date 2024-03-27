open Lang

(* NOTE : fonction Test recursivité terminale is_tailrec_expr dans le fichier transf.ml *)

(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     funbind: fpdecl list} ;;

let tp_const = function
    BoolV _ -> BoolT
    | IntV _ -> IntT ;;

let rec tp_var_local_var env var = try List.assoc var env.localvar with _ -> failwith "erreur" ;;

let rec tp_var_funbind env var = try (trouver var env.funbind) with _ -> failwith "erreur" ;;

let rec trouver var l= match l with
                    | [] -> failwith "erreur"
                    | (a::l2) -> (match a with 
                    |FPdecl (tp, vname, list)-> if vname=var then FPdecl (tp, vname, list) else trouver var l2 );;
                    
let rec tp_var env var =    try tp_var_local_var env var 
                            with _ -> ( try (match tp_var_funbind env var with |FPdecl(t, _, _) -> t)
                                        with _ -> failwith "tp_var: internal error: var not found in env");;


let rec tp_application f_type arg_types =   match arg_types with 
                              ([]) -> f_type
                            | (a::l) -> (match f_type with
                                    | FunT (param_types, return_type) -> if param_types = a then tp_application return_type l else failwith "type error"
                                    | _ -> failwith "tp_application: not a function type" ) ;;

(* Typage d’expressions simples *)
let rec tp_expr env = function
    Const c -> tp_const c
    | VarE v -> tp_var env v
    | BinOp (binop, e1, e2) ->( match binop with 
                                | BArith bop -> if (tp_expr env e1)=IntT && (tp_expr env e1)=(tp_expr env e2) then IntT else failwith "tp_expr: wrong arithmetical operation form"
                                | BCompar bop -> let aux=(tp_expr env e1) in if ((aux=IntT) || (aux=BoolT)) && (tp_expr env e1)=(tp_expr env e2) then aux else failwith "tp_expr: arguments types are not boolean"
                                | BLogic bop -> if (tp_expr env e1)=BoolT && (tp_expr env e1)=(tp_expr env e2) then BoolT else failwith "tp_expr: this is not a binary operation")
    | IfThenElse (cond, e1, e2) -> if tp_expr env cond = BoolT && tp_expr env e1 = tp_expr env e2 then tp_expr env e1 else failwith "syntax condition error"
    | CallE (f::args) ->    let ft = tp_expr env f
                            in let argst = List.map(tp_expr env) args 
                            in tp_application ft argst
    | _ -> failwith "param type unmatch " ;;

(*Vérification du corps des fonctions
Cette fonction vérifie le corps des fonctions déclarées dans le programme *)
let rec tp_funbind env (FPdecl (typ, name, e)) =
  (* Nous ajoutons les arguments de la fonction à l'environnement local *)
  let env' = { env with localvar = (name, typ) :: env.localvar } in
  (* Nous vérifions le type du corps de la fonction *)
  let typ' = tp_expr env' e in
  (* Si le type déclaré de la fonction ne correspond pas au type réel du corps de la fonction, une erreur est levée *)
  if typ = typ' then ()
  else failwith "tp_funbind: function body type does not match declared type";;

(* Cette fonction vérifie le programme dans son ensemble *)
let tp_prog (Prog (fdfs, e)) =
  (* Initialisation de l'environnement avec des variables locales vides et les déclarations de fonction du programme *)
  let env = { localvar = []; funbind = List.map (fun (fp,e) -> fp) fdfs } in
  (* Pour chaque déclaration de fonction, nous vérifions le corps de la fonction *)
  List.iter (fun fd -> tp_funbind env fd) fdfs;
  (* Nous obtenons le type de l'expression principale *)
  let main_type = tp_expr env e in
  (* Retourner le type de l'expression principale *)
  main_type;;


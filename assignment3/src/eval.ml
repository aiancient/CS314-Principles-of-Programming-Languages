open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

let rec inArray a b = match b with
  | [] -> false
  | (x,y)::t -> if x=a then true else inArray a t;;

let gettype a = match a with 
  | Int_Val(x) -> Int_Type
  | Bool_Val(y) -> Bool_Type;;

let getbool b = match b with 
  | Bool_Val(x) -> x;;

let getint i = match i with 
  | Int_Val(i) -> i;;

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)

(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value = match e with 
  | Number (i) -> Int_Val(i)
  
  | True-> Bool_Val(true)
  
  | False-> Bool_Val(false)
  
  | Var (v) -> if inArray v env = true then let (str,a) = List.find (fun (string,value) -> if string=v then true else false) env in a
                else raise UndefinedVar
  
  | Plus (a,b) -> (match (eval_expr a env, eval_expr b env) with 
      | (Int_Val (x), Int_Val (y)) -> Int_Val (x+y)
      | _ -> raise TypeError
    )
  
  | Minus (a,b) -> (match (eval_expr a env, eval_expr b env) with 
      | (Int_Val (x), Int_Val (y)) -> Int_Val (x-y)
      | _ -> raise TypeError
    )

  | Times (a,b) -> (match (eval_expr a env, eval_expr b env) with 
      | (Int_Val (x), Int_Val (y)) -> Int_Val (x*y)
      | _ -> raise TypeError
    )
  
  | Div (a,b) -> (match (eval_expr a env, eval_expr b env) with 
      | (Int_Val (x), Int_Val (y)) -> if y!=0 then Int_Val(x/y) else raise DivByZeroError
      | _ -> raise TypeError
    )

  | Mod (a,b) -> (match (eval_expr a env, eval_expr b env) with 
      | (Int_Val (x), Int_Val (y)) -> if y!=0 then Int_Val(x mod y) else raise DivByZeroError
      | _ -> raise TypeError
    )

  | Or (a,b) -> (match (eval_expr a env, eval_expr b env) with 
      | (Bool_Val (x), Bool_Val (y)) -> Bool_Val (x||y)
      | _ -> raise TypeError
    )

  | And (a,b) -> (match (eval_expr a env, eval_expr b env) with 
      | (Bool_Val (x), Bool_Val (y)) -> Bool_Val (x&&y)
      | _ -> raise TypeError
    )

  | Not (a) -> (match (eval_expr a env) with 
      | (Bool_Val (x)) -> Bool_Val (not x)
      | _ -> raise TypeError
    )

  | Lt (a,b) -> (match (eval_expr a env, eval_expr b env) with 
      | (Int_Val (x), Int_Val (y)) -> if x<y then Bool_Val(true) else Bool_Val(false)
      | _ -> raise TypeError
    )

  | Leq (a,b) -> (match (eval_expr a env, eval_expr b env) with 
      | (Int_Val (x), Int_Val (y)) -> if x<=y then Bool_Val(true) else Bool_Val(false)
      | _ -> raise TypeError
    )

  | Eq (a,b) -> (match (eval_expr a env, eval_expr b env) with 
      | (Int_Val (x), Int_Val (y)) -> if x=y then Bool_Val(true) else Bool_Val(false)
      | (Bool_Val (x), Bool_Val (y)) -> if x=y then Bool_Val(true) else Bool_Val(false)
      | _ -> raise TypeError
    )
  ;;

(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment = match c with
  | Skip -> env

  | Comp(x,y) -> eval_command y (eval_command x env) 

  | Declare(a,str) -> (match a with
      | Int_Type -> (str, Int_Val 0)::env
      | Bool_Type -> (str, Bool_Val false)::env)

  | Assg(str, exp) -> 
    if (inArray str env) = false then raise UndefinedVar
    else List.map (fun x -> let (a,b) = x in 
      if a=str then 
        if gettype(eval_expr exp env) = gettype(b) then (str,eval_expr exp env) else raise TypeError
        else x) env 

  | Cond(a,b,c) ->
    if gettype(eval_expr a env) != Bool_Type then raise TypeError 
    else (if getbool(eval_expr a env)=true then eval_command b env else eval_command c env)

  | While(a,b) ->
    if gettype(eval_expr a env) != Bool_Type then raise TypeError 
    else (if getbool(eval_expr a env)=true then eval_command c (eval_command b env) else env)





  

  

  
  

  

  

let _  = Random.self_init ()

type term =
  | Constant of string
  | Variable of string
  | Function of string * term list

type head = term
type body = term list

type clause = Fact of head | Rule of head * body

type program = clause list

type goal = term list

let rec string_of_f_list f tl =
  let _, s = List.fold_left (fun (first, s) t ->
    let prefix = if first then "" else s ^ ", " in
    false, prefix ^ (f t)) (true,"") tl
  in
  s

let rec string_of_term t =
  match t with
  | Constant c -> c
  | Variable v -> v
  | Function (f,tl) ->
      f ^ "(" ^ (string_of_f_list string_of_term tl) ^ ")"

let string_of_term_list fl =
  string_of_f_list string_of_term fl

let string_of_goal g =
  "?- " ^ (string_of_term_list g)

let string_of_clause c =
  match c with
  | Fact f -> string_of_term f ^ "."
  | Rule (h,b) -> string_of_term h ^ " :- " ^ (string_of_term_list b) ^ "."

let string_of_program p =
  let rec loop p acc =
    match p with
    | [] -> acc
    | [c] -> acc ^ (string_of_clause c)
    | c::t ->  loop t (acc ^ (string_of_clause c) ^ "\n")
  in loop p ""

let var v = Variable v
let const c = Constant c
let func f l = Function (f,l)
let fact f = Fact f
let rule h b = Rule (h,b)


(* ################# *)
(* # occurs_check ## *)
(* ################# *)

let rec occurs_check v t =
  match t with
  | Variable _ -> t = v
  | Constant _ -> false
  | Function (_,l) -> List.fold_left (fun acc t' -> acc || occurs_check v t') false l

(* ################# *)
(* ### Problem 1 ### *)
(* ################# *)

module VarSet = Set.Make(struct type t = term let compare = Pervasives.compare end)
(* API Docs for Set : https://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.S.html *)

let rec variables_of_term t =
  VarSet.empty

let variables_of_clause c =
  VarSet.empty


(* ################# *)
(* ### Problem 2 ### *)
(* ################# *)

module Substitution = Map.Make(struct type t = term let compare = Pervasives.compare end)
(* See API docs for OCaml Map: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.S.html *)

let string_of_substitution s =
  "{" ^ (
    Substitution.fold (
      fun v t s ->
        match v with
        | Variable v -> s ^ "; " ^ v ^ " -> " ^ (string_of_term t)
        | Constant _ -> assert false (* substitution maps a variable to a term *)
        | Function _ -> assert false (* substitution maps a variable to a term *)
    ) s "" 
  ) ^ "}"

let rec substitute_in_term s t =
  let acc = [] in
  match t with
  |Constant(x) -> t
  |Variable(x) -> if (Substitution.find_opt t s) = None then t else (Substitution.find t s)
  |Function(fun_name,l) ->
    Function(fun_name,List.fold_left(fun acc x -> acc@[substitute_in_term s x]) acc l)

let substitute_in_clause s c =
  let acc = [] in
  match c with
  |Fact(f) -> Fact(substitute_in_term s f)
  |Rule(h,b) ->
    Rule(substitute_in_term s h, List.fold_left(fun acc x -> acc@[substitute_in_term s x]) acc b)


(* ################# *)
(* ### Problem 3 ### *)
(* ################# *)

exception Not_unifiable

let rec unify sx sy =
  match sx,sy with 
  | x :: sx, y:: sy -> (
    match x,y with
    | Some vx ,None -> (
        match Substitution.find_opt vx !bindings with
        | Some x -> (
            match x with 
            | Some vx -> bind vx y; loop sx sy
            | None -> loop (x :: sx) (y :: sy)
        )
        | None -> bind vx y; loop sx sy
    )
  )

 
(* ################# *)
(* ### Problem 4 ### *)
(* ################# *)

let counter = ref 0
let fresh () =
  let c = !counter in
  counter := !counter + 1;
  Variable ("_G" ^ string_of_int c)

let freshen c =
  let vars = variables_of_clause c in
  let s = VarSet.fold (fun v s -> Substitution.add v (fresh()) s) vars Substitution.empty in
  substitute_in_clause s c

let c = (rule (func "p" [var "X"; var "Y"; const "a"]) [func "q" [var "X"; const "b"; const "a"]])
let _ = (*print_endline*) (string_of_clause c)
let _ = (*print_endline*) (string_of_clause (freshen c))

let nondet_query program goal =
  []


(* ################# *)
(* ### Problem 5 ### *)
(* ################# *)

let det_query program goal =
  [goal]

open List

let rec iterateList myList x1 x2 x3 = match myList with
  | [] -> ((x1),(x2),(x3))
  | head::body -> 
    if (head = 1) then iterateList body (x1+1) x2 x3
    else if (head = 2) then iterateList body x1 (x2+1) x3
    else if (head = 3) then iterateList body x1 x2 (x3+1)
    else iterateList body x1 x2 x3

let count123 l =
  iterateList l 0 0 0

let rec n_times (f, n, v) =
  if n > 0 then
    n_times (f,(n-1),f v)
  else v

let buckets p l = 
  let rec get ff num list= 
      match list with
      [] -> [[num]]
      | head::tail -> 
        match head with 
        [] -> []
        | h::_ -> 
          if p h num then (head@[num])::tail
          else head::(get p num tail)
  in 
  let rec combine ff pList pAcc= 
      match pList with
      [] -> pAcc
      | pH::pT -> combine p pT (get p pH pAcc) 
  in 
    match l with 
    [] -> []
    | pH::pT -> combine p pT [[pH]]  

let fib_tailrec n = 
    let rec aux i x y  =
      if i = n then x 
      else aux (i+1) (y) (x+y) 
    in 
  aux 0 0 1


let countNum lst num = 
  List.fold_left (fun a h -> if h=num then a+1 else a) 0 lst

let assoc_list l =
  List.map (fun x -> (x,countNum l x)) l


let apHelp fs args= 
  let fs = fs in List.map (fun x -> List.map x args) fs

let ap fs args =
  List.fold_right (@) (apHelp fs args) []

let getMax lst = 
  match lst with 
  | [] -> 0
  | h::[] -> 0
  | h::t -> List.fold_left max h t 

let countNum lst num = 
  List.fold_left (fun a h -> if h=num then a+1 else a) 0 lst

let numOfMax lst =
  countNum lst (getMax lst)

let changeMax lst maximum=
  List.map (fun x -> if(x=maximum) then -maximum else x) lst 

let maxl2 lst =
  match lst with 
  | [] -> 0
  | h::[] -> 0
  | h::t -> if ((numOfMax lst) > 1) then 2*(getMax lst) else (getMax (changeMax lst (getMax lst)))+(getMax lst)



type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec insert tree x =
  match tree with
  | Leaf -> Node(Leaf, x, Leaf)
  | Node(l, y, r) ->
     if x = y then tree
     else if x < y then Node(insert l x, y, r)
     else Node(l, y, insert r x)

let construct l =
  List.fold_left (fun acc x -> insert acc x) Leaf l


let lstOfNodesInOrder tree = 
  let rec aux tree accumulator =
      match tree with
      | Leaf -> accumulator
      | Node (left, value, right) -> 
        let acc_with_right = aux right accumulator in
        let acc_with_value = value :: acc_with_right in
        aux left acc_with_value in
  aux tree [] 

let rec excfunc f acc lstOfNodes=
  match lstOfNodes with 
    [] -> acc
    | (h::t) -> excfunc f (f acc h) t

let fold_inorder f acc t =
  excfunc f acc (lstOfNodesInOrder t)


let at_level t level =
    let rec currLevelAux t acc counter = match t with
      | Leaf -> acc
      | Node (l, x, r) ->
        if counter=level then
          x :: acc
        else
          currLevelAux l (currLevelAux r acc (counter + 1)) (counter + 1)
    in
    currLevelAux t [] 1


let rec height t =
  match t with
    Leaf -> 0
  | Node (l, v, r) -> 1 + (max (height l) (height r))


let rec l acc i height t=
  if i >= 1 then (l ((at_level t i)::acc) (i-1) height t) else acc
  
let levelOrder t =
  l [] (height t) (height t) t







(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for count123 *)
  let _ =
    try
      assert (count123 [3;4;2;1;3] = (1,1,2));
      assert (count123 [4;4;1;2;1] = (2,1,0))
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for n_times *)
  let _ =
    try
      assert (n_times((fun x-> x+1), 50, 0) = 50);
      assert (n_times((fun x-> (x +. 2.0)), 50, 0.0) = 100.0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in
  
  (* Testcases for buckets *)
  let _ =
    try
      assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]);
      assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]);
      assert (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fib_tailrec *)
  let _ =
    try
      assert (fib_tailrec 50 = 12586269025);
      assert (fib_tailrec 90 = 2880067194370816120);
      assert (fib_tailrec 0 = 0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for assoc_list *)
  let _ =
    let y = ["a";"a";"b";"a"] in
    let z = [1;7;7;1;5;2;7;7] in
    let a = [true;false;false;true;false;false;false] in
    let b = [] in
    let cmp x y = if x < y then (-1) else if x = y then 0 else 1 in
    try
      assert ([("a",3);("b",1)] = List.sort cmp (assoc_list y));
      assert ([(1,2);(2,1);(5,1);(7,4)] = List.sort cmp (assoc_list z));
      assert ([(false,5);(true,2)] = List.sort cmp (assoc_list a));
      assert ([] = assoc_list b)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for ap *)
  let _ =
    let x = [5;6;7;3] in
    let b = [3] in
    let c = [] in
    let fs1 = [((+) 2) ; (( * ) 7)] in
    try
      assert  ([7;8;9;5;35;42;49;21] = ap fs1 x);
      assert  ([5;21] = ap fs1 b);
      assert  ([] = ap fs1 c);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in
  
  (* Testcases for maxl2 *)  
  let _ =
    try
      assert (maxl2 [1;10;2;100;3;400] = 500)
      ; assert (maxl2 [] = 0)
      ; assert (maxl2 [1000;29;10;5;10000;100000] = 110000)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fold_inorder *)
  let _ =
    try
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = 6)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for levelOrder *)
  let _ =
    try
      assert (levelOrder (construct [3;20;15;23;7;9]) = [[3];[20];[15;23];[7];[9]]);
      assert (levelOrder (construct [41;65;20;11;50;91;29;99;32;72]) = [[41];[20;65];[11;29;50;91];[32;72;99]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  if !error_count = 0 then  Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 9 programming questions are incorrect.\n") (!error_count)

let _ = main()

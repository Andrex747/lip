module T = ANSITerminal
open Printf

let parse input =
  let lexbuf = Lexing.from_string input in
    Parser.prog Lexer.read_token lexbuf

  

(* let rec range a b = if b<a then [] else a::(range (a+1) b) *)

let rec zeroes = function
    0 -> []
  | n -> false::(zeroes (n-1))

let string_of_world1 w1 =
  List.fold_left (fun s x -> s ^ (if x then "*" else " ")) "" w1

let string_of_world w =
  List.fold_left (fun s x -> s ^ "\n" ^ string_of_world1 x) "" w

(* p in [0,100] is the probability of 1 *)

let rec rnd_world1 p = function
    0 -> []
  | n -> (Random.int(100)<p)::(rnd_world1 p (n-1))

let rec rnd_world p m = function
  0 -> []
| n -> (rnd_world1 p m) :: (rnd_world p m (n-1))

let init_w = rnd_world 20 30 60

let neighbours w i j = 
  let n = List.length w in
  let l0 = List.nth w ((i+n-1) mod n) 
  and l1 = List.nth w (i mod n) 
  and l2 = List.nth w ((i+1) mod n) 
  in 
  let m = List.length l0
  in  
  assert (List.length l1 = m && List.length l2 = m);
  (
    List.nth l1 (j mod m),
    [
      [List.nth l0 ((j+m-1) mod m); List.nth l0 (j mod m); List.nth l0 ((j+1) mod m)]; 
      [List.nth l1 ((j+m-1) mod m); false; List.nth l1 ((j+1) mod m)]; 
      [List.nth l2 ((j+m-1) mod m); List.nth l2 (j mod m); List.nth l2 ((j+1) mod m)]
    ])

let count1 l = List.fold_left (fun s x -> s + (if x then 1 else 0)) 0 l

let count w = List.fold_left (fun s x -> s + count1 x) 0 w

(*let alive w i j rule =
  let (cell,nb) = neighbours w i j in
  let alive_nb = count nb in
  if cell then (* cell is alive *)
    (* cell survives? *)
    alive_nb = rule || alive_nb = 3
  else (* cell is dead *)
    (* cell is born? *)
    alive_nb = 3
*)
let alive w i j (rule : Rule.rule)  =
  let (cell, nb) = neighbours w i j in
  let alive_nb = count nb in
  let rule_pair = match rule with
        | Rule (s, b) -> (s, b)  
        | Seq s -> (s, [])  
  in
  let (survival_rules, birth_rules) = rule_pair in 
  if cell then (* cell is alive *)
    List.mem alive_nb survival_rules
  else (* cell is dead *)
    List.mem alive_nb birth_rules

let step1 w i rule =
  let n = List.length w in
  List.mapi (fun j _ -> alive w i j rule) (zeroes n)

let step w rule =
  let n = List.length w in
  List.mapi (fun i _ -> step1 w i rule) (zeroes n)

(* let step w = List.map step1 w *)
(* let step w = w *)

let display w =
  T.erase T.Screen;
  T.set_cursor 1 1;
  (* T.print_string [] (string_of_world w); *)
  printf "%s\n%!" (string_of_world w);
  Unix.sleepf 0.15;;

let rec loop w n rule =
  if n=0 then (display w; w)
  else (display w; loop (step w rule) (n-1) rule)

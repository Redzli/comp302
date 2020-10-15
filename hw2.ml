exception NotImplemented
exception Error

(* Q1 The type of nucleobase. *)
type nucleobase = T | A | C | G

(* Q2 *)
type exp = 
  | PLUS  of exp * exp  (* Plus *)
  | MINUS of exp * exp  (* Minus *)
  | MULT of exp * exp  (* Mult *)
  | DIV   of exp * exp  (* Div *)
  | SIN   of exp        (* Sin *)
  | COS   of exp        (* Cos *)
  | EXP   of exp        (* Exp *)
  | FLOAT of float

type instruction = Plus | Minus | Mult | Div | Sin | Cos | Exp | Float of float

type stack = float list


(* Q1 *)
(* TODO: Write a good set of tests for compress *)
let compress_tests = [
  ( [], [] );
  ( [A], [(1, A)] );
  ( [A;A;A;A;G;G;A;T;T;T;C;T;C], [(4,A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)] );
]

(* TODO: Implement compress. *)

let rec count (n:int) (s:nucleobase) (l:nucleobase list): (int * nucleobase list) =
  match l with
  | [] -> (n,[])
  | x::t -> if (x=s) then count (n+1) s t
      else (n,l)
  
let rec compress (l : nucleobase list) : (int * nucleobase) list =
  match l with
  | [] -> []
  | x::t -> let (n,l') = count 1 x t in (n,x)::(compress l')
    
              
(* TODO: Write a good set of tests for decompress *)
let decompress_tests = [
  ( [], [] );
  ( [(1, A)], [A]);
  ( [(4,A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)], [A;A;A;A;G;G;A;T;T;T;C;T;C] );
]

(* TODO: Implement decompress. *)

let rec build (n:int) (s:nucleobase) : nucleobase list =
  match n with
  | 0 -> []
  | _ -> s::(build (n-1) s) 

let rec decompress (l : (int * nucleobase) list) : nucleobase list =
  match l with
  | [] -> []
  | (n, s)::t -> (build n s)@(decompress t)



(* Q2 *)
(* TODO: Write a good set of tests for eval *)
let eval_tests = [ 
  ( FLOAT 0.0, 0.0 );
  ( MULT (FLOAT 0.0, FLOAT 1.0), 0.0 );
  ( MINUS (FLOAT 3.5, FLOAT 2.0), 1.5 );
  ( DIV (FLOAT 4.0, FLOAT 2.0), 2.0 );
  ( SIN (FLOAT 0.0), 0.0 );
  ( COS (FLOAT 0.0), 1.0 );
  ( EXP (FLOAT 0.0), 1.0 );
  ( MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0), 27.5 ); 
]

(* TODO: Implement eval. *)
let rec eval (e: exp): float =
  match e with
  | PLUS  (l,r) -> (eval l) +. (eval r)
  | MINUS (l,r) -> (eval l) -. (eval r)
  | MULT  (l,r) -> (eval l) *. (eval r)
  | DIV   (l,r) -> (eval l) /. (eval r)
  | SIN   (x) -> sin(eval x)
  | COS   (x) -> cos(eval x)
  | EXP   (x) -> exp(eval x)
  | FLOAT (n) -> n

(* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
  ( FLOAT 0.0, [Float 0.0] );
  ( MULT (FLOAT 0.0, FLOAT 1.0), [Float 0.0; Float 1.0; Mult] );
  ( MINUS (FLOAT 3.5, FLOAT 2.0), [Float 3.5; Float 2.0; Minus] );
  ( DIV (FLOAT 4.0, FLOAT 2.0), [Float 4.0; Float 2.0; Div] );
  ( SIN (FLOAT 0.0), [Float 0.0; Sin] );
  ( COS (FLOAT 0.0), [Float 0.0; Cos] );
  ( EXP (FLOAT 0.0), [Float 0.0; Exp] );
  ( MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0), [Float 2.2; Float 3.3; Plus; Float 5.0; Mult] ); 
]

(* TODO: Implement to_instr. *)
let rec to_instr (e: exp): instruction list = 
  match e with
  | PLUS  (l,r) -> (to_instr(l) @ to_instr(r)) @ Plus::[]
  | MINUS (l,r) -> (to_instr(l) @ to_instr(r)) @ Minus::[]
  | MULT  (l,r) -> (to_instr(l) @ to_instr(r)) @ Mult::[]
  | DIV   (l,r) -> (to_instr(l) @ to_instr(r)) @ Div::[]
  | SIN   (x) -> to_instr(x) @ Sin::[]
  | COS   (x) -> to_instr(x) @ Cos::[]
  | EXP   (x) -> to_instr(x) @ Exp::[]
  | FLOAT (n) -> Float n::[]


(* TODO: Write a good set of tests for instr *)
let instr_tests = [
  ((Plus, [0.]), None);
  ((Minus, [0.]), None);
  ((Mult, [0.]), None);
  ((Div, [0.]), None);
  ((Sin, []), None);
  ((Cos, [0.;165.]), Some [1.;165.]);
  ((Exp, []), None);
  ((Float 3.2, []), Some [3.2]);
  ((Mult, [5.0; 5.5]), Some [27.5]);
  ((Plus, [2.2; 3.3; 5.0]), Some [5.5; 5.]);
  ((Float 4.2, [2.2; 3.3; 5.5]), Some [4.2; 2.2; 3.3; 5.5]);
]

(* TODO: Implement instr. *)
let instr (i: instruction) (s: stack): stack option = 
  match (i,s) with 
  | (Plus, a::b::t)  -> Some((b+.a)::t)
  | (Minus, a::b::t) -> Some((b-.a)::t)
  | (Mult, a::b::t)  -> Some((b*.a)::t)
  | (Div, a::b::t)   -> Some((b/.a)::t)
  | (Sin, a::t)  -> Some((sin a)::t)
  | (Cos, a::t)  -> Some((cos a)::t)
  | (Exp, a::t)  -> Some((exp a)::t)
  | (Float(n),_) -> Some(n::s)
  | (_,_) -> None
    
(* TODO: Write a good set of tests for prog *)
let prog_tests = [
  ([Float 0.0], Some 0.0);
  ([Float 0.0; Float 1.0; Mult], Some 0.0);
  ([Float 3.5; Float 2.0; Minus], Some 1.5);
  ([Float 4.0; Float 2.0; Div], Some 2.0);
  ([Float 0.0; Sin], Some 0.0);
  ([Float 0.0; Cos], Some 1.0);
  ([Float 0.0; Exp], Some 1.0);
  ([Float 2.2; Float 3.3; Plus; Float 5.0; Mult], Some 27.5);
  
  ([], None);
  ([Float 0.; Plus], None);
  ([Float 0.; Minus], None);
  ([Float 0.; Mult], None);
  ([Float 0.; Div], None);
  ([Sin], None);
  ([Cos], None);
  ([Exp], None);
]

(* TODO: Implement prog. *)
let prog (l: instruction list): float option = 
  let rec helper (list: instruction list) (s: stack): stack option = 
    match list with
    | [] -> Some s 
    | h::t -> match instr h s with
      | None -> None
      | Some(x) -> helper t x 
  in match helper l [] with
  | None -> None
  | Some([]) -> None
  | Some(h::_) -> Some h
  
  
  
  

exception NotImplemented
let domain () =
    failwith "REMINDER: You should not be writing tests for undefined values."


(* Question 1 *)
(* TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
(* TODO: Correct these tests for the fact function. *)
let fact_tests = [
  (0, 1.0);
  (1, 1.0);
  (2, 2.0);
  (3, 6.0);
  (4, 24.0);
  (5, 120.0);
  (10, 3628800.0);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*) 

let fastfact (n: float): float = 
  let rec helper (n, acc) = 
    if n = 0. then acc
    else helper (n -. 1., n *. acc)
  in helper (n, 1.)
    
let fact (n: int): float = 
  match n > 0 with
  | false -> 1.0
  | true -> fastfact (float_of_int(n))

(* TODO: Write your own tests for the binomial function.
         See the provided tests for fact, above, for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0, 0), 1.);
  ((1, 0), 1.);
  ((2, 0), 1.);
  ((10,1), 10.);
  ((10,2), 45.);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial (n: int) (k:int): float =
  if n < 0 then domain () 
  else if k > n then domain ()
  else (fact n) /. ( (fact k)  *. (fact (n-k)) )


(* TODO: Write a good set of tests for ackerman. *)
let ackerman_tests = [
  (* Your test cases go here *)
  ((0, 0), 1);
  ((0, 1), 2);
  ((1, 0), 2);
  ((1, 1), 3);
  ((0, 2), 3);
  ((2, 0), 3);
  ((2, 2), 7);
  ((0, 3), 4);
  ((3, 0), 5);
  ((3, 3), 61);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let ackerman (n, k: int * int): int  =
  if n < 0 || k < 0 then domain ()
  else (let rec ack n k = match (n,k) with 
      | (0 , _ ) -> k + 1 
      | (_ , 0 ) -> ack (n-1) 1
      | (_ , _ ) -> ack (n-1) (ack n (k-1))
     in ack n k)


(* Question 2: is_prime *)

(* TODO: Write a good set of tests for is_prime. *)
let is_prime_tests = [
(* Your tests go here *) 
  (2, true);
  (3, true);
  (4, false);
  (5, true);
  (6, false);
  (7, true);
  (11, true);
  (47, true);
  (89, true);
  (100, false);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let is_prime (n: int): bool =
  if n > 1 then
    let rec findprime (a: int) =
      if (a = 1) then true 
      else if (n mod a = 0) then false
      else findprime (a-1);
    in findprime (int_of_float(sqrt(float_of_int n)));
  else domain ()


(* Question 3: Newton-Raphson method for computing the square root
*)

let square_root_tests = [ 
  (1., 1.); 
  (4., 2.); 
  (9., 3.);
  (16., 4.);
  (25., 5.);
  (100., 10.);
]

let square_root (a: float): float =
  let rec findroot x acc =
    let x' = ((a/.x) +. x) /. 2. in
    if abs_float(x-.x') < acc then x'
    else findroot x' acc
  in
  if a > 0.0 then
    findroot 1.0 epsilon_float
  else domain ()

 
(* Question 4: Fibonacci*)

(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [
  (0,1);
  (1,1);
  (2,2);
  (3,3);
  (4,5);
  (5,8);
  (10,89);
  (20,10946);
]

(* TODO: Implement a tail-recursive helper fib_aux. *)
let rec fib_aux (n: int) (a: int) (b: int): int =
  if (n = 0) then (a+b)
  else fib_aux (n-1) (a+b) (a)

(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl (n: int): int =
  match n with
  | (0) -> 1
  | (1) -> 1
  | (_) -> fib_aux (n-2) 1 1
 
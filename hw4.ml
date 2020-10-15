exception NotImplemented
exception Fail

exception Msg of string

type passwd = string
type bank_account = {update_passwd  : passwd -> passwd -> unit;
                     retrieve       : passwd -> int -> unit;
                     deposit        : passwd -> int -> unit;
                     print_balance  : passwd -> int }

(* Bank account errors *)
let wrong_pass = Msg "Wrong Password"
let too_many_attempts = Msg "Change your password"
let no_money = Msg "Insufficient funds"


let rec fib n = if n = 0 then 0
                else (if n = 1 then 1 else fib (n-2) + fib (n-1))

type fib_result =
  { num_rec : int;
    result  : int }

type stats =
  { entries : int ref;
    lkp : int ref }

let store : (int, int) Hashtbl.t = Hashtbl.create 1000

(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)

let new_account (p: passwd) : bank_account =
  let balance = ref 0 in
  let password = ref p in
  let attempts = ref 0 in

  let checkPass (p: passwd): bool =
    if (!attempts < 3) then 
      if (p = !password) then (attempts := 0; true)
      else (attempts := !attempts+1; raise wrong_pass)
    else raise too_many_attempts
  in {
    update_passwd = (fun (oldP: passwd) (newP: passwd) -> 
      if (oldP = !password) then (password := newP; attempts := 0)
      else (attempts := !attempts+1; raise wrong_pass));
    retrieve = (fun (p: passwd) (amount: int) -> 
      if (checkPass p) then
        if (amount <= !balance) then balance := !balance - amount
        else raise no_money);
    deposit = (fun (p: passwd) (amount: int) -> 
      if (checkPass p) then balance := !balance + amount);
    print_balance = (fun (p: passwd) -> 
      if (checkPass p) then !balance else raise too_many_attempts);
      (*Else statement above will never be reached, only needed to compile *)
  }
;;

(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)

let rec fib_I (n: int) : fib_result =
  if (n = 0) then {num_rec = 1; result = 0;} else
  if (n = 1) then {num_rec = 1; result = 1;} else
    let a = ref (fib_I(n-2)) in
    let b = ref (fib_I(n-1))
    in {
      num_rec = !a.num_rec + !b.num_rec + 1;
      result = !a.result + !b.result;
    }
;;

(* Q 2.2 : Memoization with a global store *)

let fib_memo (n: int) : int =
  let rec fib n =
    match Hashtbl.find_opt store n with
    | Some(v) -> v
    | None -> 
        if n = 0 then (Hashtbl.add store 0 0; 0) else 
        if n = 1 then (Hashtbl.add store 1 1; 1) else 
        let v = fib (n-2) + fib (n-1) in Hashtbl.add store n v; v
  in fib n
;;

(* Q 2.3 : General memoization function *)

let memo (f: (('a -> 'b) -> 'a -> 'b)) (stats: stats) : ('a -> 'b) =
  let entries = stats.entries in
  let lookup = stats.lkp in
  let table = ref (Hashtbl.create 1000) in
  let rec g (input: 'a): 'b =
    match Hashtbl.find_opt !table input with
    | Some(v) -> (lookup := !lookup+1; v)
    | None -> 
        entries := !entries+1; 
        let v = f g input in Hashtbl.add !table input v; v
  in g
;;

(* Q 2.4 : Using memo to efficiently compute the Fibonacci number *)

let fibM: (int -> (int * stats)) =
  let info = {
    entries = ref 0;
    lkp = ref 0;
  } in
  let f (g: (int -> int)) (x: int): int = 
    if x = 0 then 0 else
    if x = 1 then 1 else
      g(x-2) + g(x-1)
  in 
  let memoFib = memo f info in
  fun n -> (memoFib n, info)
;;
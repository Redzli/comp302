(* Q0  : Get familiar with the external syntax of MiniML *)
let parse_tests : (string * (string, exp) either) list = [
    (* Provide your tests for the parser *)
  ("1;", Right (Int 1));
  (valid_program_1, Right
     (Let
        ([Val
            (Rec ("apply", TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt)),
                  Fn
                    ("f", Some (TArrow (TInt, TInt)),
                     Fn ("x", Some TInt, Apply (Var "f", Var "x")))),
             "apply")],
         Apply
           (Apply (Var "apply", Fn ("x", None, Primop (Times, [Var "x"; Int 3]))),
            Int 100))))
]


let toExp (program: string) : exp =
  match P.parse program with
  | Right e -> e
  | Left e -> stuck "Invalid Program"

let rec flatExp (input: exp) : exp =
  match input with
  | If (e,e1,e2)  -> If (flatExp e, flatExp e1, flatExp e2)
  | Primop (op,l) -> Primop (op, List.map flatExp l)
  | Tuple l       -> Tuple (List.map flatExp l)
  | Fn (x,t,e)    -> Fn (x, t, flatExp e)
  | Rec (x,t,e)   -> Rec (x, t, flatExp e)
  | Let ([],e)    -> e
  | Let (h::t,e)  -> Let ([h], flatExp (Let(t,e)))
  | Apply (e1,e2) -> Apply (flatExp e1, flatExp e2)
  | Anno (e,t)    -> Anno (flatExp e, t)
  | Int _  -> input
  | Bool _ -> input
  | Var _  -> input

let free_vars_tests : (exp * name list) list = [
  (Int 10, []);
  (toExp valid_program_1, []);
  (toExp valid_program_2, []);
  (toExp valid_program_3, []);
  (toExp valid_program_4, []);
  (toExp valid_program_5, []);
  (toExp valid_program_6, []);
  (toExp valid_program_7, []);
  (toExp valid_program_8, []);
  (toExp valid_program_9, []);
  (toExp valid_program_10, []);
  
  (toExp "fn x : int => x * x;", []);
  (toExp "fn x : int => x * y;", ["y"]); 
  (toExp "fn x : int => x * y * z;", ["y"; "z"]);
  (toExp "let fun f : int = fn x : int => x * y * z in 6 end;", ["y"; "z"]);
  (toExp "let fun f : int = fn x : int => x * y * z in apply 6 end;", ["y"; "z"; "apply"]);
  (toExp "let val x = 3 in let val (y1,y2) = (x, 2) in x + y1 end end;", []);
  (toExp "let val x = 3\n val (y1,y2) = (x, 2) in x + y1 end;", []);
  (toExp "let val x = 5 name y = x + 3 val z = 3 in x + y + z end;", []);
  (toExp "let val x = x name y = x + 3 val z = 3 in x + y + z end;", ["x"]);
]

(* Q1  : Find the free variables in an expression *)
let getExp (d: dec): exp =
  match d with
  | Val (e,_)       -> e
  | Valtuple (e,_)  -> e
  | ByName (e,_)    -> e

let getNames (d: dec) : name list =
  match d with
  | Val (_,n)       -> [n]
  | Valtuple (_,l)  -> l
  | ByName (_,n)    -> [n]

let rec free_vars (e : exp) : name list = 
  match flatExp e with
  | Int _         -> []
  | Bool _        -> []
  | Var x         -> [x]
  | If (e,e1,e2)  -> union (union (free_vars e1) (free_vars e2)) (free_vars e) 
  | Primop (_,l)  -> union_list (List.map free_vars l)
  | Tuple l       -> union_list (List.map free_vars l)
  | Fn (x,_,e)    -> delete [x] (free_vars e)
  | Rec (x,_,e)   -> delete [x] (free_vars e)
  | Let ([dec],e) -> union (free_vars (getExp dec)) (delete (getNames dec) (free_vars e))
  | Apply (e1,e2) -> union (free_vars e1) (free_vars e2)
  | Anno (e,_)    -> free_vars e 
  | _ -> stuck "Q1: Invalid Expression"


let unused_vars_tests : (exp * name list) list = [
  (Int 10, []);
  (toExp valid_program_1, []);
  (toExp valid_program_2, []);
  (toExp valid_program_3, []);
  (toExp valid_program_4, []);
  (toExp valid_program_5, []);
  (toExp valid_program_6, ["x"]);
  (toExp valid_program_7, []);
  (toExp valid_program_8, []);
  (toExp valid_program_9, []);
  (toExp valid_program_10, ["y"]);
  
  (toExp "fn x : int => x * x;", []);
  (toExp "fn x : int => x * y;", []); 
  (toExp "let fun f : int = fn x : int => x * y * z in 6 end;", ["f"]);
  (toExp "let fun f : int = fn x : int => f(y) * z in f 6 end;", ["x"]);
  (toExp "let fun f : int = fn x : int => x * z in f 6 end;", []); 
  (toExp "let val x = 3 val y = 4 in x + y end;",[]);
  (toExp "let val x = let val y = 2 in 3 end val y = 4 in x + y end;", ["y"]);
  (toExp "let val x = true in let val x = 1 in let val x = 3 in x + 5 end end end;", ["x"]);
  (toExp "let val x = 3 in let val (y1,y2) = (x, 2) in x + y1 end end;", ["y2"]); 
  (toExp "let val x = 3\n val (y1,y2) = (x, 2) in x + y1 end;", ["y2"]); 
]

(* Q2  : Check variables are in use *)
let rec unused_vars (e : exp) : name list = 
  match flatExp e with 
  | Int _         -> []
  | Bool _        -> []
  | Var x         -> []
  | If (e,e1,e2)  -> union (union (unused_vars e1) (unused_vars e2)) (unused_vars e) 
  | Primop (_,l)  -> union_list (List.map unused_vars l)
  | Tuple l       -> union_list (List.map unused_vars l)
  | Fn (x,_,e)    -> union (delete (free_vars e) [x]) (unused_vars e)
  | Rec (x,_,e)   -> unused_vars e
  | Let ([dec],e) -> union (unused_vars (getExp dec)) (union (delete (free_vars e) (getNames dec)) (unused_vars e))
  | Apply (e1,e2) -> union (unused_vars e1) (unused_vars e2)
  | Anno (e,_)    -> unused_vars e 
  | _ -> stuck "Q2: Invalid Expression"
 

let subst_tests : (((exp * name) * exp) * exp) list = [
  (((Int 5, "x"), (If (Bool(true), Var"x", Var"y"))), (If (Bool(true), Int 5, Var"y")));
  (((Var "z", "x"), Var "x"), Var "z");
  (((Var "z", "x"), toExp "fn x : int => x * x;"), 
   toExp "fn x : int => x * x;");
  (((Var "z", "x"), toExp "fn z : int => x * x;"), 
   (Fn ("1z", Some TInt, Primop (Times, [Var "z"; Var "z"]))));
  (((Var "z", "x"), toExp "fn z : int => z * x;"), 
   (Fn ("1z", Some TInt, Primop (Times, [Var "1z"; Var "z"]))));
  
  (((Var "z", "x"), toExp "let val x = 6 in x * x end;"), 
   toExp "let val x = 6 in x * x end;");
  (((Var "z", "x"), toExp "let val (x,y,z) = (1,x,y) in x * x end;"), 
   toExp "let val (x,y,z) = (1,z,y) in x * x end;");
  (((Var "x", "a"), toExp "let val (x,y,z) = (1,a,b) in x * a end;"), 
   Let ([Valtuple (Tuple [Int 1; Var "x"; Var "b"], ["1x"; "y"; "z"])],
        Primop (Times, [Var "1x"; Var "x"])));
  ((((Primop (Times, [Var"x"; Var"y"])), "a"), toExp "let val (x,y,z) = (1,a,b) in x * a end;"), 
   Let ([Valtuple (Tuple [Int 1; (Primop (Times, [Var"x"; Var"y"])); Var "b"], ["1x"; "2y"; "z"])],
        Primop (Times, [Var "1x"; (Primop (Times, [Var"x"; Var"y"]))])));
  
  (((Int 1, "x"), toExp "let val x = 5 name y = x + 3 val z = 3 in x + y + z end;"), 
   toExp "let val x = 5 in let name y = x + 3 in let val z = 3 in x + y + z end end end;");
  (((Int 1, "x"), toExp "let val x = x name y = x + 3 val z = 3 in x + y + z end;"), 
   toExp "let val x = 1 in let name y = x + 3 in let val z = 3 in x + y + z end end end;");
  (((Int 1, "x"), toExp "let val a = x name b = x + 3 val c = 3 in a + b + x end;"), 
   toExp "let val a = 1 in let name b = 1 + 3 in let val c = 3 in a + b + 1 end end end;");
] 

let changeName ((newV, oldV): name * name) (d: dec) : dec =
  match d with
  | Val (e,n)       -> Val (e, newV)
  | Valtuple (e,l)  -> Valtuple (e, List.map (fun n -> if n = oldV then newV else n) l)
  | ByName (e,n)    -> ByName (e, newV)

(* Q3  : Substitute a variable *)
let rec subst ((e', x) : exp * name) (e : exp) : exp =
  match flatExp e with
  | Var y  -> if x = y then e' else Var y
  | Int _  -> e
  | Bool _ -> e
  | Primop (op, es)  -> Primop (op, List.map (subst (e', x)) es)
  | If (e1, e2, e3)  -> If (subst (e', x) e1, subst (e', x) e2, subst (e', x) e3)
  | Tuple es         -> Tuple (List.map (subst (e', x)) es)
  | Anno (e, t)      -> Anno (subst (e', x) e, t) 
                          
  | Let ([dec], e)   -> let ys = getNames dec in
                        if member x ys then Let([substDec (e', x) dec], e)
                        else (
                          try let y = List.find (fun n -> member n (free_vars e')) ys in
                              let newY = fresh_var y in
                              subst (e', x) (Let ([changeName (newY, y) dec], subst (Var newY, y) e))
                          with Not_found -> Let ([substDec (e', x) dec], subst (e', x) e)     )

  | Fn (y, t, e)     -> if x = y then Fn (y, t, e)
                        else if member y (free_vars e') then let newY = fresh_var y in 
                          Fn (newY, t, subst (e', x) (subst (Var newY, y) e))
                        else Fn (y, t, subst (e', x) e)
  | Rec (y, t, e)    -> if x = y then Rec (y, t, e)
                        else if member y (free_vars e') then let newY = fresh_var y in 
                          Rec (newY, t, subst (e', x) (subst (Var newY, y) e))
                        else Rec (y, t, subst (e', x) e)
  | Apply (e1, e2)   -> Apply (subst (e', x) e1, subst (e', x) e2)
  | _ -> stuck "Q3: Invalid Expression"
and
  substDec ((e', x) as s : exp * name) (d: dec): dec =
  match d with
  | Val (e,n)       -> Val (subst s e, n)
  | Valtuple (e,l)  -> Valtuple (subst s e, l)
  | ByName (e,n)    -> ByName (subst s e, n)  


let eval_tests : (exp * exp) list = [
  (toExp valid_program_1, Int 300);
  (toExp valid_program_2, Int 133);
  (toExp valid_program_3, Int 120);
  (toExp valid_program_4, Int 3);
  (toExp valid_program_5, Int 6);
  (toExp valid_program_6, Int 6);
  (toExp valid_program_7, Int 4);
  (toExp valid_program_8, Int 900);
  (toExp valid_program_9, Int 1600);
  (toExp valid_program_10, Int 10);

  (Primop (Or, [Bool false; Primop (Or, [Bool false; Primop (Or, [Bool false; Bool true])])]), Bool true);
  (Primop (And, [Bool true; Primop (And, [Bool true; Primop (And, [Bool true; Bool false])])]), Bool false);
]

let weave (xs: name list) (vs: exp list) : (exp * name) list =
  List.map2 (fun x v -> (v,x)) xs vs

(* Q4  : Evaluate an expression in big-step *)
let rec eval : exp -> exp =
  (* do not change the code from here *)
  let bigstep_depth = ref 0 in
  fun e ->
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "eval (" ^ Print.exp_to_string e ^ ")\n");
    incr bigstep_depth;
  (* to here *)
    let result =
      match e with
      | Int _ | Bool _ -> e
      | Tuple es -> Tuple (List.map eval es)
      | If (e1, e2, e3) ->
          begin match eval e1 with
            | Bool b ->
                if b then
                  eval e2
                else
                  eval e3
            | _ -> stuck "Condition for if expression should be of the type bool"
          end
      | Anno (e, _) -> eval e     (* types are ignored in evaluation *)
      | Var x -> stuck ("Free variable \"" ^ x ^ "\" during evaluation")

      | Fn (x, t, e) -> Fn (x, t, e)
      | Apply (e1, e2) -> 
          let v2 = eval e2 in 
          begin match e1 with
            | Fn (x, t, e) -> eval (subst (v2, x) e)
            | Rec (f, t, e) -> eval (Apply ((eval (Rec (f, t, e))), v2))
            | Apply (e1, e2) -> eval ( Apply((eval (Apply (e1, e2))), v2) )
            | _ -> stuck "Given expressions cannot be applied"
          end
      | Rec (f, t, e) -> eval (subst (Rec (f, t, e), f) e)

      | Primop (And, es) ->
          begin match es with
            | [e1; e2] -> 
                begin match eval e1 with
                  | Bool b -> if b then eval e2 else Bool (false)
                  | _      -> stuck "Condition for AND operation should be of the type bool"
                end
            | _ -> stuck "AND operation requires two arguments"
          end
      | Primop (Or, es) ->
          begin match es with
            | [e1; e2] -> 
                begin match eval e1 with
                  | Bool b -> if b then Bool (true) else eval e2
                  | _      -> stuck "Condition for OR operation should be of the type bool"
                end
            | _    -> stuck "OR operation requires two arguments"
          end
      | Primop (op, es) ->
          let vs = List.map eval es in
          begin match eval_op op vs with
            | None -> stuck "Bad arguments to primitive operation"
            | Some v -> v
          end

      | Let (ds, e)  -> 
          begin match ds with
            | Val (e1,x1)::t     -> eval (subst (eval e1, x1) (Let (t,e)))
            | ByName (e1,x1)::t  -> eval (subst (e1, x1) (Let (t,e)))
            | Valtuple (e1,l)::t -> 
                begin match eval e1 with 
                  | Tuple (vs) -> eval (List.fold_right subst (weave l vs) (Let(t,e)) )
                  | _          -> stuck ("Let expression must evaluate to a tuple")
                end
            | []                 -> eval e
          end
    in
  (* do not change the code from here *)
    decr bigstep_depth;
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "result of eval (" ^ Print.exp_to_string e ^ ") = "
         ^ Print.exp_to_string result ^ "\n");
  (* to here *)
    result


let infer_tests : ((context * exp) * typ) list = [
  ((Ctx [], toExp valid_program_1), TInt);
  ((Ctx [], toExp valid_program_2), TInt);
  ((Ctx [], toExp valid_program_3), TInt);
  ((Ctx [], toExp valid_program_4), TInt);
  ((Ctx [], toExp valid_program_5), TInt);
  ((Ctx [], toExp valid_program_6), TInt);
  ((Ctx [], toExp valid_program_7), TInt);
  ((Ctx [], toExp valid_program_8), TInt);
  ((Ctx [], toExp valid_program_9), TInt);
  ((Ctx [], toExp valid_program_10), TInt);
]

let rec typeToString (t: typ) =
  match t with
  | TArrow   (a, b)   -> typeToString(a) ^ " -> " ^ typeToString(b)
  | TProduct (a::b::t)-> typeToString(a) ^ " * " ^ typeToString(TProduct (b::t))
  | TProduct (b::[])  -> typeToString(b)
  | TInt              -> "int"
  | TBool             -> "bool"
  | _                 -> ""

let rec printContext (ctx: (name * typ) list) : unit =
  match ctx with
  | [] -> Printf.printf "\n";
  | (n, t)::rest -> let t = typeToString t in Printf.printf "%s, %s; " n t; printContext rest

(* Q5  : Type an expression *)
(* Q7* : Implement the argument type inference
         For this question, move this function below the "unify". *)
let rec infer (ctx : context) (e : exp) : typ =
  match e with
  | Int _  -> TInt
  | Bool _ -> TBool
  | Var x  -> ctx_lookup ctx x

  | If (e, e1, e2)  ->
      let (e1T,e2T) = (infer ctx e1, infer ctx e2) in 
      if (infer ctx e = TBool) then
        if (typ_eq e1T e2T) then e1T
        else type_fail "If statement branches must have the same type"
      else type_fail "If statement guard must have type bool"
  | Primop (op, l)  ->
      begin match (op, l) with
        | (Equals,       [e1; e2])   -> 
            if (infer ctx e1 = TInt) && (infer ctx e2 = TInt) 
            then TBool else type_fail "Equals: Inputs must have type int"
        | (NotEquals,    [e1; e2])   -> 
            if (infer ctx e1 = TInt) && (infer ctx e2 = TInt) 
            then TBool else type_fail "NotEquals: Inputs must have type int"
        | (LessThan,     [e1; e2])   -> 
            if (infer ctx e1 = TInt) && (infer ctx e2 = TInt) 
            then TBool else type_fail "LessThan: Inputs must have type int"
        | (LessEqual,    [e1; e2])   -> 
            if (infer ctx e1 = TInt) && (infer ctx e2 = TInt) 
            then TBool else type_fail "LessEqual: Inputs must have type int"
        | (GreaterThan,  [e1; e2])   -> 
            if (infer ctx e1 = TInt) && (infer ctx e2 = TInt) 
            then TBool else type_fail "GreaterThan: Inputs must have type int"
        | (GreaterEqual, [e1; e2])   -> 
            if (infer ctx e1 = TInt) && (infer ctx e2 = TInt) 
            then TBool else type_fail "GreaterEqual: Inputs must have type int"
        | (Plus,         [e1; e2])   -> 
            if (infer ctx e1 = TInt) && (infer ctx e2 = TInt) 
            then TInt else type_fail "Plus: Inputs must have type int"
        | (Minus,        [e1; e2])   -> 
            if (infer ctx e1 = TInt) && (infer ctx e2 = TInt) 
            then TInt else type_fail "Minus: Inputs must have type int"
        | (Times,        [e1; e2])   -> 
            if (infer ctx e1 = TInt) && (infer ctx e2 = TInt) 
            then TInt else type_fail "Times: Inputs must have type int"
        | (Div,          [e1; e2])   -> 
            if (infer ctx e1 = TInt) && (infer ctx e2 = TInt) 
            then TInt else type_fail "Div: Inputs must have type int"
        | (And,          [e1; e2])   -> 
            if (infer ctx e1 = TBool) && (infer ctx e2 = TBool) 
            then TBool else type_fail "And: Inputs must have type bool"
        | (Or,           [e1; e2])   -> 
            if (infer ctx e1 = TBool) && (infer ctx e2 = TBool) 
            then TBool else type_fail "Or: Inputs must have type bool"
        | (Negate,       [e])        -> 
            if (infer ctx e = TInt) 
            then TInt else type_fail "Negate: Input must have type int"
        | _ -> type_fail "Incorrect number of arguments for primitive operation"
      end
  | Tuple (list)    -> TProduct (List.map (infer ctx) list)
  | Fn (x, t, e)    ->
      begin match t with
      | Some (tau) -> let tau' = infer (extend ctx (x, tau)) e in TArrow (tau, tau')
      | None -> type_fail "No type annotation given for function"
      end
  | Rec (f, t, e)   -> infer (extend ctx (f, t)) e
  | Apply (e1, e2)  -> let tau = infer ctx e2 in
      begin match infer ctx e1 with
      | TArrow (t,t') -> if (t = tau) then t' else type_fail "Function argument is of incorrect type"
      | _ -> type_fail "Expression is not a function, it cannot be applied"
      end
  | Anno (e, t)     -> if (infer ctx e) = t then t else type_fail "Annotation and expression type mismatch"
  | Let (ds, e)     -> let Ctx (gamma') = inferDecs ctx (Ctx []) ds in infer (extend_list ctx gamma') e
and inferDecs (gamma: context) (acc: context) (ds: dec list) : context =
  let inferDec (ctx: context) (d: dec) : context =
    match d with
    | Val (e, x)       -> Ctx [x, infer ctx e]
    | Valtuple (e, xs) -> 
        begin match (infer ctx e) with
        | TProduct (ts) -> (try Ctx (List.map2 (fun x t -> (x,t)) xs ts)
                           with Invalid_argument _ -> type_fail "Tuple length mismatch in Let binding")
        | _ ->  type_fail "Expression in Let binding must evaluate to a tuple"
        end
    | ByName (e, x)    -> Ctx [x, infer ctx e]
  in 
  match ds with
  | dec::rest -> let Ctx (gamma1) = inferDec gamma dec in 
                 inferDecs (extend_list gamma gamma1) (extend_list acc gamma1) rest
  | [] -> acc


let unify_tests : ((typ * typ) * unit) list = [
]

(* find the next function for Q5 *)
(* Q6  : Unify two types *)
let unify (ty1 : typ) (ty2 : typ) : unit = raise NotImplemented


(* Now you can play with the language that you've implemented! *)
let execute (s: string) : unit =
  match P.parse s with
  | Left s -> print_endline ("parsing failed: " ^ s)
  | Right e ->
      try
       (* first we type check the program *)
        ignore (infer (Ctx []) e);
        let result = eval e in
        print_endline ("program is evaluated to: " ^ Print.exp_to_string result)
      with
      | NotImplemented -> print_endline "code is not fully implemented"
      | Stuck s -> print_endline ("evaluation got stuck: " ^ s)
      | NotFound -> print_endline "variable lookup failed"
      | TypeError s -> print_endline ("type error: " ^ s)
      | e -> print_endline ("unknown failure: " ^ Printexc.to_string e)


(************************************************************
 *             Do not change these functions.               *
 *               They are needed for tests.                 *
 ************************************************************)
let list_to_string el_to_string l : string =
  List.fold_left
    begin fun acc el ->
      if acc = "" then
        el_to_string el
      else
        acc ^ "; " ^ el_to_string el
    end
    ""
    l
  |> fun str -> "[" ^ str ^ "]"

let run_test name f ts stringify : unit = 
  List.iteri
    begin fun idx (input, expected_output) ->
      try
        reset_ctr();
        let output = f input in
        if output <> expected_output then
          begin
            print_string (name ^ " test #" ^ string_of_int idx ^ " failed\n");
            print_string (stringify output ^ " <> " ^ stringify expected_output ^ "\n")
          end
      with
      | exn ->
          print_string (name ^ " test #" ^ string_of_int idx ^ " raised an exception:\n");
          print_string (Printexc.to_string exn ^ "\n")
    end
    ts

let run_free_vars_tests () : unit =
  run_test "free_vars" free_vars free_vars_tests (list_to_string (fun x -> x))

let run_unused_vars_tests () : unit =
  run_test "unused_vars" unused_vars unused_vars_tests (list_to_string (fun x -> x))

let run_subst_tests () : unit =
  run_test "subst" (fun (s, e) -> subst s e) subst_tests Print.exp_to_string

let run_eval_tests () : unit =
  run_test "eval" eval eval_tests Print.exp_to_string

let run_infer_tests () : unit =
  run_test "infer" (fun (ctx, e) -> infer ctx e) infer_tests Print.typ_to_string

let run_unify_tests () : unit =
  run_test "unify" (fun (ty1, ty2) -> unify ty1 ty2) unify_tests (fun () -> "()")

let run_all_tests () : unit =
  run_free_vars_tests ();
  run_unused_vars_tests ();
  run_subst_tests ();
  run_eval_tests ();
  run_infer_tests ();
  run_unify_tests ()

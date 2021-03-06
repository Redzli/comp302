exception NotImplemented
exception Error of string

(* Abstract syntax trees produced by parse *)
type exp =
  | Sum of exp * exp
  | Minus of exp * exp
  | Prod of exp * exp
  | Div of exp * exp
  | Int of int

(* Tokens produced by the lexer *)
type token = SEMICOLON | PLUS | SUB | TIMES | DIV | LPAREN | RPAREN | INT of int


(*------------------ Q1------------------ *)
let rec parseExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parseSExp
    toklist
    (fun toklist' exp -> match toklist' with
      | SEMICOLON :: toklist'' -> sc toklist'' exp
      | _ -> raise (Error "Expected a single semicolon"))

and parseSExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parsePExp
    toklist
    (fun toklist' exp -> match toklist' with
      | PLUS :: toklist'' -> parseSExp toklist'' (fun toklist''' exp' -> sc toklist''' (Sum(exp,exp')))
      | SUB :: toklist'' -> parseSExp toklist'' (fun toklist''' exp' -> sc toklist''' (Minus(exp,exp')))
      | _ -> sc toklist' exp)

and parsePExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parseAtom
    toklist
    (fun toklist' exp -> match toklist' with
      | TIMES :: toklist'' -> parsePExp toklist'' (fun toklist''' exp' -> sc toklist''' (Prod(exp,exp')))
      | DIV :: toklist'' -> parsePExp toklist'' (fun toklist''' exp' -> sc toklist''' (Div(exp,exp')))
      | _ -> sc toklist' exp)

and parseAtom (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  match toklist with
  | (INT n) :: toklist' -> sc toklist' (Int n)
  | LPAREN :: toklist' -> parseSExp toklist' (fun toklist'' exp -> match toklist'' with 
      | RPAREN :: t -> sc t exp
      | _ -> raise (Error "Expected a closing parenthesis"))
  | _ -> raise (Error "List must begin with INT or LPAREN")

(* parse : string -> exp *)
let parse string =
  parseExp
    (lex string)
    (fun s e -> match s with
       | [] -> e
       | _ -> raise (Error "Incomplete expression"))

(* eval : string -> int *)
let eval e = eval' (parse e)

(* ---------- Hamming Numbers ----------- *)

let rec merge (s1: 'a str) (s2: 'a str): 'a str = {
  hd = if (s1.hd < s2.hd) then s1.hd else s2.hd;
  tl = if (s1.hd < s2.hd) then delay (fun () -> merge (force s1.tl) s2)
  else if (s1.hd > s2.hd) then delay (fun () -> merge s1 (force s2.tl))
  else delay (fun () -> merge (force s1.tl) (force s2.tl));
}

let rec hamming_series: 'a str = {
  hd = 1;
  tl = Susp (fun () -> merge (times 5 hamming_series) (merge (times 2 hamming_series) (times 3 hamming_series)));
}
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
  raise NotImplemented

and parsePExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  raise NotImplemented

and parseAtom (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  raise NotImplemented

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

let rec merge (s1: 'a str) (s2: 'a str) : 'a str = 
  raise NotImplemented

let rec hamming_series = 
  raise NotImplemented

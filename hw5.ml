exception NotImplemented
exception Fail
(* The type of graphs. *)
type weight = int
            
type 'a graph = {
  nodes: 'a list;
  edges: ('a * 'a * weight) list
}


(* TODO: Write some tests for neighbours. Consider creating a graph first,
 and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
((arg1, arg1), (expected_output))
]
*)

(* We've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
  (({ nodes = ["A"; "B"; "C"; "D"; "E"];
      edges = [("A","B",10); ("A","C",3); ("B","D",7); ("D","A",12); ("D","C",5)];
    }, "A"),[("B",10); ("C",3)]); 
  (({ nodes = ["A"; "B"; "C"; "D"; "E"];
      edges = [("A","B",10); ("A","C",3); ("B","D",7); ("D","A",12); ("D","C",5)];
    }, "B"),[("D",7)]);
  (({ nodes = ["A"; "B"; "C"; "D"; "E"];
      edges = [("A","B",10); ("A","C",3); ("B","D",7); ("D","A",12); ("D","C",5)];
    }, "C"),[]);
  (({ nodes = ["A"; "B"; "C"; "D"; "E"];
      edges = [("A","B",10); ("A","C",3); ("B","D",7); ("D","A",12); ("D","C",5)];
    }, "D"),[("A",12); ("C",5)]);
  (({ nodes = ["A"; "B"; "C"; "D"; "E"];
      edges = [("A","B",10); ("A","C",3); ("B","D",7); ("D","A",12); ("D","C",5)];
    }, "E"),[]);
]

(* TODO: Implement neighbours. *)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list =
  let f (acc: ('a * weight) list) (edge: ('a * 'a * weight)): ('a * weight) list = 
    let (v1, v2, w) = edge in if (v1 = vertex) then (v2, w)::acc else acc
  in List.fold_left f [] g.edges

(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) = 
  let rec aux_node (node: 'a * weight) (visited : 'a list) (acc: weight): ('a list * weight) =
    let (v,w) = node in 
    if (List.mem v visited) then raise Fail
    else
      if (v = b) then (visited@[v], acc+w)    (*TODO*)
      else aux_list (neighbours g v) (visited@[v]) (acc+w);
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) (acc: weight) : ('a list * weight) =
    match nodes with
    | [] -> raise Fail
    | h::t -> try aux_node h visited acc with Fail -> aux_list t visited acc
  in
  aux_node (a,0) [] 0

(* TODO: Implement find_path'. *)
let find_path' (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node (node: 'a * weight) (visited : 'a list) fc sc : ('a list * weight)=
    raise NotImplemented
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) fc sc : ('a list * weight) =
    raise NotImplemented
  in
  raise NotImplemented


(* TODO: Implement find_all_paths *)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list = 
  let rec aux_node (node: 'a * weight) (visited : 'a list) : ('a list * weight) list =
    raise NotImplemented  
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) list =
    raise NotImplemented  
  in
  raise NotImplemented  


(* TODO: Implement find_shortest_path *)
let find_shortest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option = 
  raise NotImplemented    
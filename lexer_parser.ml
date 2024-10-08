module StringMap = Map.Make(String);; 
module IntMap = Map.Make(struct type t = int let compare = compare end);; 

exception NaN;;
exception InvalidToken;; 
exception Err;;

(* Converts a string to a list of chars *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [] 

(* Converts a list of chars to a string *)
let rec implode chars =
  let string_of_char c = String.make 1 c in
  match chars with
    [] -> ""
  | h::t ->  string_of_char h ^ (implode t)

let rec take k l =
  match k, l with
  | _, [] -> []
  | 0, _ -> []
  | n, h::t -> h :: take (n-1) t;;
                 
let rec drop k l =
  match k, l with
  | _, [] -> []
  | 0, _ -> l 
  | n, h::t -> drop (n-1) t;; 

let (^$) c s = s ^ Char.escaped c (* append *)
let ($^) c s = Char.escaped c ^ s (* prepend *) 

let is_pos_digit = function '1' .. '9' -> true | _ -> false
let is_nat_digit = function '0' .. '9' -> true | _ -> false
  

type token = 
  | NUM of float 
  | SUB
  | ADD
  | POW 
  | COS
  | FACT
  | SKIP
  | EPS
  | END;; 

type state = 
  | FAIL
  | ACCEPT

let token_map = StringMap.of_seq @@ List.to_seq [ 
    ("-", SUB);
    ("+", ADD);
    ("^", POW);
    ("cos", COS);
    ("!", FACT);
    (" ", SKIP);
  ];; 

let sign_map = StringMap.of_seq @@ List.to_seq [ 
    ("-", (~-.));
    ("+", (~+.));
  ];; 
    
    
(* S -> + | - *)
let is_sign s = StringMap.mem (Char.escaped s) sign_map
let get_sign s = StringMap.find (Char.escaped s) sign_map 
    
(* L -> B | BL *)
let is_lead s = 
  let rec aux sl =
    match sl with
    | [] -> false
    | [d] -> is_nat_digit d
    | h::t -> is_nat_digit h && aux t
  in 
  let sl = explode s in
  aux sl 
    
(* I -> B | AL *)
let is_int s =
  let rec aux sl =
    match sl with
    | [] -> false
    | [d] -> is_nat_digit d
    | h::t -> is_pos_digit h && is_lead (implode t)
  in
  let sl = explode s in
  aux sl

(* D -> 0.L | I.L *)
let is_dec s =
  let rec aux sl =
    match sl with
    | [] -> false
    | a::b::t ->
        if (a = '0' && b = '.') then is_lead (implode t)
        else 
          let rec aux' sl mem =
            match sl with 
            | [] -> false
            | h::t -> 
                if h = '.' then (is_int mem) && (is_lead (implode t))
                else aux' t (h ^$ mem)
          in aux' (a::b::t) ""
    | _ -> false
  in
  let sl = explode s in
  aux sl 

(* M -> I | D *)
let is_M s = is_dec s || is_int s
               
(* N -> M | MeSI *)
let is_N s =
  let rec aux sl mem =
    match sl with 
    | [] -> false
    | h::t -> 
        let aux' sl =
          match sl with
          | [] -> false
          | h::t -> (is_int (implode (h::t))) || ((is_sign h) && (is_int (implode t)))
        in
        if h = 'e' then (is_M mem) && (aux' t)
        else aux t (h ^$ mem)
  in 
  let sl = explode s in
  is_M s || aux sl ""
    
(* F -> SN | N *)
let is_F s =
  let aux sl =
    match sl with
    | [] -> false
    | h::t -> (is_sign h) && (is_N (implode t))
  in
  let sl = explode s in 
  is_N s || aux sl 

let get_F s = NUM (float_of_string s)
  
(* Checks if string maps to a token *)
let is_token s = StringMap.mem s token_map || is_F s

(* Get token mapped to symbol *)
let get_token s = 
  if is_F s then get_F s
  else StringMap.find s token_map

(* ignore spaces *)
let rec remove_skips syms =
  match syms with
  | [] -> []
  | h::t ->
      if h = SKIP then remove_skips t
      else h::remove_skips t
             
(*testing*)
let get_N s = NUM (float_of_string s)

(* Checks if string maps to a token *)
let is_token s = StringMap.mem s token_map || is_N s

(* Get token mapped to symbol *)
let get_token s = 
  if is_N s then get_N s
  else StringMap.find s token_map
(*end testing*) 

(*------------------LEXER-------------------*)

let lex s = 
  let lex' sl = 
    let sl' = ref sl in 
    let tks = ref [] in 
    while List.length !sl' > 0 do 
      let sym = ref "" in 
      let count = ref 0 in 
      let a_pos = ref (-1) in  (*last accepted token position in string*) 
      let q = ref FAIL in 
      while !count <= List.length !sl' do 
        match is_token !sym with 
        | true -> 
            a_pos := !count; 
            q := ACCEPT; 
            count := !count + 1; 
            sym := implode (take !count !sl'); 
        | false -> 
            count := !count + 1; 
            sym := implode (take !count !sl'); 
      done; 
      match !q with 
      | ACCEPT -> 
          let a_sym = implode (take !a_pos !sl') in 
          let tk = get_token a_sym in 
          tks := tk :: !tks; 
          sl' := drop (!a_pos) !sl';
          count := 0;
          a_pos := (-1);
      | FAIL -> raise InvalidToken; 
    done; 
    List.rev !tks 
  in 
  let sl = explode s in 
  remove_skips (lex' sl);;


(*testing*)
lex "cos+-^cos";;
lex "-10.2++3";;
lex "cos -10. 2 --3";;
  

(*------------------LR(1) PARSER-------------------*)

type non_terminal = Z | E | E' | T | T' | R | C | X | F
                     
type symbol =
    NT of non_terminal 
  | Te of token 
      
module PairMap = Map.Make(struct
    type t = int * symbol
    let compare (x1, s1) (x2, s2) =
      let c = compare x1 x2 in
      if c <> 0 then c else compare s1 s2
  end)      

module SymMap = Map.Make(struct type t = symbol let compare = compare end)    

let get_sym_keys map =
  SymMap.fold (fun key _ acc -> key :: acc) map []    

let get_int_keys map =
  IntMap.fold (fun key _ acc -> key :: acc) map []   
    
let get_pair_keys map = 
  PairMap.fold (fun key _ acc -> key :: acc) map [] 

type prod_rule =
    Production of symbol * symbol list
                    
let rec get_num_prod tks =
  match tks with 
  | [] -> []
  | NUM f::t -> Production (NT F, [Te (NUM f)]) :: 
                Production (NT F, [Te ADD; Te (NUM f)]) ::
                Production (NT F, [Te SUB; Te (NUM f)]) ::
                get_num_prod t
  | _::t -> get_num_prod t 

let grammar tks = [ 
  Production (NT Z, [NT E; Te END]);
  Production (NT E, [NT T; NT E']);
  Production (NT E', [Te ADD; NT T; NT E']);
  Production (NT E', [Te EPS]);
  Production (NT T, [NT R; NT T']);
  Production (NT T', [Te SUB; NT R; NT T']);
  Production (NT T', [Te EPS]);
  Production (NT R, [NT C]);
  Production (NT R, [NT C; Te POW; NT R]);
  Production (NT C, [NT X]);
  Production (NT C, [Te COS; NT C]);
  Production (NT X, [NT X; Te FACT]); 
  Production (NT X, [NT F])
] @ get_num_prod tks
  
(* Define types for ACTION and GOTO tables *)
type action =
  | SHIFT of int
  | REDUCE of int * symbol
  | ACCEPT
  | NONE

    
module ActionMap = Map.Make(struct
    type t = int * symbol
    let compare (x1, s1) (x2, s2) =
      let c = compare x1 x2 in
      if c <> 0 then c else compare s1 s2
  end)    
    
module GotoMap = Map.Make(struct
    type t = int * symbol
    let compare (x1, s1) (x2, s2) =
      let c = compare x1 x2 in
      if c <> 0 then c else compare s1 s2
  end) 
    
type lr1_item = {
  production : prod_rule;  
  dot_position : int;       
  lookahead : token option;  
} 

let rec get_prod_in_grammar nt grammar =
  match grammar with
  | [] -> []
  | Production (x, y) :: t -> 
      if nt = x then (Production (x, y)) :: get_prod_in_grammar nt t
      else get_prod_in_grammar nt t

          
(*generate lookahead*)
let rec gen_las next la grammar = 
  match next with
  | [] | [Te EPS] ->
      (match la with
       | Some t -> [t]
       | None -> raise Err
      )
  | h::t ->
      (match h with
       | Te te -> [te]
       | NT nt -> 
           let prods = get_prod_in_grammar (NT nt) grammar in 
           let rec aux prods =
             match prods with
             | [] -> []
             | Production (_, y)::xs -> (gen_las y la grammar) :: aux xs
           in List.flatten (aux prods)
      )
    
(*generate items*)
let rec gen_items next la grammar new_prods =
  match new_prods with
  | [] -> []
  | Production (nt, y)::t -> 
      let las = gen_las next la grammar in
      let rec aux las =
        match las with
        | [] -> []
        | x::xs ->
            {
              production = Production (nt, y);
              dot_position = 0;
              lookahead = Some x 
            } :: aux xs
      in aux las @ gen_items next la grammar t
  
(*Get symbol directly after the dot*)
let rec get_dot_sym dot_pos syms =
  try
    List.nth syms dot_pos
  with 
  | (Failure nth) -> Te END 

let rec closure items grammar = 
  let rec aux items =
    match items with
    | [] -> raise Err (*[]*)
    | h::t -> 
        match h.production with
        | Production (_, xs) ->
            let after_dot = get_dot_sym (h.dot_position) (xs) in
            let new_prods = List.filter (function
                | Production (l, r) when l = after_dot -> true 
                | _ -> false
              ) grammar in 
            gen_items (drop (h.dot_position+1) (xs)) (h.lookahead) grammar new_prods 
  in aux items
  
    
let set_closure items grammar =
  let rec aux seen items grammar =
    match items with
    | [] -> seen
    | h::t -> 
        if List.mem h seen then aux seen t grammar
        else
          let new_seen = h::seen in
          let new_items = closure (h::t) grammar in
          aux new_seen (new_items@items) grammar
  in aux [] items grammar 
    
(*--------------PRINTING FOR DEBUGGING----------------*) 
(* Convert non_terminal to string *)
let string_of_non_terminal = function
  | Z -> "Z"
  | E -> "E"
  | E' -> "E'"
  | T -> "T"
  | T' -> "T'"
  | R -> "R"
  | C -> "C"
  | X -> "X"
  | F -> "F"

(* Convert token to string *)
let string_of_token = function
  | NUM f -> Printf.sprintf "NUM(%f)" f
  | SUB -> "SUB"
  | ADD -> "ADD"
  | POW -> "POW"
  | COS -> "COS"
  | FACT -> "FACT"
  | SKIP -> "SKIP" 
  | EPS -> "EPS"
  | END -> "END"

(* Convert symbol to string *)
let string_of_symbol = function
  | NT nt -> string_of_non_terminal nt
  | Te t -> string_of_token t

(* Print a list of symbols *)
let print_symbol_list symbols =
  symbols
  |> List.map string_of_symbol
  |> String.concat ", "
  |> Printf.printf "[%s]\n" 

let rec print_int_list = function 
    [] -> ()
  | e::l -> print_int e ; print_string " " ; print_int_list l 
(*----------------------------------------*) 

(*adds item to map of states *)
let rec add_item_to_state item key dfa_states =
  if SymMap.mem key dfa_states then
    let vals = SymMap.find key dfa_states in
    if List.mem item vals then (dfa_states)
    else
      (dfa_states |> SymMap.add key (item :: vals))
  else
    (dfa_states |> SymMap.add key [item])    

(*Adds new items to new states from the current state after shifting*)
let get_next_sets set =
  let rec aux set dfa_states =
    match set with
    | [] -> dfa_states
    | h::t ->
        match h.production with
        | Production (_, xs) -> 
            let dot_sym = get_dot_sym (h.dot_position) xs in
            if dot_sym = Te END || dot_sym = Te EPS then aux t dfa_states 
            else
              let new_item = {production = h.production; dot_position = h.dot_position+1; lookahead = h.lookahead} in
              let new_dfa_states = add_item_to_state new_item dot_sym dfa_states in 
              aux t new_dfa_states
  in 
  let dfa_states = SymMap.empty in
  aux set dfa_states  

let rec remove_item set item =
  match set with
  | [] -> []
  | h::t -> 
      if h = item then t
      else h::(remove_item t item)

(*check if two lists contain the same elements*)
let rec is_list_same l1 l2 =
  match l1, l2 with 
  | [], [] -> true
  | [], _ | _, [] -> false
  | xs, y::ys -> 
      if List.mem y xs then is_list_same (remove_item xs y) ys
      else false 

(*check if set value already in numbered map of sets*)
let set_exists set num_sets = 
  let ids = get_int_keys num_sets in
  let rec aux set_ids set num_sets = 
    match set_ids with
    | [] -> None
    | h::t -> 
        let h_set = IntMap.find h num_sets in
        if is_list_same h_set set then (Some h)
        else aux t set num_sets
  in aux ids set num_sets

let make_transitions set_id sym_sets dfa_trans num_sets g = 
  let rec aux trans_sym set_id dfa_trans num_sets set_ids = 
    match trans_sym with
    | [] -> (num_sets, dfa_trans, set_ids)
    | h::t -> (*create numbered set based on length of num_sets keys and transition function entry*)
        let next_id = List.length (get_int_keys num_sets) in
        let set_clos = set_closure (SymMap.find h sym_sets) g in 
        (*if set_clos is already a set in num_sets*) 
        (*print_endline (string_of_int (List.length set_clos));*)
        match set_exists set_clos num_sets with
        | None -> 
            (*print_endline "new id";*)
            let new_num_sets = num_sets |> IntMap.add next_id set_clos in
            let new_dfa_trans = dfa_trans |> PairMap.add (set_id, h) next_id in
            aux t set_id new_dfa_trans new_num_sets (next_id::set_ids)
        | Some n ->
            (*print_endline "old id";*)
            let new_dfa_trans = dfa_trans |> PairMap.add (set_id, h) n in
            aux t set_id new_dfa_trans num_sets set_ids
  in
  let trans_sym = get_sym_keys sym_sets in
  aux trans_sym set_id dfa_trans num_sets []
    
(*returns map of id to set and map of transition functions*)
let get_trans_sets grammar init =
  let set_zero = set_closure [init] grammar in
  let num_sets = IntMap.empty in
  let num_sets = num_sets |> IntMap.add 0 set_zero in
  let set_ids = get_int_keys num_sets in
  let dfa_trans = PairMap.empty in
  let rec aux set_ids dfa_trans num_sets = 
    match set_ids with
    | [] -> (num_sets, dfa_trans)
    | h::t -> 
        let set = IntMap.find h num_sets in 
        let sym_sets = get_next_sets set in
        let new_num_sets, new_dfa_trans, next_set_ids = make_transitions h sym_sets dfa_trans num_sets grammar in
        aux (t@next_set_ids) new_dfa_trans new_num_sets
  in aux set_ids dfa_trans num_sets


(*construct action table*)
let get_action grammar init = 
  let num_sets, dfa_trans = get_trans_sets grammar init in
  let num_ids = get_int_keys num_sets in
  let action_tbl = ActionMap.empty in
  let rec aux num_ids dfa_trans num_sets action_tbl =
    match num_ids with
    | [] -> action_tbl
    | h::t -> (*set number id*)
        let set = IntMap.find h num_sets in 
        let rec aux' set action_tbl =
          match set with
          | [] -> action_tbl
          | x::xs -> (*x is the item (production, dot_position, lookhead)*)
              match x.production with
              | Production(nt, ys) -> 
                  let after_dot = get_dot_sym x.dot_position ys in
                  match after_dot with
                  | NT _ -> aux' xs action_tbl
                  | Te _ -> 
                      if ys = [Te EPS] then 
                        let new_action_tbl = action_tbl |> ActionMap.add (h, Te (Option.get x.lookahead)) (REDUCE(0, nt)) in 
                        aux' xs new_action_tbl
                      else
                        match after_dot with
                        | Te END -> 
                            (match x.lookahead with
                             | Some END -> 
                                 (if nt = NT Z then
                                    let new_action_tbl = action_tbl |> ActionMap.add (h, Te END) ACCEPT in aux' xs new_action_tbl
                                  else let new_action_tbl = action_tbl |> ActionMap.add (h, Te (Option.get x.lookahead)) (REDUCE(x.dot_position, nt)) in aux' xs new_action_tbl) 
                             | _ -> let new_action_tbl = action_tbl |> ActionMap.add (h, Te (Option.get x.lookahead)) (REDUCE(x.dot_position, nt)) in aux' xs new_action_tbl) 
                        | _ -> 
                            if PairMap.mem (h, after_dot) dfa_trans then 
                              let new_action_tbl = action_tbl |> ActionMap.add (h, after_dot) (SHIFT(PairMap.find (h, after_dot) dfa_trans)) in aux' xs new_action_tbl
                            else
                              let new_action_tbl = action_tbl |> ActionMap.add (h, after_dot) NONE in aux' xs new_action_tbl 
        in aux t dfa_trans num_sets (aux' set action_tbl)
  in aux num_ids dfa_trans num_sets action_tbl
    

(*construct goto table*)
let get_goto grammar init = 
  let num_sets, dfa_trans = get_trans_sets grammar init in
  let num_ids = get_int_keys num_sets in
  let goto_tbl = GotoMap.empty in
  let rec aux num_ids dfa_trans num_sets goto_tbl = 
    match num_ids with
    | [] -> goto_tbl
    | h::t -> (*set number id*)
        let set = IntMap.find h num_sets in
        let rec aux' set goto_tbl =
          match set with
          | [] -> goto_tbl
          | x::xs -> (*x is the item (production, dot_position, lookhead)*)
              match x.production with
              | Production(nt, ys) -> 
                  let after_dot = get_dot_sym x.dot_position ys in
                  match after_dot with
                  | NT _ ->
                      if PairMap.mem (h, after_dot) dfa_trans then
                        let new_goto_tbl = goto_tbl |> GotoMap.add (h, after_dot) (PairMap.find (h, after_dot) dfa_trans) in aux' xs new_goto_tbl
                      else
                        let new_goto_tbl = goto_tbl |> GotoMap.add (h, after_dot) (-1) in aux' xs new_goto_tbl
                  | Te _ ->
                      aux' xs goto_tbl
        in aux t dfa_trans num_sets (aux' set goto_tbl)
  in aux num_ids dfa_trans num_sets goto_tbl 

let rec get_nums syms =
  match syms with
  | [] -> []
  | h::t -> 
      match h with
      | NUM n -> h::(get_nums t)
      | _ -> get_nums t    
               
let rec to_sym_list syms =
  match syms with
  | [] -> [Te END]
  | h::t -> (Te h)::(to_sym_list t)
               
let base_item = { 
  production = Production (NT Z, [NT E; Te END]);
  dot_position = 0;
  lookahead = Some END;
}

(*TODO: generate parse tree*) 

type 'a tree = Br of 'a * 'a tree list

let make_branch sym stack n = Br (sym, take n stack)                       

let rec string_of_tree (Br (value, children)) indent =
  let prefix = String.make indent ' ' in
  let children_str = List.map (fun child -> string_of_tree child (indent + 2)) children in
  prefix ^ (string_of_symbol value) ^ "\n" ^ (String.concat "" children_str) 
  
let print_tree t = print_string (string_of_tree (List.nth t 0) 0)       

let gen_parse_tree s = 
  let init = { 
    production = Production (NT Z, [NT E; Te END]);
    dot_position = 0;
    lookahead = Some END;
  } in
  let syms = lex s in
  let g = grammar (get_nums syms) in
  let sym_list = to_sym_list syms in
  let action_tbl = get_action g init in
  let goto_tbl = get_goto g init in 
  let rec aux state_stack sym_stack syms tree_stack =
    match state_stack, sym_stack, syms with 
    | x::xs, _, z::zs ->
        print_string "state stack: ";
        print_int_list state_stack;
        print_endline "";
        print_string "symbol stack: ";
        print_symbol_list sym_stack;
        print_string "next symbol: ";
        print_endline (string_of_symbol z); 
        let act = ActionMap.find (x, z) action_tbl in
        let aux' = 
          match act with
          | SHIFT n ->
              print_endline "SHIFT\n"; 
              aux (n::state_stack) (z::sym_stack) zs ((Br (z, []))::tree_stack)
          | REDUCE(m, nt) ->
              print_endline ("REDUCE "^(string_of_int m)^"\n"); 
              (match drop m state_stack with
               | st::sts ->
                   let goto_state = GotoMap.find (st, nt) goto_tbl in 
                   aux (goto_state::(st::sts)) (nt::(drop m sym_stack)) syms (Br (nt, take m tree_stack)::(drop m tree_stack))
               | _ -> raise Err)
          | ACCEPT -> 
              print_endline "ACCEPT\n"; 
              print_endline "Parse tree:";
              print_tree tree_stack;
          | _ -> raise Err
        in aux' 
    | _, _, _ -> raise Err
  in aux [0] [] sym_list []

open Bitstream
open Combinational

(* a map with strings as the keys *)
module StringMap = Map.Make(String)
type 'a map = 'a StringMap.t

module StringSet = Set.Make(String)
type set = StringSet.t

(* since we internally represent inputs and outputs as registers, we need a
 * flag to specify their type *)
type reg_type =
  | Rising | Falling | Input | Output

type register_input =
  | User_input | AST of comb

(* a digital state component *)
type register = {
  reg_type : reg_type;
  length : int;
  value : bitstream;
  next : register_input;
}

type subcircuit = {
  ast : comb;
  length : int;
  args : (id * int) list;
}

(* a circuit component is either a register or a subcircuit *)
type component =
  | Register of register | Subcirc of subcircuit

(* a type to represent the state of a circuit *)
type circuit = {
  comps : component map;
  clock : bool;
}

module type CircuitSimulator = sig
  val evaluate : circuit -> comb -> bitstream
  val step : circuit -> circuit
  val step_n : circuit -> int -> circuit
  val change_input : circuit -> id -> bitstream -> circuit
end

module type StaticAnalyzer = sig
  type error_log
  val validate : circuit -> error_log
  val valid : error_log -> bool
  val format_log : Format.formatter -> error_log -> unit
end

module type CircuitFormatter = sig
  type formatted_circuit
  type display_node

  type display_let

  type display_register

  type display_reg_type

  type node
  val format : circuit -> formatted_circuit
  val format_format_circuit : Format.formatter -> formatted_circuit -> unit
end

let make_register length logic reg_type =
  Register {
    reg_type = reg_type;
    length = length;
    value = zeros length;
    next = logic;
  }

let rising_register length logic =
  make_register length (AST logic) Rising

let falling_register length logic =
  make_register length (AST logic) Falling

let input length =
  make_register length User_input Input

let output length logic =
  make_register length (AST logic) Output

let subcircuit logic length args =
  Subcirc {
    ast = logic;
    length = length;
    args = args;
  }

let circuit comps =
  {
    comps = comps;
    clock = false;
  }

let circuit_from_list l =
  let map_of_assoclist =
    List.fold_left (fun acc (k,v) -> StringMap.add k v acc) StringMap.empty in
  l |> map_of_assoclist |> circuit

let format_register_input f input =
  match input with
  | User_input -> Format.fprintf f "User Input"
  | AST c -> Format.fprintf f "%a" (format_logic) c

let rec format_args f args =
  match args with
  | [] -> ()
  | h::[] -> Format.fprintf f "%s" (fst h)
  | h::t -> Format.fprintf f "%s, %a" (fst h) (format_args) t

let format_comp f comp =
  match comp with
  | Register reg ->
    Format.fprintf f "%s\nValue: %a\nNext: %a"
      (match reg.reg_type with
       | Rising -> "Rising Register"
       | Falling -> "Falling Register"
       | Input -> "Input"
       | Output -> "Output")
      (format_bitstream) reg.value
      (format_register_input) reg.next
  | Subcirc s ->
    Format.fprintf f "(%a) -> %a" (format_args) s.args (format_logic) s.ast

let format_circuit f circ =
  Format.fprintf f "Clock: %s\n\n" (if circ.clock then "1" else "0");
  StringMap.iter
    (fun id comp -> Format.fprintf f ("%s =\n%a\n\n") id (format_comp) comp)
    circ.comps

(* [is_subcirc comp] is true if [comp] is a subcircuit, false otherwise *)
let is_subcirc _ = function
  | Subcirc _ -> true
  | _ -> false

(* [is_reg_type t comp] is true if [comp] is a register of type [t], false
 * otherwise *)
let is_reg_type t _ = function
  | Register r -> r.reg_type = t
  | _ -> false

(************************ eval ***********************)

module Simulator : CircuitSimulator = struct
  let rec eval_gates bin_op bin_not g b1 b2 =
    match g with
    | And -> bin_op and_bits b1 b2
    | Or -> bin_op or_bits b1 b2
    | Xor -> bin_op xor_bits b1 b2
    | Nand -> bin_not (bin_op and_bits b1 b2)
    | Nor -> bin_not (bin_op or_bits b1 b2)
    | Nxor -> bin_not (bin_op xor_bits b1 b2)

  let rec eval_reduce g b1 =
    match g with
    | And -> reduce and_bits b1
    | Or -> reduce or_bits b1
    | Xor -> reduce xor_bits b1
    | Nand -> bitwise_not (reduce and_bits b1)
    | Nor ->  bitwise_not (reduce or_bits b1)
    | Nxor -> bitwise_not (reduce xor_bits b1)

  let eval_neg n b1 =
    match n with
    | Neg_bitwise -> bitwise_not b1
    | Neg_logical -> logical_not b1
    | Neg_arithmetic -> negate b1

  let eval_comp comp b1 b2 =
    match comp with
     | Lt -> less_than b1 b2
     | Gt -> greater_than b1 b2
     | Eq -> equals b1 b2
     | Lte -> logical_binop or_bits (less_than b1 b2) (equals b1 b2)
     | Gte -> logical_binop or_bits (greater_than b1 b2) (equals b1 b2)
     | Neq -> logical_not (equals b1 b2)

  let eval_arith arth b1 b2 =
    match arth with
    | Add -> add b1 b2
    | Subtract -> subtract b1 b2
    | Sll -> shift_left b1 b2
    | Srl -> shift_right_logical b1 b2
    | Sra -> shift_right_arithmetic b1 b2

  let subcirc_len_check b len =
    if length b < len then (zero_extend len b)
    else if length b > len then substream b 0 (len - 1)
    else b

  let rec eval_hlpr circ comb env =
     match comb with
    | Const b -> b
    | Var id -> List.assoc id env
    | Sub_seq (n1,n2,c) -> substream (eval_hlpr circ c env) n1 n2
    | Nth (i,c) -> nth (eval_hlpr circ c env) i
    | Gate (g,c1,c2) -> let b1 = (eval_hlpr circ c1 env) in
                        let b2 = (eval_hlpr circ c2 env) in
                        eval_gates bitwise_binop bitwise_not g b1 b2
    | Logical (g,c1,c2) -> let b1 = (eval_hlpr circ c1 env) in
                        let b2 = (eval_hlpr circ c2 env) in
                        eval_gates logical_binop logical_not g b1 b2
    | Reduce (g,c) -> let b1 = (eval_hlpr circ c env) in
                      eval_reduce g b1
    | Neg (n,c) -> let b1 = (eval_hlpr circ c env) in eval_neg n b1
    | Comp (comp,c1,c2) -> let b1 = (eval_hlpr circ c1 env) in
                        let b2 = (eval_hlpr circ c2 env) in
                        eval_comp comp b1 b2
    | Arith (arth,c1,c2) -> let b1 = (eval_hlpr circ c1 env) in
                        let b2 = (eval_hlpr circ c2 env) in
                        eval_arith arth b1 b2
    | Concat (clst) -> List.fold_left
                      (fun acc c ->
                        concat (eval_hlpr circ c env) acc) (create []) clst
    | Mux2 (c1,c2,c3) -> let s = (eval_hlpr circ c1 env) in
                          if is_zero s
                          then eval_hlpr circ c3 env
                          else eval_hlpr circ c2 env
    | Apply (id,clst) -> let subcirc = StringMap.find id circ.comps in
                          let Subcirc s = subcirc in
                          let (nv, comb1) = eval_apply subcirc circ clst env in
                          subcirc_len_check (eval_hlpr circ comb1 nv) s.length
    | Let (id,c1,c2) -> let b1 = (eval_hlpr circ c1 env) in
                        if List.mem_assoc id env then
                        failwith "Cannot use variable twice" else
                        let nv = (id, b1)::env in eval_hlpr circ c2 nv

  and

      eval_apply subcirc circ clst env = (* returns (new_environment, comb) *)
        match subcirc with
        | Subcirc s -> let nv = eval_apply_hlpr s.args
                                   clst env circ in (nv, s.ast)
        | _ -> failwith "incorrect sub circuit application"

  and
      eval_apply_hlpr ids clst env circ =
         match (ids, clst) with
        | ([], []) -> env
        | (i::is,c::cs) -> let b =
                          subcirc_len_check (eval_hlpr circ c env) (snd i) in
                          (fst i, b)::(eval_apply_hlpr is cs env circ)
        | _ -> failwith "incorrect sub circuit application"

  let rec evaluate circ comb =
    let env = StringMap.fold
    (fun k v acc ->
      match v with
      | Register r -> (k, r.value)::acc
      | _ -> acc)
    circ.comps [] in
    (* env is a assoc list of RegID: bitstream ex: "A": 101011 *)
     eval_hlpr circ comb env

  (************************ eval ***********************)

  (* [register_len_check r new_val] zero eztends new_val or truncates new_val
      to be the same length as r *)
  let register_len_check (r:register) new_val =
    if (length new_val) < r.length
    then (zero_extend r.length new_val)
    else if (length new_val) > r.length then substream new_val 0 (r.length - 1)
    else new_val

  (*[eval_regs r circ] evaluates the rising / falling registers *)
  let eval_regs r circ =
    match r.next with
    | User_input -> Register r
    | AST comb -> if (r.reg_type = Falling && circ.clock)
                  || (r.reg_type = Rising && not circ.clock)
                  then let new_val = register_len_check r (evaluate circ comb) in
                  Register {r with value = new_val}
                else Register r

  let update_rising_falling circ c =
    match c with
      | Register r -> eval_regs r circ
      | _ -> c

  let update_outputs circ c =
    match c with
      | Register r -> (match (r.next, r.reg_type) with
                      |(AST comb, Output) -> let new_val =
                                        register_len_check r
                                        (evaluate circ comb)
                                            in
                                        Register {r with value = new_val}
                      | _ -> c)
      | _ -> c


  let step circ =
    let new_comps = StringMap.map (update_rising_falling circ) circ.comps in
    let new_circ = {comps = new_comps; clock = not circ.clock} in
    let comps_new = StringMap.map (update_outputs new_circ) new_circ.comps in
    {comps = comps_new; clock = new_circ.clock}

  let rec step_n circ n =
    match n with
    | 0 -> circ
    | i -> let new_circ = step circ in step_n new_circ (i - 1)

  let change_input circ id value =
    let r = match StringMap.find id circ.comps with
      | Register reg -> reg
      | _ -> failwith "tried to change the value of a subcircuit" in
    let new_comps =
      StringMap.add id (Register {r with value = value}) circ.comps in
    let new_circ = {comps = new_comps; clock = circ.clock} in
    let comps_new = StringMap.map (update_outputs new_circ) new_circ.comps in
    {comps = comps_new; clock = new_circ.clock}
end

module Analyzer : StaticAnalyzer = struct
  (* an error log is a monadic data type containing a circuit and a list of
   * string descriptions of the errors in it *)
  type error_log = circuit * string list

  exception Found_recursion of string list

  (* monadic return *)
  let make_log circ =
    (circ,[])

  (* monadic bind *)
  let (|>>) log f =
    let new_log = f (fst log) in
    (fst new_log, (snd new_log) @ (snd log))

  (* [ast_length comps env ast] is the length of [ast] evaluated in environment
   * [env] or [None] if [ast] is invalid *)
  let rec ast_length (comps:component map) env = function
    | Const c -> Some (length c)
    | Var v -> (try (StringMap.find v env) with Not_found -> None)
    | Sub_seq (n1,n2,_) -> if n1 <= n2 then Some (n2 - n1 + 1) else None
    | Nth _ | Reduce _ -> Some 1
    | Gate (_,c1,c2) | Logical (_,c1,c2) | Comp (_,c1,c2)
    | Arith (_,c1,c2) | Mux2 (_,c1,c2) ->
      (match ast_length comps env c1 with
       | Some l1 -> (match ast_length comps env c2 with
           | Some l2 -> Some (l1 + l2)
           | None -> None)
       | None -> None)
    | Neg (_,c) -> ast_length comps env c
    | Concat cs ->
      let ls = List.map (ast_length comps env) cs in
      List.fold_left (fun acc -> function
          | None -> None
          | Some l1 -> match acc with
            | None -> None
            | Some l2 -> Some (l1 + l2)) (Some 0) ls
    | Apply (f,_) ->
      (match (try Some (StringMap.find f comps) with Not_found -> None) with
      | None -> None
      | Some comp -> (match comp with
        | Subcirc s -> Some s.length
        | _ -> None))
    | Let (x,c1,c2) ->
      let new_env = StringMap.add x (ast_length comps env c1) env in
      ast_length comps new_env c2

  (* [detect_ast_errors comps env id ast] recursively traverses [ast] detecting
   * and logging errors in the context of overall component map [comps] and
   * environment [env] where [env] is a map from bound variables to their length
   *)
  let rec detect_ast_errors (comps:component map) env id ast =
    let template = Printf.sprintf "Error in definition for %s:\n" id in
    match ast with
    | Const _ -> []
    | Var v ->
      if StringMap.mem v env
      then []
      else [Printf.sprintf "%sUnbound variable %s" template v]
    | Sub_seq (n1,n2,c) ->
      let warning =
        if n1 > n2
        then [Printf.sprintf
                "%sArray access [%i - %i] is in the wrong order" template n1 n2]
        else
          match ast_length comps env c with
          | Some l ->
            if n2 >= l
            then [Printf.sprintf
                 "%sArray access [%i - %i] is out of bounds" template n1 n2]
            else []
          | _ -> [] in
      warning @ (detect_ast_errors comps env id c)
    | Nth (n,c) ->
      let warning =
        match ast_length comps env c with
        | Some l ->
          if n >= l
          then [Printf.sprintf
                  "%sArray access [%i] is out of bounds" template n]
          else []
        | _ -> [] in
      warning @ (detect_ast_errors comps env id c)
    | Gate (_,c1,c2) | Logical (_,c1,c2) | Comp (_,c1,c2) | Arith (_,c1,c2) ->
      (detect_ast_errors comps env id c1) @ (detect_ast_errors comps env id c2)
    | Reduce (_,c) | Neg (_,c) -> detect_ast_errors comps env id c
    | Concat cs -> cs |> (List.map (detect_ast_errors comps env id))
                   |> (List.fold_left (@) [])
    | Mux2 (c1,c2,c3) ->
      (detect_ast_errors comps env id c1) @
      (detect_ast_errors comps env id c2) @
      (detect_ast_errors comps env id c3)
    | Apply (f,cs) ->
      let warning =
        if not (StringMap.mem f comps)
        then  [Printf.sprintf "%sUndefined subcircuit %s" template f]
        else match StringMap.find f comps with
          | Register _ -> [Printf.sprintf "%s%s is not a subcircuit" template f]
          | Subcirc s ->
            let expected = List.length s.args in
            let found = List.length cs in
            if expected <> found
            then [Printf.sprintf
                    "%sExpected %i %s to subcircuit %s but found %i"
                    template expected
                    (if expected = 1 then "input" else "inputs") f found]
            else [] in
      let arg_warnings = cs |> (List.map (detect_ast_errors comps env id))
                         |> (List.fold_left (@) []) in
      warning @ arg_warnings
    | Let (x,c1,c2) ->
      let warning =
        if StringMap.mem x comps
        then [Printf.sprintf "%sLocal variable %s shadows definition" template x]
        else [] in
      let new_env = StringMap.add x (ast_length comps env c1) env in
      warning @
      (detect_ast_errors comps new_env id c1) @
      (detect_ast_errors comps new_env id c1)

  (* [detect_comp_errors comps id comp] detects and logs errors in the AST of
   * [comp] given the context of overall component map [comps] *)
  let detect_comp_errors comps id comp =
    let bound = comps |> (StringMap.filter
                (fun k v -> not (is_reg_type Output k v || is_subcirc k v))) in
    let env = StringMap.fold
        (fun k v acc -> StringMap.add k
            (match v with
             | Register r -> Some r.length
             | _ -> failwith "impossible" ) acc) bound StringMap.empty in
    match comp with
    | Register r ->
      (match r.next with
       | User_input -> []
       | AST ast -> detect_ast_errors comps env id ast)
    | Subcirc s ->
      let ids = List.map (fst) s.args in
      let binding_warnings =
        ids |>
        (List.map (fun arg ->
            if StringMap.mem arg comps
            then Some (Printf.sprintf
           "Error in definition for %s:\nArgument %s shadows definition" id arg)
            else None))
        |> (List.filter (fun w -> w <> None))
        |> (List.map (function Some c -> c | None -> failwith "impossible")) in
      let fun_env =
        List.fold_left
          (fun acc (id,l) -> StringMap.add id (Some l) acc) env s.args in
      binding_warnings @ (detect_ast_errors comps fun_env id s.ast)

  (* [detect_variable_errors circ] detects and logs the following errors:
   * - binding a local variable that shadows a register name
   * - referring to the value of an output
   * - using an unbound variable
   * - accessing a substream with invalid indices
   * - applying a subcircuit with the wrong number of arguments *)
  let detect_variable_errors circ =
    let warnings_map =
      StringMap.mapi
        (detect_comp_errors circ.comps) circ.comps in
    let warnings_list =
      StringMap.fold (fun _ v acc -> v @ acc) warnings_map [] in
    (circ,warnings_list)

  (* [contains acc ast] is a list of the ids of the subcircuits contained
   * within [ast] *)
  let rec contains = function
    | Const _ | Var _ -> []
    | Sub_seq (_,_,c) | Nth (_,c) | Reduce (_,c) | Neg (_,c) ->
      contains c
    | Gate (_,c1,c2) | Logical (_,c1,c2) | Comp (_,c1,c2)
    | Arith (_,c1,c2) | Let (_,c1,c2) ->
      (contains c1) @ (contains c2)
    | Mux2 (c1,c2,c3) ->
      (contains c1) @ (contains c2) @ (contains c3)
    | Concat cs ->
      cs |> (List.map (contains)) |> (List.fold_left (@) [])
    | Apply (f,cs) ->
      f::(cs |> (List.map (contains)) |> (List.fold_left (@) []))

(* [detect_cycles graph] performs depth first search on directed graph [graph]
 * and raises [Found_recursion path] if it encouters a cycle with path [path]
 * It ignores edges pointing to nodes that do not exist *)
  let detect_cycles graph =
    let rec dfs_helper path visited node =
      if List.mem node path then raise (Found_recursion (node::path)) else
      if StringSet.mem node visited then visited else
      if not (StringMap.mem node graph) then (StringSet.add node visited) else
        let new_path = node::path in
        let new_visited = StringSet.add node visited in
        let edges = StringMap.find node graph in
        List.fold_left
          (fun acc edge -> dfs_helper new_path acc edge) new_visited edges in
    let nodes = graph |> StringMap.bindings |> (List.map (fst)) in
    List.fold_left
      (fun acc node -> dfs_helper [] acc node) StringSet.empty nodes

  (* [make_graph circ] constructs a directed graph from [circ] where each node
   * is a subcircuit and an edge is a contains relation *)
  let make_graph circ =
    circ.comps |> (StringMap.filter (is_subcirc))
    |> (StringMap.map
          (function | Subcirc s -> s.ast
                    | _ -> failwith "impossible"))
    |> (StringMap.map (contains))

  (* [format_path_warning path] is a string representation of a recursion error
   * with path [path] *)
  let format_path_warning path =
    let rec format_path_helper _ = function
      | [] -> ""
      | h::[] -> Printf.sprintf "%s" h
      | h::t -> Printf.sprintf "%s contains %a" h (format_path_helper) t in
    Printf.sprintf
      "Recursion Detected:\n%a" (format_path_helper) (List.rev path)

  (* [detect_recursion circ] detects and logs any potentially recursive function
   * calls. These are not a valid construct in hardware implementation *)
  let rec detect_recursion (circ:circuit) : error_log =
    let g = make_graph circ in
    let warnings =
      try ignore (detect_cycles g); []
      with Found_recursion path -> [format_path_warning path] in
    (circ,warnings)

  (* validate pipes the circuit through several error checking functions *)
  let validate circ =
    circ |>
    make_log |>>
    detect_variable_errors |>>
    detect_recursion

  let valid (_,log) =
    List.length log = 0

  let format_log f (_,log) =
    let rec format_list f2 =
      function
      | [] -> ()
      | h::[] -> Format.fprintf f2 "%s\n" h
      | h::t -> Format.fprintf f2 "%s\n\n%a" h (format_list) t in
    let l = List.length log in
    if l = 0 then Format.fprintf f "No errors were detected\n" else
      Format.fprintf f "%i %s detected\n\n%a"
        l (if l = 1 then "error was" else "errors were") (format_list) log

end

module Formatter : CircuitFormatter = struct

  type comb_id =
    | Id_Const     of int * bitstream
    | Id_Var       of int * id
    | Id_Sub_seq   of int * int * int * comb_id
    | Id_Nth       of int * int * comb_id
    | Id_Gate      of int * gate * comb_id * comb_id
    | Id_Logical   of int * gate * comb_id * comb_id
    | Id_Reduce    of int * gate * comb_id
    | Id_Neg       of int * negation * comb_id
    | Id_Comp      of int * comparison * comb_id * comb_id
    | Id_Arith     of int * arithmetic * comb_id * comb_id
    | Id_Concat    of int * comb_id list
    | Id_Mux2      of int * comb_id * comb_id * comb_id
    | Id_Apply     of int * id * comb_id list
    | Id_Let       of int * id * comb_id * comb_id


  let new_id = ref 0

  (** generate an unused type variable *)
  let newvar =
    new_id := 1 + !(new_id);
    !new_id

  let attach_ids ast =
    let rec id_helper ast =
      match ast with
      | Const b -> Id_Const (newvar, b)
      | Var v -> Id_Var (newvar, v)
      | Sub_seq(i1, i2, comb) -> Id_Sub_seq (newvar, i1, i2, id_helper comb)
      | Nth (n, comb) -> Id_Nth (newvar, n, id_helper comb)
      | Gate (g, c1, c2) ->  Id_Gate (newvar, g, id_helper c1, id_helper c2)
      | Logical(l, c1, c2) -> Id_Logical(newvar, l, id_helper c1, id_helper c2)
      | Reduce (g, comb) -> Id_Reduce (newvar, g, id_helper comb)
      | Neg (n, comb) -> Id_Neg (newvar, n, id_helper comb)
      | Comp(c, c1, c2) -> Id_Comp (newvar, c, id_helper c1, id_helper c2)
      | Arith (o, c1, c2) -> Id_Arith (newvar, o, id_helper c1, id_helper c2)
      | Concat (c_list) -> Id_Concat (newvar, (List.map (function x -> id_helper x) c_list))
      | Mux2 (c1, c2, c3) -> Id_Mux2 (newvar, id_helper c1, id_helper c2, id_helper c3)
      | Apply (id, c_list) -> Id_Apply (newvar, id, (List.map (function x -> id_helper x) c_list))
      | Let (id, c1, c2) -> Id_Let (newvar, id, id_helper c1, id_helper c2)
    in id_helper ast

  let get_all_registers circ =
  let reg =
    (StringMap.filter (fun k v -> match v with |Register _ -> true | _ -> false) circ.comps)
    in
  (StringMap.map
    (fun v ->
      match v with
      | Register r -> r
      | _ -> failwith "invalid map")
    reg)


  let id_comp = fun x y ->  0

  let list_dependencies ast reg_list =
    let rec dependency_helper ast dep =
      match ast with
      | Id_Const (_, b) -> dep
      | Id_Var (_, v) -> if (StringMap.mem v reg_list) then v::dep else dep
      | Id_Sub_seq (_,_, _, comb) -> (dependency_helper comb dep)
      | Id_Nth (_,_, comb) -> (dependency_helper comb dep)
      | Id_Gate (_,_, c1, c2) -> (dependency_helper c1 dep)@(dependency_helper c2 dep)
      | Id_Logical(_,_, c1, c2) -> (dependency_helper c1 dep)@(dependency_helper c2 dep)
      | Id_Reduce (_,_, comb) -> (dependency_helper comb dep)
      | Id_Neg (_,_, comb) -> (dependency_helper comb dep)
      | Id_Comp(_,_, c1, c2) -> (dependency_helper c1 dep)@(dependency_helper c2 dep)
      | Id_Arith (_,_, c1, c2) -> (dependency_helper c1 dep)@(dependency_helper c2 dep)
      | Id_Concat (_,c_list) -> List.fold_left
        (fun acc c -> acc@(dependency_helper c acc)) dep c_list
      | Id_Mux2 (_,c1, c2, c3) ->
        (dependency_helper c1 dep)@(dependency_helper c2 dep)@(dependency_helper c3 dep)
      | Id_Apply (_,_, c_list) -> List.fold_left
        (fun acc c -> acc@(dependency_helper c acc)) dep c_list
      | Id_Let (_,_, c1, c2) -> (dependency_helper c1 dep)@(dependency_helper c2 dep)
      in List.sort_uniq id_comp (dependency_helper ast [])

  let no_inputs reg_list =
  StringMap.filter
  (fun k v -> match v.reg_type with |Input -> false | _ -> true) reg_list

  let find_inputs reg_list =
  StringMap.filter
  (fun k v -> match v.reg_type with |Input -> true | _ -> false) reg_list

  let no_outputs reg_list =
  StringMap.filter
  (fun k v -> match v.reg_type with |Output -> false | _ -> true) reg_list

  let find_outputs reg_list =
  StringMap.filter
  (fun k v -> match v.reg_type with |Output -> true | _ -> false) reg_list

  (* type formatted_circuit = register StringMap.t list *)

  let assign_columns circ =
    let reg = get_all_registers circ in
    let inputs = find_inputs reg in
    let outputs = find_outputs reg in
    let asts = (no_outputs (no_inputs reg)) in
    let list_dep_of_register r =
      (match r.reg_type with
      | Rising | Falling -> (
        match r.next with
        | AST ast -> list_dependencies (attach_ids ast) reg
        | _ -> []
      )
      | _ -> []) in
    let reg_deps = (StringMap.map (fun v -> list_dep_of_register v) (no_inputs reg)) in
    let rec dep_helper not_done d cols =
      (match (StringMap.is_empty not_done) with
      | true -> cols
      | false ->
        let resolved k v =
          (List.for_all (fun x -> StringMap.mem x d) (StringMap.find k reg_deps)) in
        let new_col = StringMap.filter resolved not_done in
        let new_done = StringMap.union (fun k v1 v2 -> Some v2) d new_col in
        let new_not_done = StringMap.filter (fun k v -> not (StringMap.mem k new_done)) not_done in

        dep_helper new_not_done new_done (new_col::cols))

    in
      if (StringMap.is_empty outputs)
      then (List.rev ((dep_helper asts inputs [inputs])))
      else List.rev (outputs::(dep_helper asts inputs [inputs]))


  let get_ids ast =
  match ast with
  | Id_Const (id, _ ) -> id
  | Id_Var (id, _ ) -> id
  | Id_Sub_seq(id, _, _, _ ) -> id
  | Id_Nth (id, _, _ ) -> id
  | Id_Gate (id, _ , _, _ ) -> id
  | Id_Logical(id, _, _, _ ) -> id
  | Id_Reduce (id, _, _ ) -> id
  | Id_Neg (id, _, _ ) -> id
  | Id_Comp(id, _, _, _ ) -> id
  | Id_Arith (id, _, _, _ ) -> id
  | Id_Concat (id, _ ) -> id
  | Id_Mux2 (id, _, _, _ ) -> id
  | Id_Apply (id, _, _ ) -> id
  | Id_Let (id, _, _, _ ) -> id

  type node =  B of gate | L of gate | A of arithmetic
  | N of negation | C of comparison | Sub of int*int | Nth of int | Subcirc of id |
  Red of gate | Concat of int list | Mux of int *int * int | Const of bitstream | Apply of id * int list

  type display_info = {
    y_coord : float;
    id : int;
    node : node;
    parents : int list;
  }

  let fot (x, _, _) = x

  (* let tree_to_list ast reg_id reg_list =
    (* ast:comb - what we are analyzing
     * parents : [int] - the parents of the current node
     * lets : Map: string -> ([int], comb) - a map from each variable name to its
     *        parents as well as its combinational logic
     * reg_list : the reg map we throw around everywhere
     * reg_parents : Map id -> [int]*)
    let rec list_helper ast parents lets reg_list reg_parents =
      match ast with
      | Id_Const (id, b) ->
        ([{y_coord=0.; id=id; node = Const b; parents = parents}], lets, reg_parents)
      | Id_Var (id, v) ->
        let new_reg_parents =
          if (StringMap.mem v reg_list)
          then
            if (StringMap.mem v reg_parents)
            then StringMap.add v (parents@(StringMap.find v reg_parents)) reg_parents
            else StringMap.add v parents reg_parents
          else reg_parents in
        let new_lets =
          if (StringMap.mem v reg_list)
          then lets
          else
            let (p, comb) = StringMap.find v lets in
            StringMap.add v (parents@p, comb) lets in
        ([{y_coord = 0.; id=id; node = (L And); parents = parents}], lets, new_reg_parents)
      | Id_Sub_seq(id, i1, i2, comb) ->
        ({y_coord = 0.; id=id; node = Sub (i1, i2); parents=parents}
        ::(fot (list_helper comb [id] lets reg_list reg_parents)), lets, reg_parents)
      | Id_Nth (id, n, comb) ->
        ({y_coord = 0.; id=id; node = Nth n; parents=parents}
        ::(fot (list_helper comb [id] lets reg_list reg_parents)), lets, reg_parents)
      | Id_Gate (id, g, c1, c2) ->
        ({y_coord = 0.; id=id; node = B g; parents = parents}
        ::(( fot (list_helper c1 [id] lets reg_list reg_parents))
        @ (fot (list_helper c2 [id] lets reg_list reg_parents))) , lets, reg_parents)
      | Id_Logical (id, l, c1, c2) ->
        ({y_coord = 0.; id=id; node = L l; parents = parents}
        ::(( fot (list_helper c1 [id] lets reg_list reg_parents))
        @ (fot (list_helper c2 [id] lets reg_list reg_parents))) , lets, reg_parents)
      | Id_Reduce ( id, g, comb ) ->
        ({y_coord = 0.; id=id; node = Red g; parents=parents}
        ::(fot (list_helper comb [id] lets reg_list reg_parents)), lets, reg_parents)
      | Id_Neg (id, n, comb) ->
        ({y_coord = 0.; id=id; node = N n; parents=parents}
        ::(fot (list_helper comb [id] lets reg_list reg_parents)), lets, reg_parents)
      | Id_Comp(id, c, c1, c2) ->
        ({y_coord = 0.; id=id; node = C c; parents = parents}
          ::(( fot (list_helper c1 [id] lets reg_list reg_parents))
          @ (fot (list_helper c2 [id] lets reg_list reg_parents))) , lets, reg_parents)
      | Id_Arith (id, o, c1, c2) ->
        ({y_coord = 0.; id=id; node = A o; parents = parents}
          ::(( fot (list_helper c1 [id] lets reg_list reg_parents))
          @ (fot (list_helper c2 [id] lets reg_list reg_parents))) , lets, reg_parents)
      | Id_Concat (id, c_list) ->
        let ids = List.fold_left (fun acc x -> (get_ids x)::acc) [] c_list in
        ({y_coord = 0.; id=id; node=(Concat (List.rev ids)); parents=parents}
        ::List.flatten((List.fold_right(fun x acc -> (fot (list_helper x [id] lets reg_list reg_parents))::acc) c_list []))
        , lets, reg_parents)
      | Id_Mux2 (id, c1, c2, c3) ->
        ({y_coord = 0.; id=id; node = (Mux (get_ids c1, get_ids c2, get_ids c3) ); parents=parents}
        :: ((fot (list_helper c1 [id] lets reg_list reg_parents))
        @ (fot (list_helper c2 [id] lets reg_list reg_parents))
        @ (fot (list_helper c3 [id] lets reg_list reg_parents))), lets, reg_parents)
      | Id_Apply (id, var, c_list) ->
        ({y_coord = 0.; id = id; node = (Subcirc var); parents = parents}
        ::List.flatten ((List.fold_right (fun x acc -> (fot (list_helper x [id] lets reg_list reg_parents))::acc) c_list []))
        , lets, reg_parents)
      | Id_Let (id, var, c1, c2) ->
        let new_lets = StringMap.add var ([], c1) lets in
        let inputs = list_dependencies c1 reg_list in
        ({y_coord = 0.; id = id; node = Let (var); parents = parents}
        ::(fot (list_helper c1 [id] new_lets reg_list reg_parents)), lets, reg_parents)
      in (list_helper ast [] StringMap.empty reg_list StringMap.empty) *)

  (* let columnize_ast ast_list =
    let rec column_helper finished not_done cols =
      if (List.length not_done = 0)
      then []
      else
        let find_register x =
          (match x.node with
          |Register _-> true
          | _ -> false) in
        let find_finished parent_list =
          List.for_all
          (fun x -> List.exists (fun y -> x = y.id) finished) parent_list in
        let new_col = (List.filter (fun x -> (find_register x || find_finished x.parents)) not_done) in
        let new_done = new_col@finished in
        let new_cols = new_col :: cols in
        let new_not_done = List.filter (fun x -> not (List.mem x new_done)) not_done in
        column_helper new_done new_not_done new_cols
      in column_helper [] ast_list [] *)
(*
  type ast_column = {
    x_coordinate : float;
    nodes : display_info list
  }

  type display_ast = {
    ast_columns : ast_column list;
    reg_list : (id * int list) list;
    let_list : (id * int list) list;
  }
  type display_input = Input of id | AST of display_ast

  type display_register = {
    y_coord : float;
    ast : display_input;
    reg_type: reg_type;
  }
  type circ_column = {
    registers : (id * display_register ) list;
    x_coordinate : float;
  } *)

  type display_reg_type = Dis_rising | Dis_falling | Dis_input | Dis_output
  type parent = Reg of id | Node of int

  let reg_type_to_display reg_type =
    match reg_type with
    | Rising -> Dis_rising
    | Falling -> Dis_falling
    | Input -> Dis_input
    | Output -> Dis_output

  type display_register = {
    id : id;
    reg_type :  display_reg_type;
    x_coord : float;
    y_coord : float;
    node_connections : int list;
    reg_connections : id list;
  }

  type display_node = {
    id : int;
    x_coord: float;
    y_coord: float;
    parent: parent;
    node : node;
  }

  type display_let = {
    id : id;
    x_coord:float;
    y_coord: float;
    inputs: id list;
    node_Connections: int list;
    reg_connections: id list;
  }
  type formatted_circuit = {
  	registers : display_register list;
  	nodes : display_node list;
  	lets : display_let list
  }


  (* let format_register reg_id register reg_list =
    let reg =
    match register.next with
    | User_input -> Input reg_id
    | AST ast ->
      let id_ast = attach_ids ast in
      let (ast, lets, reg) = tree_to_list id_ast reg_id reg_list in
      AST {ast_columns=(columnize_ast ast); reg_list=StringMap.empty; let_list=(StringMap.ampty);}
    in {y_coord=0.; ast=reg; reg_type=register.reg_type;} *)

  let r1 = {
    id = "A";
    reg_type = Dis_input;
    x_coord = 0.;
    y_coord = 0.;
    node_connections = [3;];
    reg_connections = [];
  }
  let r2 = {
    id = "B";
    reg_type = Dis_input;
    x_coord = 0.;
    y_coord = 25.;
    node_connections = [3;];
    reg_connections = [];
  }
  let r3 = {
    id = "C";
    reg_type = Dis_input;
    x_coord = 0.;
    y_coord = 50.;
    node_connections = [4;];
    reg_connections = [];
  }
  let r4 = {
    id = "D";
    reg_type = Dis_rising;
    x_coord = 50.;
    y_coord = 50.;
    node_connections = [6;];
    reg_connections = [];
  }
  let r5 = {
    id = "E";
    reg_type = Dis_output;
    x_coord = 100.;
    y_coord = 50.;
    node_connections = [];
    reg_connections = [];
  }
  let r = [r1;r2;r3;r4;r5;]

  let n1 = {
    id = 3;
    x_coord = 25.;
    y_coord = 25.;
    parent = Node 4;
    node = L And;
  }

  let n2 = {
    id = 4;
    x_coord = 35.;
    y_coord = 50.;
    parent = Reg "D";
    node = L And;
  }
  let n3 = {
    id = 3;
    x_coord = 75.;
    y_coord = 55.;
    parent = Reg "E";
    node = Red And;
  }
  let n = [n1; n2; n3]

  (* Test circuit *)
  let test_circ () =
  {
    registers = r;
    lets = [];
    nodes = n;
  }

  let format circ =
  {
    registers = r;
    lets = [];
    nodes = n;
  }

  let format_format_circuit f circ = ()
    (* Format.fprintf f "Columns : %s\n\n" (string_of_int (List.length circ));
    List.iter (fun x -> (
      print_string "\n";
      StringMap.iter
        (fun k v -> print_string (k^", ") ) x
      )
    ) circ *)

  let format_ast f ast = ()
  (* List.iter (fun col ->
    print_string "Column\n";
    print_string "Length : " ^ (string_of_int (List.length col) ^ "\n");
    print_string "~~~~~~~~~~~~~~~~~~~"

  ) ast *)
end

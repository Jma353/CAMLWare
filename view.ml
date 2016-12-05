open D3
open Extensions
open Components
open Circuit
open Circuit.Formatter
open Combinational


(* Sizes *)
let width = 800
let height = 400
let padding = 100
let nonNodeS = 60.
let nodeS = 50.


(* Scale makers *)
let make_scale dim edge = (fun x -> dim *. x /. 100. -. edge /. 2.)
let make_node_scale dim = make_scale dim nodeS
let make_non_node_scale dim = make_scale dim nonNodeS


(* Dimensions & scaling *)
let width_f    = float_of_int width
let height_f   = float_of_int height
let x_nn_scale = make_non_node_scale width_f
let y_nn_scale = make_non_node_scale height_f
let x_n_scale  = make_node_scale width_f
let y_n_scale  = make_node_scale height_f


(* Mini-Int Module for Maps *)
module Int = struct
  type t = int
  let make n : t = n
  let compare n1 n2 : int =
    if n1 < n2 then -1
    else if n1 > n2 then 1
    else 0
end


(* Maps *)
module IntMap = Map.Make(Int)

(* Representing an output *)
type point = { x : float; y : float }

(* Update Registers
 *
 * Given a circuit, select on all the registers & update them *)
let update_registers circ =
  let map = Circuit.register_values circ in
  let f k bit_s acc =
    ((select ("." ^ k)
    |. html (fun _ _ _ -> Bitstream.bitstream_to_hexstring bit_s))::acc) in
  let result = Circuit.StringMap.fold f map [] in
  plz_run (seq result)


(* Collect Registers
 *
 * This function collects all the registers in this variant of the circuit *)
let rec collect_registers (regs: display_register list) acc =
  match regs with
  | [] -> acc
  | h::t ->
  let x = x_nn_scale h.r_x_coord in
  let y = y_nn_scale h.r_y_coord in
  let zeros = Bitstream.zeros 32 in
  let id = h.r_id in
  begin match h.r_reg_type with
    | Dis_rising  -> collect_registers t ((u_register zeros id x y nonNodeS)::acc)
    | Dis_falling -> collect_registers t ((d_register zeros id x y nonNodeS)::acc)
    | Dis_output  -> collect_registers t ((o_register zeros id x y nonNodeS)::acc)
    | Dis_input   ->
      let f = Controller.did_change_input update_registers in
      collect_registers t ((i_register f zeros id x y nonNodeS)::acc)
  end


(* Collect Lets
 *
 * This function collects all the let-statements of this variant of the circuit *)
let rec collect_lets (lets: display_let list) acc =
  match lets with
  | [] -> acc
  | h::t ->
  let x = x_nn_scale h.l_x_coord in
  let y = y_nn_scale h.l_y_coord in
  let id = h.l_id in
  collect_lets t ((let_c id x y nonNodeS)::acc)


(* Collect Wires
 *
 * This function collects all the wires present in this variant of the
 * circuit *)
let rec collect_wires map (n_s:display_node list) acc =

  (* Add wirings of a node to a properly accumulating list *)
  let rec make_wirings c_s n x base_y space acc =
    match c_s with
    | [] -> acc
    | c::t ->
      let y = base_y +. (float_of_int n) *. space in
      begin match c with
        | Reg id ->
          make_wirings t (n+1) x base_y space ((l_tunnel id x y nodeS)::acc)
        | Let id ->
          make_wirings t (n+1) x base_y space ((l_tunnel id x y nodeS)::acc)
        | Node i ->
          let i_i = Int.make i in
          let pt = IntMap.find i_i map in
          make_wirings t (n+1) x base_y space ((wiring pt.x pt.y x y)::acc)
      end
  in

  (* Process node wirings (after categorizing which node we have) *)
  let process_node_wirings c_s cx cy acc =
    let side = nodeS in
    let n = List.length c_s in
    let x = cx in
    let base_y = cy in
    match n with
    | 1 -> make_wirings c_s 1 x base_y (side /. 2.) acc
    | 2 -> make_wirings c_s 0 x base_y side acc
    | a -> make_wirings c_s 0 x base_y (side /. ((float_of_int a) -. 1.)) acc
  in

  (* Adds wiring based on the type of node to exist *)
  let handle_wiring (n:display_node) acc =
    let cx = x_n_scale n.n_x_coord in
    let cy = y_n_scale n.n_y_coord in
    match n.node with
    | B (_,c1,c2)     -> process_node_wirings [c1;c2] cx cy acc
    | L (_,c1,c2)     -> process_node_wirings [c1;c2] cx cy acc
    | A (_,c1,c2)     -> process_node_wirings [c1;c2] cx cy acc
    | C (_,c1,c2)     -> process_node_wirings [c1;c2] cx cy acc
    | Mux (c1,c2,c3)  -> process_node_wirings [c1;c2;c3] cx cy acc
    | Sub (_,_,c)     -> process_node_wirings [c] cx cy acc
    | N (_,c)         -> process_node_wirings [c] cx cy acc
    | Nth (_,c)       -> process_node_wirings [c] cx cy acc
    | Red (_,c)       -> process_node_wirings [c] cx cy acc
    | Concat c_s      -> process_node_wirings c_s cx cy acc
    | Apply (_,c_s)   -> process_node_wirings c_s cx cy acc
    | Const _         -> acc
  in

  match n_s with
  | [] -> acc
  | h::t -> collect_wires map t (handle_wiring h acc)


(* Finalize Tunnels
 *
 * This function finalizes any tunneling we have for this circuit *)
let rec finalize_tunnels map (regs: display_register list) acc =

  (* Process  *)
  let process_reg_tunnel reg acc =
    match reg.input with
    | Reg id | Let id ->
      let x_r = x_nn_scale reg.r_x_coord in
      let y_r = y_nn_scale reg.r_y_coord in
      (l_tunnel id x_r (y_r +. nonNodeS /. 2.) nonNodeS)::acc
    | Node i ->
      if i <> -1 then
        let id_i = Int.make i in
        let p = IntMap.find id_i map in
        (r_tunnel reg.r_id p.x p.y nodeS)::acc
      else
        acc
  in

  match regs with
  | [] -> acc
  | h::t -> finalize_tunnels map t (process_reg_tunnel h acc)


(* Collect Nodes
 *
 * This function collects all the non-Registers/Lets of this variant
 * of the circuit *)
let rec collect_nodes (n:display_node list) acc =
  (* Singular helper *)
  let handle_node (n:display_node) stuff =
    let id_i = Int.make n.n_id in
    let acc = snd stuff in
    let x = x_n_scale n.n_x_coord in
    let y = y_n_scale n.n_y_coord in
    let map = (IntMap.add id_i
      {x = x +. nodeS; y = y +. nodeS /. 2.}
      (fst stuff)) in
    match n.node with
    | B (g,_,_) ->
      begin match g with
      | And  -> (map, (arith_and x y nodeS) ::acc)
      | Or   -> (map, (arith_or x y nodeS)  ::acc)
      | Xor  -> (map, (arith_xor x y nodeS) ::acc)
      | Nand -> (map, (arith_nand x y nodeS)::acc)
      | Nor  -> (map, (arith_nor x y nodeS) ::acc)
      | Nxor -> (map, (arith_nxor x y nodeS)::acc)
      end
    | L (g,_,_) ->
      begin match g with
      | And -> (map, (logical_and x y nodeS) ::acc)
      | Or  -> (map, (logical_or x y nodeS)  ::acc)
      | _   -> failwith "not possible"
      end
    | Red (g,_) ->
      begin match g with
      | And  -> (map, (red_and x y nodeS) ::acc)
      | Or   -> (map, (red_or x y nodeS)  ::acc)
      | Xor  -> (map, (red_xor x y nodeS) ::acc)
      | Nand -> (map, (red_nand x y nodeS)::acc)
      | Nor  -> (map, (red_nor x y nodeS) ::acc)
      | Nxor -> (map, (red_nxor x y nodeS)::acc)
      end
    | A (a,_,_) ->
      begin match a with
      | Add      -> (map, (add_c x y nodeS)                 ::acc)
      | Subtract -> (map, (subtract_c x y nodeS)            ::acc)
      | Sll      -> (map, (shift_left_logical x y nodeS)    ::acc)
      | Srl      -> (map, (shift_right_logical x y nodeS)   ::acc)
      | Sra      -> (map, (shift_right_arithmetic x y nodeS)::acc)
      end
    | N (n,_) ->
      begin match n with
      | Neg_bitwise    -> (map, (bitwise_not x y nodeS)::acc)
      | Neg_logical    -> (map, (logical_not x y nodeS)::acc)
      | Neg_arithmetic -> (map, (arith_not x y nodeS)  ::acc)
      end
    | C (c,_,_) ->
      begin match c with
      | Lt  -> (map, (less_than x y nodeS)               ::acc)
      | Gt  -> (map, (greater_than x y nodeS)            ::acc)
      | Eq  -> (map, (equal_to x y nodeS)                ::acc)
      | Lte -> (map, (less_than_or_equal_to x y nodeS)   ::acc)
      | Gte -> (map, (greater_than_or_equal_to x y nodeS)::acc)
      | Neq -> (map, (not_equal_to x y nodeS)            ::acc)
      end
    | Sub (i1,i2,_)   -> (map, (sub_seq_c i1 i2 x y nodeS)::acc)
    | Nth (n,_)       -> (map, (nth_c n x y nodeS)        ::acc)
    | Concat (_)      -> (map, (concat_c x y nodeS)       ::acc)
    | Mux (_,_,_)     -> (map, (mux2_c x y nodeS)         ::acc)
    | Const b         -> (map, (constant b x y nodeS)     ::acc)
    | Apply (id,_)    -> (map, (sub_circ_c id x y nodeS)  ::acc)
  in

  match n with
  | [] -> acc
  | h::t -> collect_nodes t (handle_node h acc)


(* Make
 *
 * Creates a circuit *)
let make circ =

  (* Applies all views to a container *)
  let rec apply_views views container =
    match views with
    | []   -> container
    | h::t -> apply_views t (container |> h)
  in

  (* Collects a list of functions to be applied to a view, as well as map
   * info regarding inputs & outputs. *)
  let collect_views (op_c: circuit option) =
    (* Clears the view *)
    plz_run (select ".circuit" |. html (fun _ _ _ -> ""));
    match op_c with
    | None -> []
    | Some a_c ->

      (* Format the circuit *)
      let c = Circuit.Formatter.format a_c in

      (* Helper function *)
      let get_second = (fun (_,x) -> x) in

      (* Format info *)
      let registers  = List.map get_second c.registers in
      let lets       = List.map get_second c.lets in
      let c_nodes    = List.map get_second c.nodes in

      (* Collect Everything *)
      let regs       = collect_registers registers [] in
      let lets       = collect_lets lets [] in
      let e_map      = IntMap.empty in
      let stuff      = collect_nodes c_nodes (e_map,[]) in
      let nodes      = snd stuff in
      let map        = fst stuff in
      let wires      = collect_wires map c_nodes [] in
      let tunnels    = finalize_tunnels map registers [] in
      regs @ lets @ nodes @ wires @ tunnels
  in

  (* Our resultant *)
  let result = apply_views (collect_views circ) (select ".circuit") in

  (* Apply it to the view *)
  plz_run result;

  (* Set the registers to their starting values *)
  match circ with
  | None -> ()
  | Some c -> update_registers c

(* Initial view for compiling *)
let init_view () =
  let init = initial_svg width height padding in
  let div = compile_area (Controller.did_compile make) in
  let step_b = step_btn (Controller.did_step update_registers) in
  let clock_c = clock () in
  seq [init; div; step_b; clock_c]

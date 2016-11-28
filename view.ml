open D3
open Extensions
open Components
open Circuit
open Circuit.Formatter

(* Sizes *)
let nonNodeS = 80.
let nodeS = 50.

(* Representing an output *)
type point = { x : float; y : float }

(* Mini-Int module for use in mapping id's to channels *)
module Int = struct
  type t = int
  let make n : t = n
  let compare n1 n2 : int =
    if n1 < n2 then -1
    else if n1 > n2 then 1
    else 0
end

(* Int of int -> output point *)
module IntMap = Map.Make(Int)

(* Make a generic scale *)
let make_scale dim edge =
  (fun x -> dim *. x /. 100. -. edge /. 2.)

(* Scale creation *)
let make_node_scale dim = make_scale dim nodeS
let make_non_node_scale dim = make_scale dim nonNodeS

(* Collect registers to add to the screen *)
let rec collect_registers x_scale y_scale (regs: display_register list) acc =
  match regs with
  | [] -> acc
  | h::t ->
    let x = x_scale h.x_coord in
    let y = y_scale h.y_coord in
    let ones = Bitstream.ones 32 in
    let id = h.id in
    begin match h.reg_type with
      | Dis_rising  -> collect_registers x_scale y_scale t ((u_register ones id x y nonNodeS)::acc)
      | Dis_falling -> collect_registers x_scale y_scale t ((d_register ones id x y nonNodeS)::acc)
      | Dis_input   -> collect_registers x_scale y_scale t ((i_register ones id x y nonNodeS)::acc)
      | Dis_output  -> collect_registers x_scale y_scale t ((o_register ones id x y nonNodeS)::acc)
    end

(* Collect lets to add to the screen *)
let rec collect_lets x_scale y_scale (lets: display_let list) acc =
  match lets with
  | [] -> acc
  | h::t ->
    let x = x_scale h.x_coord in
    let y = y_scale h.y_coord in
    let id = h.id in
    collect_lets x_scale y_scale t ((let_c id x y nonNodeS)::acc)

(* Add wirings of a node to a properly accumulating list *)
let rec make_wirings c_s map n x base_y space acc =
  match c_s with
  | [] -> acc
  | c::t ->
    let y = base_y +. (float_of_int n) *. space in
    begin match c with
      | RegOrLet id ->
        make_wirings t map (n+1) x base_y space ((tunnel id x y nodeS)::acc)
      | Node i ->
        let i_i = Int.make i in
        let pt = IntMap.find i_i map in
        make_wirings t map (n+1) x base_y space ((wiring pt.x pt.y x y)::acc)
    end

(* Process node wirings (after categorizing which node we have) *)
let process_node_wirings c_s map cx cy acc =
  let side = nodeS in
  let n = List.length c_s in
  let x = cx -. side /. 2. in
  let base_y = cy -. side /. 2. in
  match n with
  | 1 -> make_wirings c_s map 1 x base_y (side /. 2.) acc
  | 2 -> make_wirings c_s map 0 x base_y side acc
  | a -> make_wirings c_s map 0 x base_y (side /. ((float_of_int a) -. 1.)) acc

(* Adds wiring based on the type of node to exist *)
let handle_wiring x_scale y_scale map (n:display_node) acc =
  let cx = x_scale n.x_coord in
  let cy = y_scale n.y_coord in
  match n.node with
  | B (_,c1,c2)     -> process_node_wirings [c1;c2] map cx cy acc
  | L (_,c1,c2)     -> process_node_wirings [c1;c2] map cx cy acc
  | A (_,c1,c2)     -> process_node_wirings [c1;c2] map cx cy acc
  | N (_,c)         -> process_node_wirings [c] map cx cy acc
  | C (_,c1,c2)     -> process_node_wirings [c1;c2] map cx cy acc
  | Sub (_,_,c1,c2) -> process_node_wirings [c1;c2] map cx cy acc
  | Nth (_,c)       -> process_node_wirings [c] map cx cy acc
  | Red (_,c)       -> process_node_wirings [c] map cx cy acc
  | Concat c_s      -> process_node_wirings c_s map cx cy acc
  | Mux (c1,c2,c3)  -> process_node_wirings [c1;c2;c3] map cx cy acc
  | Const _         -> acc
  | Apply (_,c_s)   -> process_node_wirings c_s map cx cy acc

(* Collect wires to add to the screen *)
let rec collect_wires x_scale y_scale map (n_s:display_node list) acc =
  match n_s with
  | [] -> acc
  | h::t -> collect_wires x_scale y_scale map t (handle_wiring x_scale y_scale map h acc)

(* Collects a list of functions to be applied to a view, as well as map
 * info regarding inputs & outputs. *)
let collect_views (width:int) (height:int) (c: formatted_circuit) =

  (* Dimensions in float form *)
  let width_f = float_of_int width in
  let height_f = float_of_int height in

  (* Non-node scales *)
  let x_nn_scale = make_non_node_scale width_f in
  let y_nn_scale = make_non_node_scale height_f in

  (* Node-scales *)
  let x_n_scale = make_node_scale width_f in
  let y_n_scale = make_node_scale height_f in

  (* Collect the registers & lets *)
  let regs = collect_registers x_nn_scale y_nn_scale c.registers [] in
  let lets = collect_lets x_nn_scale y_nn_scale c.lets [] in

  (* Collect nodes + mapping from ints to output *)
  (* Collect wires *)

  let result = regs @ lets in



  result

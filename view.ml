open D3
open Extensions
open Components
open Circuit
open Circuit.Formatter

(* Sizes *)
let nonNodeS = 100.
let nodeS = 50.

(* Representing a location (of inputs &/or outputs) *)
type point = { x : int; y : int }

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

(* Make a scale for placing down nodes *)
let make_node_scale dim = make_scale dim nodeS

(* Make a scale for placing down non-nodes *)
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

  let result = regs @ lets in



  result

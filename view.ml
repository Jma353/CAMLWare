open D3
open Extensions
open Components
open Circuit
open Circuit.Formatter

(* Sizes *)
let regS = 100.
let letS = 100.
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
let make_node_scale dim = make_scale nodeS

(* Make a scale for placing down registers *)
let make_register_scale dim = make_scale regS

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
      | Dis_rising  -> collect_registers x_scale y_scale t ((u_register ones id x y regS)::acc)
      | Dis_falling -> collect_registers x_scale y_scale t ((d_register ones id x y regS)::acc)
      | Dis_input   -> collect_registers x_scale y_scale t ((i_register ones id x y regS)::acc)
      | Dis_output  -> collect_registers x_scale y_scale t ((o_register ones id x y regS)::acc)
    end

(* Collects a list of functions to be applied to a view, as well as map
 * info regarding inputs & outputs. *)
let collect_circuit_views x_scale y_scale (c: formatted_circuit) =
  let regs = collect_registers x_scale y_scale c.registers [] in


  regs

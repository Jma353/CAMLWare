open D3
open Extensions
open Components
open Circuit
open Circuit.Formatter
open Combinational

(* Int
 *
 * This mini-module *)
module Int = struct
  type t = int
  let make n : t = n
  let compare n1 n2 : int =
    if n1 < n2 then -1
    else if n1 > n2 then 1
    else 0
end

(* The Maps
 *
 * These modules are utilized in various places across the front-end
 * to handle associations between integers / strings and arbitrary data-types *)
module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)


(* Global State
 *
 * Houses the data-type that is persisted for our application.
 * Features the mappings from registers to their decimal values, as
 * well as the current circuit we are dealing with displaying. *)
let code = ref ""
let circ = ref None


(* The Events
 *
 * This module deals with the events that happen during the lifecycle of
 * the application.  They are reported via the application (creation
 * of the particular types of events in dealt with in the handlers on)
 * specific actions by the user. *)
module Event = struct

  (* Types of events *)
  type t =
    | ChangeInputs of int StringMap.t (* Created on reading *)
    | Compile of string (* Grabbed from the string being compiled *)
    | Step

  (* Handle changing input *)
  let change_input_handler map =
    let f = (fun k v acc -> (Simulator.change_input
      acc k (Bitstream.bitstream_of_integer v))) in
    match !circ with
    | Some(c) -> circ := Some(StringMap.fold f map c)
    | _ -> failwith "fail"

  (* Handle compiling *)
  let compile_handler s =
    circ := Some(s |> Parse.parse_circuit_no_errors)

  (* Handle stepping *)
  let step_handler =
    match !circ with
    | None -> circ := None
    | Some(c) -> circ := Some(Simulator.step c)

  (* Handler for different types of events *)
  let handle t m =
    match t with
    | ChangeInputs map -> change_input_handler map
    | Compile s        -> compile_handler s
    | Step             -> step_handler

end

(* View
 * Handles building the initial view, as well as constructing the proper dom
 * on building a circuit *)
module View = struct


  (* Sizes *)
  let width = 800
  let height = 400
  let padding = 100
  let nonNodeS = 80.
  let nodeS = 50.


  (* Representing an output *)
  type point = { x : float; y : float }


  (* Scales *)
  let make_scale dim edge = (fun x -> dim *. x /. 100. -. edge /. 2.)
  let make_node_scale dim = make_scale dim nodeS
  let make_non_node_scale dim = make_scale dim nonNodeS


  (* Collect Registers
   *
   * This function collects all the registers in this variant of the circuit *)
  let rec collect_registers x_scale y_scale (regs: display_register list) acc =
    match regs with
    | [] -> acc
    | h::t ->
      let x = x_scale h.x_coord in
      let y = y_scale h.y_coord in
      let zeros = Bitstream.zeros 32 in
      let id = h.id in
      begin match h.reg_type with
        | Dis_rising  -> collect_registers x_scale y_scale t ((u_register zeros id x y nonNodeS)::acc)
        | Dis_falling -> collect_registers x_scale y_scale t ((d_register zeros id x y nonNodeS)::acc)
        | Dis_input   -> collect_registers x_scale y_scale t ((i_register zeros id x y nonNodeS)::acc)
        | Dis_output  -> collect_registers x_scale y_scale t ((o_register zeros id x y nonNodeS)::acc)
      end


  (* Collect Lets
   *
   * This function collects all the let-statements of this variant of the circuit *)
  let rec collect_lets x_scale y_scale (lets: display_let list) acc =
    match lets with
    | [] -> acc
    | h::t ->
      let x = x_scale h.x_coord in
      let y = y_scale h.y_coord in
      let id = h.id in
      collect_lets x_scale y_scale t ((let_c id x y nonNodeS)::acc)


  (* Collect Wires
   *
   * This function collects all the wires present in this variant of the
   * circuit *)
  let rec collect_wires x_scale y_scale map (n_s:display_node list) acc =

    (* Add wirings of a node to a properly accumulating list *)
    let rec make_wirings c_s n x base_y space acc =
      match c_s with
      | [] -> acc
      | c::t ->
        let y = base_y +. (float_of_int n) *. space in
        begin match c with
          | RegOrLet id ->
            make_wirings t (n+1) x base_y space ((tunnel id x y nodeS)::acc)
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
      let x = cx -. side /. 2. in
      let base_y = cy -. side /. 2. in
      match n with
      | 1 -> make_wirings c_s 1 x base_y (side /. 2.) acc
      | 2 -> make_wirings c_s 0 x base_y side acc
      | a -> make_wirings c_s 0 x base_y (side /. ((float_of_int a) -. 1.)) acc
    in

    (* Adds wiring based on the type of node to exist *)
    let handle_wiring (n:display_node) acc =
      let cx = x_scale n.x_coord in
      let cy = y_scale n.y_coord in
      match n.node with
      | B (_,c1,c2)     -> process_node_wirings [c1;c2] cx cy acc
      | L (_,c1,c2)     -> process_node_wirings [c1;c2] cx cy acc
      | A (_,c1,c2)     -> process_node_wirings [c1;c2] cx cy acc
      | C (_,c1,c2)     -> process_node_wirings [c1;c2] cx cy acc
      | Sub (_,_,c1,c2) -> process_node_wirings [c1;c2] cx cy acc
      | Mux (c1,c2,c3)  -> process_node_wirings [c1;c2;c3] cx cy acc
      | N (_,c)         -> process_node_wirings [c] cx cy acc
      | Nth (_,c)       -> process_node_wirings [c] cx cy acc
      | Red (_,c)       -> process_node_wirings [c] cx cy acc
      | Concat c_s      -> process_node_wirings c_s cx cy acc
      | Apply (_,c_s)   -> process_node_wirings c_s cx cy acc
      | Const _         -> acc
    in

    match n_s with
    | [] -> acc
    | h::t -> collect_wires x_scale y_scale map t (handle_wiring h acc)


  (* Collect Nodes
   *
   * This function collects all the non-Registers/Lets of this variant
   * of the circuit *)
  let rec collect_nodes x_scale y_scale (n:display_node list) acc =
    (* Singular helper *)
    let handle_node (n:display_node) stuff =
      let id_i = Int.make n.id in
      let acc = snd stuff in
      let x = x_scale n.x_coord in
      let y = y_scale n.y_coord in
      let map = IntMap.add id_i {x = x +. nodeS /. 2.; y} (fst stuff) in
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
      | Sub (i1,i2,_,_) -> (map, (sub_seq_c x y nodeS i1 i2)::acc)
      | Nth (n,_)       -> (map, (nth_c x y nodeS n)        ::acc)
      | Concat (_)      -> (map, (concat_c x y nodeS)       ::acc)
      | Mux (_,_,_)     -> (map, (mux2_c x y nodeS)         ::acc)
      | Const b         -> (map, (constant b x y nodeS)     ::acc)
      | Apply (id,_)    -> (map, (sub_circ_c id x y nodeS)  ::acc)
    in

    match n with
    | [] -> acc
    | h::t -> collect_nodes x_scale y_scale t (handle_node h acc)


  (* Make *)
  let make k =

    (* An initial, blank view *)
    let init =
      (* Total SVG Dimensions *)
      let width = width + 2 * padding in
      let height = height + 2 * padding in
      (* Base SVG *)
      let svg =
        (append "svg"
        |. int attr "width" width
        |. int attr "height" height) in
      (* Circuit Container *)
      let g =
        (append "g"
        |. str attr "class" "circuit"
        |. str attr "transform" (translate padding padding)) in
      (* Border *)
      let border_rect =
        (append "rect"
        |. int attr "x" 0
        |. int attr "y" 0
        |. int attr "width" width
        |. int attr "height" height
        |. str style "stroke" "black"
        |. str style "fill" "none"
        |. int style "stroke-width" 1) in
      (* Circuit *)
      let bordered_svg = (svg |- border_rect) in
      let circuit = bordered_svg |- g in
      (* Compilation box *)
      let input_box =
        static "div"
        |. seq [
            static "textarea"
            |. str attr "class" "code"
            |. int attr "rows" 10
            |. int attr "cols" 80
            |. str attr "value" !code] in
      seq [circuit; input_box] in


    (* Applies all views to a container *)
    let rec apply_views views container =
      match views with
      | []   -> container
      | h::t -> apply_views t (container |> h)
    in


    (* Collects a list of functions to be applied to a view, as well as map
     * info regarding inputs & outputs. *)
    let collect_views (op_c: circuit option) =
      match op_c with
      | None -> []
      | Some(a_c) ->
        let c = format_circuit a_c in
        (* Dimensions & scaling *)
        let width_f = float_of_int width in
        let height_f = float_of_int height in
        let x_nn_scale = make_non_node_scale width_f in
        let y_nn_scale = make_non_node_scale height_f in
        let x_n_scale = make_node_scale width_f in
        let y_n_scale = make_node_scale height_f in

        (* Collect Everything *)
        let regs = collect_registers x_nn_scale y_nn_scale c.registers [] in
        let lets = collect_lets x_nn_scale y_nn_scale c.lets [] in
        let e_map = IntMap.empty in
        let stuff = collect_nodes x_n_scale y_n_scale c.nodes (e_map,[]) in
        let nodes = snd stuff in
        let map = fst stuff in
        let wires = collect_wires x_n_scale y_n_scale map c.nodes [] in

        (* Resultant *)
        regs @ lets @ nodes @ wires
    in

    apply_views (collect_views !circ) init


end


(* Event Loop  *)
let main_lwt () =
  let stream, push, _ =
    let stream, push = Lwt_stream.create () in
    stream, (fun x -> push (Somex)), (fun () -> push None)
  in
  let view = View.make push in
  let node = (Dom_html.document##body) in
  run ~node view ();
  Lwt_stream.fold (fun e m -> run ~node view (); ()) stream ()



(* Run the app *)
let _ = Lwt_js_events.async main_lwt




(* end *)

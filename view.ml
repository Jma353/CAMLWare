open D3
open Extensions
open Components
open Circuit
open Circuit.Formatter

let side = 50.

let make_scale size =
  (fun x -> (x /. 100.) *. (float_of_int size) -. side /. 2.)

let add_reg reg x y_scale funcs =
  match reg with
  | (id, r) -> (u_register (Bitstream.ones 32) id x (y_scale r.y_coord) side)::funcs

let rec process_regs regs x y_scale funcs =
  match regs with
  | [] -> funcs
  | h::t -> process_regs t x y_scale (add_reg h x y_scale funcs)

let process_column (col:circ_column) x_scale y_scale funcs =
  process_regs col.registers (x_scale col.x_coordinate) y_scale funcs

let rec process_columns cols x_scale y_scale funcs =
  match cols with
  | [] -> funcs
  | h::t -> process_columns t x_scale y_scale (process_column h x_scale y_scale funcs)

let gather_components (f_c:formatted_circuit) x_scale y_scale =
  process_columns f_c x_scale y_scale []

let rec apply_to_view comps d3_container =
  match comps with
  | [] -> d3_container
  | h::t -> apply_to_view t (d3_container |> h)

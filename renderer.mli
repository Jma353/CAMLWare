open Circuit
open Formatted_circuit
open Lwt_js_events

(* Redraws ONLY registers on the GUI in response to a step or change in input *)
val draw_registers : formatted_circuit -> unit

(* Draws an entire circuit from a formatted_circuit object *)
val draw : formatted_circuit -> unit

(* Performs one step in the circuit using any specificed user inputs*)
val step : Dom_html.eventTarget Js.t -> Dom_html.mouseEvent Js.t -> formatted_circuit

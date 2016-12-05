(* Controller
 *
 * Controller links view and model, facilitating view changes and augmenting
 * the model via the model's state-changing functions.  Controller functions
 * take in functions that the view provides it that allow it to change
 * the view after model updates.  This allows for events to trigger controller
 * functions, which have a means of updating the state and then updating the
 * view. *)

open D3
open Extensions

(* Handler that modifies the model state on changing an input and applies
 * that change to the view, given a view-augmenting function *)
val did_change_input : (Circuit.circuit -> unit) -> string -> unit

(* Handler that modifies the model state on attempting to step the circuit
 * and applies changes to the view, given a view-augmenting function *)
val did_step : (Circuit.circuit -> unit) -> unit -> unit

(* Handler that modifies the model state on attempting to compile and
 * applies changes to the view, given a view-augmenting function *)
val did_compile: (Circuit.circuit option -> unit) -> unit -> unit

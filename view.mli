(* View
 *
 * View handles all UI building.  It attaches controller methods to specific
 * view components so that user-interaction triggers appropriate state changes.
 * The view leaves it to the controller to handle the actual logic of the state
 * change, however, and concerns itself with all UI-based drawing and logic. *)


open D3
open Extensions
open Components

(* View entrypoint *)
val init_view : unit -> ('a, 'a) D3.t

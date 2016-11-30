open D3
open Extensions
open Components
open Circuit
open Circuit.Formatter
open Combinational

(* View entrypoint *)
val init_view : circuit option ref -> ('a, 'a) D3.t

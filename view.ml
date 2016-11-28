open D3
open Extensions
open Components
open Circuit
open Circuit.Formatter

(* Representing a location (of inputs &/or outputs) *)
type point = { x : int; y : int }

(* top to bottom point indexing *)
type view_component =
  | VConstant of point
  | VURegister of point * point
  | VDRegister of point * point
  | VMux2 of point * point * point * point
  | VNth of point * point
  | VSubSeq of point * point
  (* Todo more *)

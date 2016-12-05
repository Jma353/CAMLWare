open D3
open Extensions

(* Clock UI update *)
let set_clock num =
  let clock_lol = get_element_by_class_name "clock" in
  let new_content = "Clock: " ^ string_of_int num in
  Js.Unsafe.set (clock_lol) "innerHTML" (Js.string new_content)

(* STATE CHANGE
 * On clicking on an input register *)
let did_change_input f id =
  let msg = "Enter a new hexadecimal value for input " ^ id in
  let num = prompt msg "" in
  let model_mods = Model.change_input_and_return id num in
  set_clock (fst model_mods);
  match (snd model_mods) with
  | None -> ()
  | Some (c) ->
    f c; ()

(* STATE CHANGE
 * On stepping a circuit *)
let did_step f () =
  let clock_val, new_circ = Model.step_and_return () in
  set_clock clock_val;
  match new_circ with
  | None -> ()
  | Some (c) -> f c; ()

let rec make_lines s =
  match try Some (String.index s '\n') with Not_found -> None with
  | None -> s::[]
  | Some i -> (String.sub s 0 i) ::
              (make_lines (String.sub s (i+1) (String.length s - i - 1)))

let change_newlines s =
  let lines = make_lines s in
  let rec helper = function
    | [] -> ""
    | h::[] -> h
    | h::t -> h ^ "<br>" ^ (helper t) in
  helper lines

(* STATE CHANGE
 * On compiling - update the state of the reference we're dealing
 * with and call function f *)
let did_compile f () =
  let comp_helper msg num circ =
    set_clock num;
    let debug = get_element_by_class_name "debug-output" in
    Js.Unsafe.set (debug) "innerHTML" (Js.string msg);
    f circ; () in
  let text_area = get_element_by_class_name "code" in
  let code = Js.Unsafe.get (text_area) "value" |> Js.to_string in
  let model_mods = Model.compile_and_return code in
  match (snd model_mods) with
  | Parse.Error s ->
    comp_helper (change_newlines s) (fst model_mods) None
  | Parse.Result c ->
    comp_helper "Debug output" (fst model_mods) (Some(c))

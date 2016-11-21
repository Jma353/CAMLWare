open Circuit
open Combinational
open Bitstream

(* an error log is a monadic data type containing a circuit and a list of
 * string descriptions of the errors in it *)
type error_log = circuit * string list

(* monadic return *)
let make_log circ =
  (circ,[])

(* monadic bind *)
let (|>>) log f =
  let new_log = f (fst log) in
  (fst new_log, (snd new_log) @ (snd log))

(* [detect_variable_errors circ] detects and logs the following errors:
 * - binding a local variable that shadows a register name
 * - referring to the value of an output
 * - using an unbound variable *)
let rec detect_variable_errors circ =
  failwith "TODO"

(* [detect_recursion circ] detects and logs any potentially recursive function
 * calls. These are not a valid construct in hardware implementation *)
let rec detect_recursion circ =
  failwith "TODO"

(* validate pipes the circuit through several error checking functions *)
let validate circ =
  circ |>
  make_log |>>
  detect_variable_errors |>>
  detect_recursion

let valid (_,log) =
  List.length log = 0

let print_log (_,log) =
  let rec format_log f =
    function
    | [] -> ()
    | h::[] -> Format.fprintf f "%s\n" h
    | h::t -> Format.fprintf f "%s\n\n%a" h (format_log) t in
  let l = List.length log in
  if l = 0 then Format.printf "No errors were detected\n" else
    Format.eprintf "%i %s detected\n\n%a"
      l (if l = 1 then "error was" else "errors were") (format_log) log

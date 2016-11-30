(* Concept for this came from: https://goo.gl/Bq5wyw *)

open Js

(* Storage driver *)
let storage =
  Optdef.case (Dom_html.window##localStorage)
    (fun () -> failwith "Storage is not supported on this browser")
    (fun v -> v)

(* Key identifying what we store *)
let key = string "circuit-state"

(* Find the state *)
let find () =
  let r = storage##getItem(key) in
  Opt.to_option @@ Opt.map r to_string

(* Set an item *)
let set v = storage##setItem(key, string v)

(* Initialize an item *)
let init default =
  match find () with
  | None -> set default ; default
  | Some v -> v

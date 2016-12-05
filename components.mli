open D3
open Extensions
open Circuit

(* Wiring *)
val wiring   : float  -> float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val l_tunnel : string -> float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val r_tunnel : string -> float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t

(* Gates *)
val arith_not   : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val arith_and   : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val arith_nand  : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val arith_or    : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val arith_nor   : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val arith_xor   : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val arith_nxor  : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val red_and     : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val red_nand    : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val red_or      : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val red_nor     : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val red_xor     : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val red_nxor    : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val logical_and : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val logical_or  : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val logical_not : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val bitwise_not : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t

(* SubBits *)
val nth_c     : int -> float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val sub_seq_c : int -> int   -> float -> float -> float         -> ('a, 'b) D3.t -> ('a, 'b) D3.t

(* Registers *)
val u_register : Bitstream.bitstream -> string -> float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val d_register : Bitstream.bitstream -> string -> float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val o_register : Bitstream.bitstream -> string -> float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t

(* Comparators *)
val less_than                : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val greater_than             : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val equal_to                 : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val less_than_or_equal_to    : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val greater_than_or_equal_to : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val not_equal_to             : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t

(* Arithmetics *)
val add_c                    : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val subtract_c               : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val shift_left_logical       : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val shift_right_logical      : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val shift_right_arithmetic   : float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t

(* Misc *)
val constant    : Bitstream.bitstream -> float -> float -> float         -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val mux2_c      : float               -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val let_c       : string              -> float -> float -> float         -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val sub_circ_c  : string              -> float -> float -> float         -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val concat_c    : float               -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val svg_container_div : ('a, 'a) D3.t -> ('a, 'a) D3.t
val initial_svg : int                 -> int   -> int   -> ('a, 'a) D3.t
val clock       : unit -> ('a, 'a) D3.t

(* Triggers *)
val i_register : (string -> unit) -> Bitstream.bitstream -> string ->
  float -> float -> float -> ('a, 'b) D3.t -> ('a, 'b) D3.t
val compile_area : (unit -> unit) -> ('a, 'a) D3.t -> ('a, 'a) D3.t -> ('a, 'a) D3.t
val step_btn     : (unit -> unit) -> ('a, 'a) D3.t

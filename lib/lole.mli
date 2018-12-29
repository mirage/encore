module type VALUE = sig
  type t

  val sentinel : t

  val weight : t -> int

  val merge : t -> t -> t option

  val pequal : t -> t -> bool

  val pp : t Fmt.t
end

module RBQ (Value : VALUE) :
  sig
    type t

    type value

    val make : int -> t

    val pp : t Fmt.t

    val available : t -> int

    val push : t -> value -> (t, t) result

    val shift_exn : t -> value * t

    val cons : t -> value -> (t, t) result

    val cons_exn : t -> value -> t

    val weight : t -> int

    val to_list : t -> value list
  end
  with type value = Value.t

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'a blitter = 'a -> int -> bigstring -> int -> int -> unit

val pp_chr : char Fmt.t

val pp_scalar :
  get:('buffer -> int -> char) -> length:('buffer -> int) -> 'buffer Fmt.t

val pp_string : string Fmt.t

val pp_bytes : Bytes.t Fmt.t

val pp_bigstring : bigstring Fmt.t

module RBA : Ke.Sigs.Weighted.F

module Buffer : sig
  type t = Bigstring of bigstring | String of string | Bytes of Bytes.t

  val weight : t -> int

  val pp : t Fmt.t

  val sub : t -> int -> int -> t
end

module IOVec : sig
  type t = {buffer: Buffer.t; off: int; len: int}

  val weight : t -> int

  val length : t -> int

  val lengthv : t list -> int

  val shift : t -> int -> t

  val split : t -> int -> t * t

  val pequal : t -> t -> bool

  val merge : t -> t -> t option

  val pp : t Fmt.t
end

type encoder

val pp : encoder Fmt.t

type 'v state =
  | Flush of {continue: int -> 'v state; iovecs: IOVec.t list}
  | Continue of {continue: encoder -> 'v state; encoder: encoder}
  | End of 'v

val create : int -> encoder
val from : int -> bigstring -> encoder

val shift_buffers : int -> encoder -> IOVec.t list * encoder

val shift_flushes : int -> encoder -> encoder

val shift : int -> encoder -> IOVec.t list * encoder

val has : encoder -> int

val drain : int -> encoder -> encoder

val flush : (encoder -> 'value state) -> encoder -> 'value state

val continue : (encoder -> 'value state) -> encoder -> 'value state

val schedule :
     (encoder -> 'r state)
  -> length:('v -> int)
  -> buffer:('v -> Buffer.t)
  -> ?off:int
  -> ?len:int
  -> 'v
  -> encoder
  -> 'r state

val schedule_string :
     (encoder -> 'r state)
  -> encoder
  -> ?off:int
  -> ?len:int
  -> string
  -> 'r state

val schedule_bytes :
     (encoder -> 'r state)
  -> encoder
  -> ?off:int
  -> ?len:int
  -> Bytes.t
  -> 'r state

val schedule_bigstring :
     (encoder -> 'r state)
  -> encoder
  -> ?off:int
  -> ?len:int
  -> bigstring
  -> 'r state

val schedulev :
     (encoder -> 'r state)
  -> ((Buffer.t -> int) * int option * int option * Buffer.t) list
  -> encoder
  -> 'r state

val schedulev_bigstring :
  (encoder -> 'r state) -> bigstring list -> encoder -> 'r state

val write :
     (encoder -> 'r state)
  -> blit:'v blitter
  -> length:('v -> int)
  -> ?off:int
  -> ?len:int
  -> 'v
  -> encoder
  -> 'r state

val writev :
     (encoder -> 'r state)
  -> ('v blitter * ('v -> int) * int option * int option * 'v) list
  -> encoder
  -> 'r state

val bigarray_blit_from_string : string blitter

val bigarray_blit_from_bytes : Bytes.t blitter

val bigarray_blit : bigstring blitter

val write_string :
     ?off:int
  -> ?len:int
  -> string
  -> (encoder -> 'r state)
  -> encoder
  -> 'r state

val write_bytes :
     ?off:int
  -> ?len:int
  -> Bytes.t
  -> (encoder -> 'r state)
  -> encoder
  -> 'r state

val write_bigstring :
     ?off:int
  -> ?len:int
  -> bigstring
  -> (encoder -> 'r state)
  -> encoder
  -> 'r state

val write_char : char -> (encoder -> 'r state) -> encoder -> 'r state

val write_uint8 : int -> (encoder -> 'r state) -> encoder -> 'r state

module type SE = sig
  val write_uint16 : int -> (encoder -> 'r state) -> encoder -> 'r state

  val write_uint32 : int32 -> (encoder -> 'r state) -> encoder -> 'r state

  val write_uint64 : int64 -> (encoder -> 'r state) -> encoder -> 'r state
end

module LE : SE

module BE : SE

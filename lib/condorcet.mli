type vec =
  { off : int option
  ; len : int option }

type 'a state  = 'a Lavoisier.state
type encoder   = Lavoisier.encoder
type bigstring = Lavoisier.bigstring
type iovecs    = Lavoisier.IOVec.t list

type -'a t
type -'a s

val char: char t
val int8: int t
val beint16: int t
val beint32: int32 t
val beint64: int64 t
val leint16: int t
val leint32: int32 t
val leint64: int64 t
val bool: bool t

val substring: string s
val subbytes: bytes s
val subbigstring: bigstring s

val blitter: ('a -> int) -> ('a -> int -> bigstring -> int -> int -> unit) -> 'a s

val whole: 'a s -> 'a t
val sub: 'a s -> (vec * 'a) t

val string: string t
val bytes: bytes t
val bigstring: bigstring t

val list: ?sep:unit t -> 'a t -> 'a list t
val option: 'a t -> 'a option t
val compose: 'a t -> 'b t -> ('a * 'b) t
val using: 'a t -> ('b -> 'a) -> 'b t
val const: 'a t -> 'a -> unit t
val nop: 'a t
val fail: string -> 'a t
val fix: ('a t -> 'a t) -> 'a t
val prefix: unit t -> 'a t -> 'a t
val suffix: unit t -> 'a t -> 'a t
val satisfy: (char -> bool) -> char t
val while0: (char -> bool) -> string t
val while1: (char -> bool) -> string t
val bwhile0: (char -> bool) -> bigstring t
val bwhile1: (char -> bool) -> bigstring t
val take: int -> string t
val flush: 'a t -> 'a t
val newline: unit t
val between: unit t -> unit t -> 'a t -> 'a t

exception Fail of string

val (>>|): 'a t -> ('b -> 'a) -> 'b t
val (<$>): ('b -> 'a) -> 'a t -> 'b t
val (<!>): 'a t -> 'a -> unit t
val (<|>): 'a t -> 'a t -> 'a t
val (<*>): 'a t -> 'b t -> ('a * 'b) t
val ( *>): unit t -> 'a t -> 'a t
val (<* ): 'a t -> unit t -> 'a t

val keval: (encoder -> 'r state) -> (iovecs -> int) -> encoder -> 'v t -> 'v -> 'r
val eval: (iovecs -> int) -> encoder -> 'v t -> 'v -> unit
val to_string: 'v t -> 'v -> string

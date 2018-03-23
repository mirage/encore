type vec =
  { off : int option
  ; len : int option }

type 'a state  = 'a Lole.state
type encoder   = Lole.encoder
type bigstring = Lole.bigstring
type iovecs    = Lole.IOVec.t list

type -'a t
type -'a s

val peek: 'a t -> 'b t -> ('a, 'b) Either.t t
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
val nop: 'a t
val option: 'a t -> 'a option t

exception Fail of string

val pure: compare:('a -> 'a -> int) -> 'a -> 'a t
val fail: string -> 'a t
val const: string -> string t

val (<|>): 'a t -> 'a t -> 'a t
val (<$>): ('a -> 'b) -> 'b t -> 'a t
val (<*>): 'a t -> 'b t -> ('a * 'b) t

val prefix: unit t -> 'a t -> 'a t
val suffix: unit t -> 'a t -> 'a t

val while0: (char -> bool) -> string t
val while1: (char -> bool) -> string t
val bigstring_while0: (char -> bool) -> bigstring t
val bigstring_while1: (char -> bool) -> bigstring t

val take: int -> string t
val buffer: string t
val bigstring_buffer: bigstring t

val ( *>): unit t -> 'a t -> 'a t
val (<* ): 'a t -> unit t -> 'a t

val fix: ('a t -> 'a t) -> 'a t
val commit: unit t

val keval: (encoder -> 'r state) -> (iovecs -> int) -> encoder -> 'v t -> 'v -> 'r
val eval: (iovecs -> int) -> encoder -> 'v t -> 'v -> unit
val to_string: 'v t -> 'v -> string

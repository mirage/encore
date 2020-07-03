type state = Partial of partial | Done | Fail

and partial = {
  buffer : Bigstringaf.t;
  off : int;
  len : int;
  continue : committed:int -> state;
}

type encoder

type -'a t

val emit : 'a -> 'a t -> state

val emit_string : ?chunk:int -> 'a -> 'a t -> string

val char : char t

val string : string -> string t

val pure : compare:('a -> 'a -> bool) -> 'a -> 'a t

val choose : 'a t -> 'a t -> 'a t

val put_while0 : (char -> bool) -> string t

val put_while1 : (char -> bool) -> string t

val put : (char -> bool) -> int -> string t

val at_least_put : (char -> bool) -> int -> string t

val range : a:int -> b:int -> (char -> bool) -> string t

val ( <* ) : 'a t -> unit t -> 'a t

val ( *> ) : unit t -> 'a t -> 'a t

val product : 'a t -> 'b t -> ('a * 'b) t

val fail : string -> 'a t

val fix : ('a t -> 'a t) -> 'a t

val map : 'a t -> ('b -> 'a) -> 'b t

val commit : unit t

val peek : 'a t -> 'b t -> ('a, 'b) Either.t t

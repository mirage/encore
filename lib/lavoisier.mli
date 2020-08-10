(** Type of lavoisier's state.

    The common way to use it is to fill a {!Buffer.t} when we get the [Partial]
    case:

    {[
      let rec go = function
        | Partial { buffer; off; len; continue } ->
            let str = Bigstringaf.substring buffer ~off ~len in
            Buffer.add_string buf str ;
            go (continue ~committed:len)
        | Done -> Buffer.contents buf
        | Fail -> failwith "serialization" in
      go (emit v d)
    ]} *)
type state = Partial of partial | Done | Fail

and partial = {
  buffer : Bigstringaf.t;
  off : int;
  len : int;
  continue : committed:int -> state;
}

type -'a t
(** A serializer for values of type ['a]. *)

val emit : ?chunk:int -> 'a -> 'a t -> state

val emit_string : ?chunk:int -> 'a -> 'a t -> string
(** [emit_string ?chunk v t] runs [t] with [v]. The serializer allocates an
    internal buffer of [chunk] byte(s) (default to 4096 bytes) and enlarge it if
    it's needed. *)

val char : char t
(** [char] accepts any character and emit it. *)

val string : string -> string t
(** [string str] accepts {b only} a string which is equal to [str] and emits it.
    Otherwise, it fails. *)

val pure : compare:('a -> 'a -> bool) -> 'a -> 'a t
(** [pure ~compare v] accepts {b only} a value ['a] equal (see [compare]) to [v]
    and emits it. Otherwise, it fails. *)

val choose : 'a t -> 'a t -> 'a t
(** [choose p q] runs [p] and emits it if succeeds. If [p] fails, then the
    output is reset and [q] will be run instead. *)

val put_while0 : (char -> bool) -> string t
(** [put_while0 p] accepts a [string] which respects the predicate [p] and emits
    it. *)

val put_while1 : (char -> bool) -> string t
(** Same as {!put_while1} but the given [string] must be not empty. *)

val put : (char -> bool) -> int -> string t
(** [put p n] accepts a [string] which respects the predicate [p] and must have
    [n] bytes. Then, it emits it otherwise it fails. For example, this ABNF:

    {v
DIGIT := '0' .. '9'
NUMBER := 2DIGIT
    v}

    can be translated to:

    {[ let number = put (function '0' .. '9' -> true | _ -> false) 2 ]} *)

val at_least_put : (char -> bool) -> int -> string t
(** [at_least_put p n] accepts a [string] which respects the predicate [p] and
    must have, at least, [n] bytes. Then, it emits it otherwise it fails. For
    example, this ABNF:

    {v
DIGIT := '0' .. '9'
NUMBER := 1*DIGIT
    v}

    can be translated to:

    {[
      let number = at_least_put (function '0' .. '9' -> true | _ -> false) 1
    ]} *)

val range : a:int -> b:int -> (char -> bool) -> string t

val ( <* ) : 'a t -> unit t -> 'a t

val ( *> ) : unit t -> 'a t -> 'a t

val product : 'a t -> 'b t -> ('a * 'b) t

val fail : string -> 'a t

val fix : ('a t -> 'a t) -> 'a t

val map : 'a t -> ('b -> 'a) -> 'b t

val commit : unit t

val peek : 'a t -> 'b t -> ('a, 'b) Either.t t

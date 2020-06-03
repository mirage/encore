(** {1: Encore, combinators to produce encoder and decoder.}

    The goal of [encore] is to provide combinators to be able
    to produce an [angstrom]'s parser or a [lavoisier]'s encoder.

    Combinators are more limited than what [angstrom] can provide,
    but this limitation gives a chance to us to produce a [lavoisier]'s
    encoder an be able to deserialize and serialize the value in the
    same way.

    By this fact, we can ensure the isomorphism:
    {[
      val p : ('k, v) t

      val angstrom : v Angstrom.t   (* = to_angstrom p *)
      val lavoisier : v Lavoisier.t (* = to_lavoisier p *)

      assert (emit_string (parse_string str angstrom) lavoisier = str) ;
      assert (parse_string (emit_string v lavoisier) angstrom = v) ;
    ]}

    To be able to maje the serializer and the deserializer, the user must
    provide some {i bijective} elements. To be able to parse and encode an
    [int64], you must provide the way to get the value from a [string] and
    how you can encode your value to a [string]:

    {[
      let int64
        : (int64, string)
        = Bij.v ~fwd:Int64.of_string ~bwd:Int64.to_string
    ]}

    Then, you are able to play with combinators such as:

    {[
      let p =
        let open Syntax in
        int64 <$> while1 is_digit
    ]}

    For some values such as Git values, we must respect {i isomorphism} to ensure
    to inject/extract exactly the same representation of it into a store - and
    ensure by this way that we will produce exactly the same hash.
*)

module Bij : sig
  type ('a, 'b) t

  val v : fwd:('a -> 'b) -> bwd:('b -> 'a) -> ('a, 'b) t

  val bwd : ('a, 'b) t -> 'b -> 'a

  val fwd : ('a, 'b) t -> 'a -> 'b

  val flip : ('a, 'b) t -> ('b, 'a) t

  val product : ('u, 'v) t -> ('x, 'y) t -> ('u * 'x, 'v * 'y) t

  val compose : ('a, 'a) t -> ('a, 'a) t -> ('a, 'a) t

  val commute : ('a * 'b, 'b * 'a) t

  val identity : ('a, 'a) t

  val element : ('a -> bool) -> ('a, 'a) t

  val cons : ('a * 'a list, 'a list) t

  val char : char -> (char, unit) t

  val string : string -> (string, unit) t

  val some : ('a, 'a option) t
end

module Lavoisier = Lavoisier

type 'a t

val to_angstrom : 'a t -> 'a Angstrom.t

val to_lavoisier : 'a t -> 'a Lavoisier.t

module Syntax : sig
  val fail : string -> 'a t

  val map : ('a, 'b) Bij.t -> 'a t -> 'b t

  val product : 'a t -> 'b t -> ('a * 'b) t

  val commit : unit t

  val fix : ('a t -> 'a t) -> 'a t

  val pure : compare:('a -> 'a -> bool) -> 'a -> 'a t

  val peek : 'a t -> 'b t -> ('a, 'b) Either.t t

  val const : string -> string t

  val any : char t

  val nil : 'a list t

  val rep1 : 'a t -> 'a list t

  val rep0 : 'a t -> 'a list t

  val sep_by0 : sep:unit t -> 'a t -> 'a list t

  val sep_by1 : sep:unit t -> 'a t -> 'a list t

  val while0 : (char -> bool) -> string t

  val while1 : (char -> bool) -> string t

  val fixed : int -> string t

  val choice : 'a t list -> 'a t

  val sequence : 'a t list -> 'a list t

  val count : int -> 'a t -> 'a list t

  val option : 'a t -> 'a option t

  val lower : char t

  val upper : char t

  val alpha : char t

  val digit : char t

  val ( <$> ) : ('a, 'b) Bij.t -> 'a t -> 'b t

  val ( <|> ) : 'a t -> 'a t -> 'a t

  val ( *> ) : unit t -> 'a t -> 'a t

  val ( <* ) : 'a t -> unit t -> 'a t

  val ( <*> ) : 'a t -> 'b t -> ('a * 'b) t
end

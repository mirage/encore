module type S =
sig
  type 'a t

  val ( <$> ) : ('a, 'b) Bijection.texn -> 'a t -> 'b t
  val ( <*> ) : 'a t -> 'b t -> ('a * 'b) t
  val ( <|> ) : 'a t -> 'a t -> 'a t

  val ( *> )  : unit t -> 'a t -> 'a t
  val ( <* )  : 'a t -> unit t -> 'a t

  val ( $> )  : unit t -> (unit, 'a) Bijection.texn -> 'a t
  val ( <$ )  : 'a t -> (unit, 'a) Bijection.texn -> unit t

  val fix     : ('a t -> 'a t) -> 'a t

  val char    : char t
  val satisfy : (char -> bool) -> char t
  val between : unit t -> unit t -> 'a t -> 'a t
  val option  : 'a t -> 'a option t
  val while0  : (char -> bool) -> string t
  val while1  : (char -> bool) -> string t
  val take    : int -> string t
  val list    : ?sep:unit t -> 'a t -> 'a list t
  val string  : string -> string t
  val bwhile0 : (char -> bool) -> Condorcet.bigstring t
  val bwhile1 : (char -> bool) -> Condorcet.bigstring t
  val nop     : unit t

  module Option:
  sig
    val (<$>) : ('a, 'b) Bijection.topt -> 'a t -> 'b t
    val ( $>) : unit t -> (unit, 'a) Bijection.topt -> 'a t
    val (<$ ) : 'a t -> (unit, 'a) Bijection.topt -> unit t
  end
end

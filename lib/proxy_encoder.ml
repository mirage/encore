module Impl : Meta.S with type 'a t = 'a Encoder.t = struct
  type 'a t = 'a Encoder.t

  let ( <$> ) (bijection : ('a, 'b) Bijection.texn) p =
    let open Encoder in
    bijection.Bijection.of_ <$> p

  let ( <*> ) pa pb =
    let open Encoder in
    pa <*> pb

  let ( <|> ) pu pv =
    let open Encoder in
    pu <|> pv

  let ( *> ) pu pe =
    let open Encoder in
    pu *> pe

  let ( <* ) pe pu =
    let open Encoder in
    pe <* pu

  let ( <$ ) pu bijection =
    let open Encoder in
    bijection.Bijection.to_ <$> pu

  let ( $> ) pe bijection =
    let open Encoder in
    bijection.Bijection.of_ <$> pe

  let fix = Encoder.fix
  let nop = Encoder.nop
  let any = Encoder.char
  let fail err = Encoder.fail err
  let pure ~compare v = Encoder.pure ~compare v
  let take = Encoder.take
  let peek = Encoder.peek
  let const s = Encoder.const s
  let commit = Encoder.commit
  let while0 predicate = Encoder.while0 predicate
  let while1 predicate = Encoder.while1 predicate
  let bigstring_while0 predicate = Encoder.bigstring_while0 predicate
  let bigstring_while1 predicate = Encoder.bigstring_while1 predicate
  let buffer = Encoder.buffer
  let bigstring_buffer = Encoder.bigstring_buffer

  module Option = struct
    let ( <$> ) bijection p = Bijection.Exn.of_option bijection <$> p

    let ( $> ) : unit t -> (unit, 'a) Bijection.topt -> 'a t =
     fun pe bijection -> pe $> Bijection.Exn.of_option bijection

    let ( <$ ) : 'a t -> (unit, 'a) Bijection.topt -> unit t =
     fun pu bijection -> pu <$ Bijection.Exn.of_option bijection
  end
end

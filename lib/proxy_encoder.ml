module Impl : Meta.S with type 'a t = 'a Encoder.t =
struct
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
  let char = Encoder.char
  let skip _ = Encoder.nop
  let pure ~compare v = Encoder.pure ~compare v
  let fail err = Encoder.fail err
  let satisfy = Encoder.satisfy
  let between = Encoder.between
  let option = Encoder.option
  let while1 = Encoder.while1
  let while0 = Encoder.while0
  let take = Encoder.take
  let list = Encoder.list
  let string e =
    Bijection.make_exn ~tag:(e, "unit")
      ~fwd:(fun s ->
          if String.equal s e then s else Bijection.Exn.fail s e)
      ~bwd:(fun s ->
          if String.equal s e then s else Bijection.Exn.fail s e)
    <$> Encoder.string
  let nop = Encoder.nop
  let bwhile1 = Encoder.bwhile1
  let bwhile0 = Encoder.bwhile0

  module Option =
  struct
    let (<$>) bijection p =
      let open Encoder in
      using p
        (fun x -> match bijection.Bijection.of_ x with
           | Some x -> x
           | None -> Bijection.Exn.fail "'a" "unit")

    let ( $>)
      : unit t -> (unit, 'a) Bijection.topt -> 'a t
      = fun pe bijection ->
      let open Encoder in
      using pe
        (fun x ->
           let open Bijection in
           match bijection.of_ x, bijection.kd with
           | Some x, O -> x
           | None, O -> Bijection.Exn.fail "'a" "unit")

    let (<$ )
      : 'a t -> (unit, 'a) Bijection.topt -> unit t
      = fun pu bijection ->
      let open Encoder in
      using pu
        (fun x ->
           let open Bijection in
           match bijection.to_ x, bijection.kd with
           | Some x, O -> x
           | None, O -> Bijection.Exn.fail "'a" "unit")
  end
end

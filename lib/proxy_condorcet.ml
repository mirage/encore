module Impl : Meta.S with type 'a t = 'a Condorcet.t =
struct
  type 'a t = 'a Condorcet.t

  let ( <$> ) (bijection : ('a, 'b) Bijection.texn) p =
    let open Condorcet in
    bijection.Bijection.of_ <$> p

  let ( <*> ) pa pb =
    let open Condorcet in
    pa <*> pb

  let ( <|> ) pu pv =
    let open Condorcet in
    pu <|> pv

  let ( *> ) pu pe =
    let open Condorcet in
    pu *> pe

  let ( <* ) pe pu =
    let open Condorcet in
    pe <* pu

  let ( <$ ) pu bijection =
    let open Condorcet in
    bijection.Bijection.to_ <$> pu

  let ( $> ) pe bijection =
    let open Condorcet in
    bijection.Bijection.of_ <$> pe

  let fix = Condorcet.fix

  let char = Condorcet.char
  let satisfy = Condorcet.satisfy
  let between = Condorcet.between
  let option = Condorcet.option
  let while1 = Condorcet.while1
  let while0 = Condorcet.while0
  let take = Condorcet.take
  let list = Condorcet.list
  let string e =
    Bijection.make_exn ~tag:(e, "unit")
      ~fwd:(fun s ->
          if String.equal s e then s else Bijection.fail s e)
      ~bwd:(fun s ->
          if String.equal s e then s else Bijection.fail s e)
    <$> Condorcet.string
  let nop = Condorcet.nop
  let bwhile1 = Condorcet.bwhile1
  let bwhile0 = Condorcet.bwhile0

  module Option =
  struct
    let (<$>) bijection p =
      let open Condorcet in
      using p
        (fun x -> match bijection.Bijection.of_ x with
           | Some x -> x
           | None -> Bijection.fail "'a" "unit")

    let ( $>)
      : unit t -> (unit, 'a) Bijection.topt -> 'a t
      = fun pe bijection ->
      let open Condorcet in
      using pe
        (fun x ->
           let open Bijection in
           match bijection.of_ x, bijection.kd with
           | Some x, O -> x
           | None, O -> Bijection.fail "'a" "unit")

    let (<$ )
      : 'a t -> (unit, 'a) Bijection.topt -> unit t
      = fun pu bijection ->
      let open Condorcet in
      using pu
        (fun x ->
           let open Bijection in
           match bijection.to_ x, bijection.kd with
           | Some x, O -> x
           | None, O -> Bijection.fail "'a" "unit")
  end
end

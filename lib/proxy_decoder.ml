module Impl : Meta.S with type 'a t = 'a Angstrom.t =
struct
  type 'a t = 'a Angstrom.t

  let ( <$> ) bijection p =
    let open Angstrom in

    (p >>= fun x ->

     try let x = bijection.Bijection.to_ x in return x
     with
     | Bijection.Bijection (to_, of_) ->
       Angstrom.fail (Fmt.strf "bijection: %s to %s" to_ of_))

  let ( <*> ) pa pb =
    let open Angstrom in
    pa >>= fun a -> pb >>= fun b -> return (a, b)

  let ( <|> ) pu pv =
    let open Angstrom in
    pu <|> pv

  let ( *> ) pu pe =
    let open Angstrom in
    pu *> pe

  let ( <* ) pe pu =
    let open Angstrom in
    pe <* pu

  let ( $> )
    : unit t -> (unit, 'a) Bijection.texn -> 'a t
    = fun pu bijection ->
      let open Angstrom in
      pu >>= fun () ->
      let open Bijection in
      match bijection.to_ (), bijection.kd with
      | x, E -> return x
      | exception Bijection.Bijection (to_, of_) ->
        Angstrom.fail (Fmt.strf "bijection: %s to %s" to_ of_)

  let ( <$ )
    : 'a t -> (unit, 'a) Bijection.texn -> unit t
    = fun pe bijection ->
      let open Angstrom in
      pe >>= fun x ->
      let open Bijection in
      match bijection.of_ x, bijection.kd with
      | x, E -> return x
      | exception Bijection.Bijection (to_, of_) ->
        Angstrom.fail (Fmt.strf "bijection: %s to %s" to_ of_)

  let fix = Angstrom.fix

  let char = Angstrom.any_char
  let satisfy = Angstrom.satisfy
  let between p s a = p *> a <* s
  let option t =
    let open Angstrom in
    ((t >>| fun v -> Some v) <|> (return None))
  let while1 = Angstrom.take_while1
  let while0 = Angstrom.take_while
  let take = Angstrom.take
  let list ?sep p = match sep with
    | Some sep -> Angstrom.sep_by sep p
    | None -> Angstrom.many p
  let string = Angstrom.string
  let nop = Angstrom.return ()
  let bwhile0 = Angstrom.take_bigstring_while
  let bwhile1 = Angstrom.take_bigstring_while1

  module Option =
  struct
    let ( <$> ) bijection p =
      let open Angstrom in
      p >>= fun x -> match bijection.Bijection.to_ x with
      | Some x -> return x
      | None -> Angstrom.fail "bijection: 'a to 'b"

    let ( $>) pu bijection =
      let open Angstrom in
      pu >>= fun () ->
      let open Bijection in
      match bijection.to_ (), bijection.kd with
      | Some x, O -> return x
      | None, O -> Angstrom.fail "bijection: unit to 'a"

    let (<$ ) pe bijection =
      let open Angstrom in
      pe >>= fun x ->
      let open Bijection in
      match bijection.of_ x, bijection.kd with
      | Some x, O -> return x
      | None, O -> Angstrom.fail "bijection: 'a to unit"
  end
end

module Impl : Meta.S with type 'a t = 'a Angstrom.t = struct
  type 'a t = 'a Angstrom.t

  let ( <$> ) bijection p =
    let open Angstrom in
    p >>= fun x ->
    try
      let x = bijection.Bijection.to_ x in
      return x
    with Bijection.Exn.Bijection -> Angstrom.fail "bijection"

  let ( <*> ) pa pb =
    let open Angstrom in
    pa >>= fun a ->
    pb >>= fun b -> return (a, b)

  let ( <|> ) pu pv =
    let open Angstrom in
    pu <|> pv

  let ( *> ) pu pe =
    let open Angstrom in
    pu *> pe

  let ( <* ) pe pu =
    let open Angstrom in
    pe <* pu

  let ( $> ) : unit t -> (unit, 'a) Bijection.texn -> 'a t =
   fun pu bijection ->
    let open Angstrom in
    pu >>= fun () ->
    let open Bijection in
    match (bijection.to_ (), bijection.kd) with
    | x, E -> return x
    | exception Bijection.Exn.Bijection -> Angstrom.fail "bijection"

  let ( <$ ) : 'a t -> (unit, 'a) Bijection.texn -> unit t =
   fun pe bijection ->
    let open Angstrom in
    pe >>= fun x ->
    let open Bijection in
    match (bijection.of_ x, bijection.kd) with
    | x, E -> return x
    | exception Bijection.Exn.Bijection -> Angstrom.fail "bijection"

  let fix = Angstrom.fix

  let nop = Angstrom.return ()

  let any = Angstrom.any_char

  let fail err = Angstrom.fail err

  let pure ~compare:_ v = Angstrom.return v

  let take = Angstrom.take

  let peek a b =
    let open Angstrom in
    peek_char >>= function
    | Some _ -> a >>| fun x -> Either.L x
    | None -> b >>| fun y -> Either.R y

  let const s = Angstrom.(string s <?> s)

  let commit = Angstrom.commit

  let while0 = Angstrom.take_while

  let while1 = Angstrom.take_while1

  let bigstring_while0 = Angstrom.take_bigstring_while

  let bigstring_while1 = Angstrom.take_bigstring_while1

  let buffer =
    let open Angstrom in
    available >>= take

  let bigstring_buffer =
    let open Angstrom in
    available >>= take_bigstring

  module Option = struct
    let ( <$> ) bijection p =
      let open Angstrom in
      p >>= fun x ->
      match bijection.Bijection.to_ x with
      | Some x -> return x
      | None -> Angstrom.fail "bijection"

    let ( $> ) pu bijection =
      let open Angstrom in
      pu >>= fun () ->
      let open Bijection in
      match (bijection.to_ (), bijection.kd) with
      | Some x, O -> return x
      | None, O -> Angstrom.fail "bijection"

    let ( <$ ) pe bijection =
      let open Angstrom in
      pe >>= fun x ->
      let open Bijection in
      match (bijection.of_ x, bijection.kd) with
      | Some x, O -> return x
      | None, O -> Angstrom.fail "bijection"
  end
end

module Lavoisier = Lavoisier
module Either = Either
module Bij = Bij

let always x _ = x

type 'a t =
  | Product : 'a t * 'b t -> ('a * 'b) t
  | Map : ('a, 'b) Bij.t * 'a t -> 'b t
  | Either : 'a t * 'a t -> 'a t
  | Fix : ('a t -> 'a t) -> 'a t
  | Symbol : char t
  | IgnR : 'a t * unit t -> 'a t
  | IgnL : unit t * 'b t -> 'b t
  | Fail : string -> 'a t
  | Payload : (char -> bool) * int * n -> string t
  | Const : string -> string t
  | Commit : unit t
  | Pure : ('a -> 'a -> bool) * 'a -> 'a t
  | Peek : 'a t * 'b t -> ('a, 'b) Either.t t
  | Unsafe : ('a, 'k) v -> 'a t

and n = Infinite | Fixed of int

and ('a, 'k) v =
  | Angstrom : 'a Angstrom.t -> ('a, angstrom) v
  | Lavoisier : 'a Lavoisier.t -> ('a, lavoisier) v

and angstrom = |

and lavoisier = |

let take_while_with_max ~max p =
  let open Angstrom in
  scan_string 0 (fun n chr -> if p chr && n < max then Some (succ n) else None)

let string_for_all f x =
  let rec go a i =
    if i < String.length x then go (f x.[i] && a) (succ i) else a in
  go true 0

let rec to_angstrom : type a. a t -> a Angstrom.t = function
  | Product (a, b) ->
      let pa = to_angstrom a in
      let pb = to_angstrom b in
      Angstrom.(
        pa >>= fun a ->
        pb >>= fun b -> return (a, b))
  | Map (bij, x) -> (
      let px = to_angstrom x in
      Angstrom.(
        px >>= fun x ->
        try return (bij.Bij.to_ x) with Bij.Bijection -> fail "bijection"))
  | Either (a, b) ->
      let pa = to_angstrom a in
      let pb = to_angstrom b in
      Angstrom.(pa <|> pb)
  | Fix f -> Angstrom.fix @@ fun m -> to_angstrom (f (Unsafe (Angstrom m)))
  | Const str -> Angstrom.string str
  | Unsafe (Angstrom p) -> p
  | Unsafe (Lavoisier _) -> assert false
  | Symbol -> Angstrom.any_char
  | Pure (_compare, v) -> Angstrom.return v
  | Payload (p, 0, Infinite) -> Angstrom.take_while p
  | IgnL (p, q) ->
      let p = to_angstrom p in
      let q = to_angstrom q in
      Angstrom.(p *> q)
  | IgnR (p, q) ->
      let p = to_angstrom p in
      let q = to_angstrom q in
      Angstrom.(p <* q)
  | Payload (p, 1, Infinite) -> Angstrom.take_while1 p
  | Payload (p, a, Fixed b) ->
      Angstrom.(
        take a >>= fun v ->
        if string_for_all p v
        then take_while_with_max ~max:(b - a) p >>= fun w -> return (v ^ w)
        else fail "Invalid payload")
  | Payload (p, a, Infinite) ->
      Angstrom.(
        take a >>= fun v ->
        if string_for_all p v
        then take_while p >>= fun w -> return (v ^ w)
        else fail "Invalid payload")
  | Peek (a, b) -> (
      let pa = to_angstrom a in
      let pb = to_angstrom b in
      Angstrom.(
        peek_char >>= function
        | Some _ -> pa >>| fun x -> Either.L x
        | None -> pb >>| fun y -> Either.R y))
  | Fail err -> Angstrom.fail err
  | Commit -> Angstrom.commit

let rec to_lavoisier : type a. a t -> a Lavoisier.t = function
  | Product (a, b) ->
      let da = to_lavoisier a in
      let db = to_lavoisier b in
      Lavoisier.product da db
  | Map (bij, x) ->
      let dx = to_lavoisier x in
      Lavoisier.map dx bij.of_
  | Commit -> Lavoisier.commit
  | Either (a, b) ->
      let da = to_lavoisier a in
      let db = to_lavoisier b in
      Lavoisier.choose da db
  | Symbol -> Lavoisier.char
  | Payload (p, 0, Infinite) -> Lavoisier.put_while0 p
  | Payload (p, 1, Infinite) -> Lavoisier.put_while1 p
  | Payload (p, a, Fixed b) -> Lavoisier.range ~a ~b p
  | Payload (p, a, Infinite) -> Lavoisier.at_least_put p a
  | Const str -> Lavoisier.string str
  | IgnL (p, q) ->
      let p = to_lavoisier p in
      let q = to_lavoisier q in
      Lavoisier.(p *> q)
  | IgnR (p, q) ->
      let p = to_lavoisier p in
      let q = to_lavoisier q in
      Lavoisier.(p <* q)
  | Pure (compare, v) -> Lavoisier.pure ~compare v
  | Peek (a, b) ->
      let da = to_lavoisier a in
      let db = to_lavoisier b in
      Lavoisier.peek da db
  | Fail err -> Lavoisier.fail err
  | Unsafe (Lavoisier d) -> d
  | Unsafe (Angstrom _) -> assert false
  | Fix f -> Lavoisier.fix @@ fun m -> to_lavoisier (f (Unsafe (Lavoisier m)))

module Syntax = struct
  let fail err = Fail err

  let map f x = Map (f, x)

  let product p q = Product (p, q)

  let ( <$> ) f x = map f x

  let ( <|> ) p q = Either (p, q)

  let ( *> ) p q = IgnL (p, q)

  let ( <* ) p q = IgnR (p, q)

  let ( <*> ) p q = Product (p, q)

  let fix f = Fix f

  let const str = Const str

  let any = Symbol

  let while1 p = Payload (p, 1, Infinite)

  let while0 p = Payload (p, 0, Infinite)

  let nil =
    Pure ((fun l0 l1 -> match (l0, l1) with [], [] -> true | _ -> false), [])

  let none =
    Pure
      ( (fun o0 o1 -> match (o0, o1) with None, None -> true | _ -> false),
        None )

  let commit = Commit

  let rep1 p = fix @@ fun m -> Bij.cons <$> (p <*> (m <|> nil))

  let rep0 p = rep1 p <|> nil

  let sep_by1 ~sep p = Bij.cons <$> (p <*> rep0 (sep *> p))

  let sep_by0 ~sep p = sep_by1 ~sep p <|> nil

  let pure ~compare v = Pure (compare, v)

  let peek a b = Peek (a, b)

  let fixed n = Payload (always true, n, Fixed n)

  let identity x = x

  let rec fold_right ~k f l a =
    match l with
    | [] -> k a
    | x :: r -> (fold_right [@tailcall]) ~k:(fun r -> k (f x r)) f r a

  let choice l = fold_right ~k:identity ( <|> ) l (fail "choice")

  let sequence l =
    let fold x r = Bij.cons <$> (x <*> r) in
    fold_right ~k:identity fold l nil

  let count n t =
    let rec make a = function 0 -> a | n -> make (t :: a) (pred n) in
    sequence (make [] n)

  let option t = Bij.some <$> t <|> none

  let is_lower = function 'a' .. 'z' -> true | _ -> false

  let is_upper = function 'A' .. 'Z' -> true | _ -> false

  let is_digit = function '0' .. '9' -> true | _ -> false

  let is_alpha = function 'a' .. 'z' -> true | 'A' .. 'Z' -> true | _ -> false

  let lower = Bij.element is_lower <$> any

  let upper = Bij.element is_upper <$> any

  let alpha = Bij.element is_alpha <$> any

  let digit = Bij.element is_digit <$> any
end

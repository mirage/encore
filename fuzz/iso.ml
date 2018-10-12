open Encore

module Proxy =
struct
  type 'a t =
    | Cast    : ('a, 'b) Bijection.texn * 'a t -> 'b t
    | CastOpt : ('a, 'b) Bijection.topt * 'a t -> 'b t
    | Concat  : 'a t * 'b t -> ('a * 'b) t
    | Choose  : 'a t * 'a t -> 'a t
    | IgnL    : unit t * 'a t -> 'a t
    | IgnR    : 'a t * unit t -> 'a t
    | AppL    : unit t * (unit, 'a) Bijection.texn -> 'a t
    | AppLOpt : unit t * (unit, 'a) Bijection.topt -> 'a t
    | AppR    : 'a t * (unit, 'a) Bijection.texn -> unit t
    | AppROpt : 'a t * (unit, 'a) Bijection.topt -> unit t
    | Fix     : ('a t -> 'a t) -> 'a t
    | Nop     : unit t
    | Any     : char t
    | Fail    : string -> 'a t
    | Pure    : ('a -> 'a -> int) * 'a -> 'a t
    | Take    : int -> string t
    | Peek    : 'a t * 'b t -> ('a, 'b) Either.t t
    | Const   : string -> string t
    | Commit  : unit t
    | While0  : (char -> bool) -> string t
    | While1  : (char -> bool) -> string t
    | BWhile0 : (char -> bool) -> Encoder.bigstring t
    | BWhile1 : (char -> bool) -> Encoder.bigstring t
    | Buffer  : string t
    | BBuffer : Encoder.bigstring t

  let ( <$> ) bijection t = Cast (bijection, t)
  let ( <*> ) ta tb = Concat (ta, tb)
  let ( <|> ) ta tb = Choose (ta, tb)
  let (  *> ) tu ta = IgnL (tu, ta)
  let ( <*  ) ta tu = IgnR (ta, tu)
  let (  $> ) tu bijection = AppL (tu, bijection)
  let ( <$  ) ta bijection = AppR (ta, bijection)

  let fix f = Fix f
  let nop = Nop
  let any = Any

  let fail err = Fail err
  let pure ~compare v = Pure (compare, v)
  let take n = Take n
  let peek ta tb = Peek (ta, tb)

  let const s = Const s

  let commit = Commit

  let while0 predicate = While0 predicate
  let while1 predicate = While1 predicate
  let bigstring_while0 predicate = BWhile0 predicate
  let bigstring_while1 predicate = BWhile1 predicate

  let buffer = Buffer
  let bigstring_buffer = BBuffer

  let rec to_encoder : type a. a t -> a Encore.Proxy_encoder.Impl.t = fun x ->
    let open Encore.Proxy_encoder.Impl in

    match x with
    | Cast (bijection, t)     -> bijection <$> (to_encoder t)
    | CastOpt (bijection, t)  -> Option.(bijection <$> (to_encoder t))
    | Concat (ta, tb)         -> (to_encoder ta) <*> (to_encoder tb)
    | Choose (ta, tb)         -> (to_encoder ta) <|> (to_encoder tb)
    | IgnL (tu, ta)           -> (to_encoder tu) *> (to_encoder ta)
    | IgnR (ta, tu)           -> (to_encoder ta) <* (to_encoder tu)
    | AppL (tu, bijection)    -> (to_encoder tu) $> bijection
    | AppLOpt (tu, bijection) -> Option.((to_encoder tu) $> bijection)
    | AppR (ta, bijection)    -> (to_encoder ta) <$ bijection
    | AppROpt (ta, bijection) -> Option.((to_encoder ta) <$ bijection)
    | Nop                     -> nop
    | Any                     -> any
    | Fail err                -> fail err
    | Pure (compare, v)       -> pure ~compare v
    | Take n                  -> take n
    | Peek (ta, tb)           -> peek (to_encoder ta) (to_encoder tb)
    | Const s                 -> const s
    | Commit                  -> commit
    | While0 p                -> while0 p
    | While1 p                -> while1 p
    | BWhile0 p               -> bigstring_while0 p
    | BWhile1 p               -> bigstring_while1 p
    | Buffer                  -> buffer
    | BBuffer                 -> bigstring_buffer
    | Fix _                   -> invalid_arg "unavailable fix"

  let rec to_decoder : type a. a t -> a Encore.Proxy_decoder.Impl.t = fun x ->
    let open Encore.Proxy_decoder.Impl in

    match x with
    | Cast (bijection, t)     -> bijection <$> (to_decoder t)
    | CastOpt (bijection, t)  -> Option.(bijection <$> (to_decoder t))
    | Concat (ta, tb)         -> (to_decoder ta) <*> (to_decoder tb)
    | Choose (ta, tb)         -> (to_decoder ta) <|> (to_decoder tb)
    | IgnL (tu, ta)           -> (to_decoder tu) *> (to_decoder ta)
    | IgnR (ta, tu)           -> (to_decoder ta) <* (to_decoder tu)
    | AppL (tu, bijection)    -> (to_decoder tu) $> bijection
    | AppLOpt (tu, bijection) -> Option.((to_decoder tu) $> bijection)
    | AppR (ta, bijection)    -> (to_decoder ta) <$ bijection
    | AppROpt (ta, bijection) -> Option.((to_decoder ta) <$ bijection)
    | Nop                     -> nop
    | Any                     -> any
    | Fail err                -> fail err
    | Pure (compare, v)       -> pure ~compare v
    | Take n                  -> take n
    | Peek (ta, tb)           -> peek (to_decoder ta) (to_decoder tb)
    | Const s                 -> const s
    | Commit                  -> commit
    | While0 p                -> while0 p
    | While1 p                -> while1 p
    | BWhile0 p               -> bigstring_while0 p
    | BWhile1 p               -> bigstring_while1 p
    | Buffer                  -> buffer
    | BBuffer                 -> bigstring_buffer
    | Fix _                   -> invalid_arg "unavailable fix"

  module Option =
  struct
    let ( <$> ) bijection t = CastOpt (bijection, t)
    let (  $> ) tu bijection = AppLOpt (tu, bijection)
    let ( <$  ) ta bijection = AppROpt (ta, bijection)
  end
end

module Meta =
struct
  include Encore.Meta.Make(Proxy)

  type witness = V : 'a t -> witness

  let wrap t = V t
end

let (<.>) f g = fun x -> f (g x)
let int_equal : int -> int -> bool = fun a b -> a = b

let string_meta_gen : string Meta.t Crowbar.gen =
  let eq chr : int -> bool = (=) (Char.code chr) in

  Crowbar.choose
    [ Crowbar.(map [ int ]) Meta.take
    ; Crowbar.(map [ bytes ]) Meta.const
    ; Crowbar.const Meta.buffer
    ; Crowbar.(map [ list int8 ]) (fun l -> let predicate chr = List.exists (eq chr) l in Meta.while0 predicate)
    ; Crowbar.(map [ list int8 ]) (fun l -> let predicate chr = List.exists (eq chr) l in Meta.while1 predicate) ]

let bigstring_meta_gen : Encoder.bigstring Meta.t Crowbar.gen =
  let eq chr : int -> bool = (=) (Char.code chr) in

  Crowbar.choose
    [ Crowbar.const Meta.bigstring_buffer
    ; Crowbar.(map [ list int8 ]) (fun l -> let predicate chr = List.exists (eq chr) l in Meta.bigstring_while0 predicate)
    ; Crowbar.(map [ list int8 ]) (fun l -> let predicate chr = List.exists (eq chr) l in Meta.bigstring_while1 predicate) ]

let rec monomorphic_meta_gen : type a. a Meta.t -> a Meta.t Crowbar.gen
  = function
  | Proxy.Cast (bijection, t) ->
    Crowbar.map [ (monomorphic_meta_gen t) ] Meta.((<$>) bijection)
  | Proxy.CastOpt (bijection, t) ->
    Crowbar.map [ (monomorphic_meta_gen t) ] Meta.Option.((<$>) bijection)
  | Proxy.Concat (ta, tb) ->
    Crowbar.map [ (monomorphic_meta_gen ta); (monomorphic_meta_gen tb) ] Meta.(<*>)
  | Proxy.Choose (ta, tb) ->
    Crowbar.map [ (monomorphic_meta_gen ta); (monomorphic_meta_gen tb) ] Meta.(<|>)
  | Proxy.IgnL (tu, ta) ->
    Crowbar.map [ (monomorphic_meta_gen tu); (monomorphic_meta_gen ta) ] Meta.( *> )
  | Proxy.IgnR (ta, tu) ->
    Crowbar.map [ (monomorphic_meta_gen ta); (monomorphic_meta_gen tu) ] Meta.( <* )
  | Proxy.AppL (tu, bijection) ->
    Crowbar.map [ (monomorphic_meta_gen tu) ] Meta.(fun tu -> tu $> bijection)
  | Proxy.AppR (ta, bijection) ->
    Crowbar.map [ (monomorphic_meta_gen ta) ] Meta.(fun ta -> ta <$ bijection)
  | Proxy.AppLOpt (tu, bijection) ->
    Crowbar.map [ (monomorphic_meta_gen tu) ] Meta.Option.(fun tu -> tu $> bijection)
  | Proxy.AppROpt (ta, bijection) ->
    Crowbar.map [ (monomorphic_meta_gen ta) ] Meta.Option.(fun ta -> ta <$ bijection)
  | Proxy.Fix g -> Crowbar.const (Meta.fix g)
  | Proxy.Nop ->
    Crowbar.(choose [ const Meta.nop; const Meta.commit; ])
  | Proxy.Commit ->
    Crowbar.(choose [ const Meta.nop; const Meta.commit; ])
  | Proxy.Any ->
    Crowbar.(map [ option int8 ]) (function
        | None -> Meta.any
        | Some chr -> Meta.pure ~compare:Char.compare (Char.unsafe_chr chr))
  | Proxy.Fail err -> Crowbar.(map [ bytes ]) Meta.fail
  | Proxy.Pure (compare, value) -> Crowbar.const (Meta.pure ~compare value)
  | Proxy.Take _   -> string_meta_gen
  | Proxy.Const _  -> string_meta_gen
  | Proxy.While0 _ -> string_meta_gen
  | Proxy.While1 _ -> string_meta_gen
  | Proxy.Buffer   -> string_meta_gen
  | Proxy.BBuffer   -> bigstring_meta_gen
  | Proxy.BWhile0 _ -> bigstring_meta_gen
  | Proxy.BWhile1 _ -> bigstring_meta_gen
  | Proxy.Peek (ta, tb) -> Crowbar.const (Meta.peek ta tb)

type wwitness = VV : ('a Meta.t * 'a Meta.t) -> wwitness
(* XXX(dinosaure): this trick! *)

let meta_gen =
  let ( >>= ) = Crowbar.dynamic_bind in

  Crowbar.fix @@ fun meta_gen ->
  Crowbar.choose
    [ Crowbar.const Meta.(wrap lower)
    ; Crowbar.const Meta.(wrap upper)
    ; Crowbar.const Meta.(wrap alpha)
    ; Crowbar.const Meta.(wrap digit)
    ; Crowbar.const Meta.(wrap nop)
    ; Crowbar.const Meta.(wrap any)
    ; Crowbar.const Meta.(wrap commit)
    ; Crowbar.(map [ int ]) Meta.(wrap <.> take)
    ; Crowbar.(map [ meta_gen; meta_gen ]) Meta.(fun (V ta) (V tb) -> wrap (peek ta tb))
    ; Crowbar.(map [ bytes ]) Meta.(wrap <.> const)
    ; Crowbar.(map [ meta_gen; meta_gen ]) Meta.(fun (V ta) (V tb) -> wrap (ta <*> tb))
    ; Crowbar.(map [ (meta_gen >>=
                      fun (Meta.V ta) ->
                      map [ monomorphic_meta_gen ta
                          ; monomorphic_meta_gen ta; ]
                        (fun a b -> VV (a, b))) ])
        Meta.(fun (VV (ta, tb)) -> wrap (ta <|> tb))
    ; Crowbar.map [ meta_gen ] Meta.(fun (V t) -> wrap (option t))
    ; Crowbar.(map [ int; meta_gen ]) Meta.(fun n (V t) -> wrap (count n t))
    ; Crowbar.map [ meta_gen ] Meta.(fun (V t) -> wrap (rep0 t))
    ; Crowbar.map [ meta_gen ] Meta.(fun (V t) -> wrap (rep1 t))
    ; Crowbar.map [ monomorphic_meta_gen Proxy.nop; meta_gen ] Meta.(fun sep (V t) -> wrap (sep_by0 ~sep t))
    ; Crowbar.map [ monomorphic_meta_gen Proxy.nop; meta_gen ] Meta.(fun sep (V t) -> wrap (sep_by1 ~sep t))
    ; Crowbar.map [ monomorphic_meta_gen Proxy.nop; meta_gen ] Meta.(fun sep (V t) -> wrap (end_by0 ~sep t))
    ; Crowbar.map [ monomorphic_meta_gen Proxy.nop; meta_gen ] Meta.(fun sep (V t) -> wrap (end_by1 ~sep t))
    ; Crowbar.map [ monomorphic_meta_gen Proxy.nop; meta_gen ] Meta.(fun tu (V t) -> wrap (tu *> t))
    ; Crowbar.map [ meta_gen; monomorphic_meta_gen Proxy.nop ] Meta.(fun (V t) tu -> wrap (t <* tu))
    ; Crowbar.(map [ int8 ]) Meta.(fun chr -> wrap (pure ~compare:Char.compare (Char.unsafe_chr chr)))
    ; Crowbar.map [ string_meta_gen ] Meta.wrap
    ; Crowbar.map [ bigstring_meta_gen ] Meta.wrap ]

let bigstring_of_string s =
  let ln = String.length s in
  let cs = Bigarray.Array1.create Bigarray.Char Bigarray.c_layout ln in
  for i = 0 to ln - 1
  do Bigarray.Array1.set cs i (String.get s i) done; cs

let () = Random.self_init ()

exception Found of int

let predicate_gen p =
  let ( >>= ) = Crowbar.dynamic_bind in
  let has bs = try String.iteri (fun idx chr -> if p chr then raise (Found idx)) bs; None with Found idx -> Some idx in
  let buffer = Buffer.create 16 in
  Crowbar.(map [ fix @@ fun go -> bytes >>= fun bs -> match has bs with
    | Some 0 -> Crowbar.const ()
    | Some idx -> Buffer.add_substring buffer bs 0 (idx - 1); Crowbar.const ()
    | None -> Buffer.add_string buffer bs; go ])
    (fun () -> Buffer.contents buffer)

let predicate_gen p =
  let has bs = try String.iteri (fun idx chr -> if p chr then raise (Found idx)) bs; None with Found idx -> Some idx in
  Crowbar.(map [ bytes ]) (fun bs -> match has bs with
      | Some 0 -> ""
      | Some idx -> String.sub bs 0 (idx - 1)
      | None -> bs)

let rec value_gen : type a. a Meta.t -> a Crowbar.gen
  = function
    | Proxy.Cast (bijection, t) ->
      Crowbar.map [ value_gen t ]
        (fun t ->
           try bijection.Bijection.to_ t
           with _ -> Crowbar.bad_test ())
    | Proxy.CastOpt (bijection, t) ->
      Crowbar.map [ value_gen t ] (fun v ->
          match bijection.Bijection.to_ v with
          | Some v -> v
          | None -> Crowbar.bad_test ())
    | Proxy.Concat (ta, tb) ->
      Crowbar.map [ value_gen ta; value_gen tb ] (fun a b -> (a, b))
    | Proxy.Choose (ta, tb) ->
      Crowbar.choose [ value_gen ta; value_gen tb ]
    | Proxy.IgnL (tu, ta) -> value_gen ta
    | Proxy.IgnR (ta, tu) -> value_gen ta
    | Proxy.AppL (tu, bijection) ->
      Crowbar.const (bijection.Bijection.to_ ())
    | Proxy.AppR (ta, bijection) ->
      Crowbar.const ()
    | Proxy.AppLOpt (tu, bijection) ->
      Crowbar.const (match bijection.Bijection.to_ () with
          | Some v -> v
          | None -> Crowbar.bad_test ())
    | Proxy.AppROpt _ ->
      Crowbar.const ()
    | Proxy.Nop ->
      Crowbar.const ()
    | Proxy.Commit ->
      Crowbar.const ()
    | Proxy.Any ->
      Crowbar.(map [ int8 ]) Char.unsafe_chr
    | Proxy.Pure (_, value) ->
      Crowbar.const value
    | Proxy.Take n ->
      Crowbar.bytes_fixed n
    | Proxy.Const s ->
      Crowbar.const s
    | Proxy.Buffer ->
      Crowbar.bytes
    | Proxy.BBuffer ->
      Crowbar.(map [ bytes ]) bigstring_of_string
    | Proxy.Peek (ta, tb) ->
      (match Random.bool () with
       | true -> Crowbar.map [ value_gen ta ] (fun v -> Encore.Either.L v)
       | false -> Crowbar.map [ value_gen tb ] (fun v -> Encore.Either.R v))
    | Proxy.Fail _ -> Crowbar.bad_test ()
    | Proxy.Fix _ -> Crowbar.bad_test ()
    | Proxy.While0 p -> predicate_gen p
    | Proxy.While1 p ->
      Crowbar.map [ predicate_gen p ] (fun bs -> if String.length bs = 0 then Crowbar.bad_test () else bs)
    | Proxy.BWhile0 p ->
      Crowbar.map [ predicate_gen p ] bigstring_of_string
    | Proxy.BWhile1 p ->
      Crowbar.map [ predicate_gen p ] (bigstring_of_string <.> (fun bs -> if String.length bs = 0 then Crowbar.bad_test () else bs))

type value = G : 'a Meta.t * 'a -> value

let rec gen : value Crowbar.gen =
  let ( >>= ) = Crowbar.dynamic_bind in
  meta_gen >>= fun (Meta.V t) -> Crowbar.map [ value_gen t ] (fun v -> G (t, v))

let () =
  Crowbar.add_test ~name:"iso" [ gen ] @@ fun (G (t, v)) ->
  let encoder = Proxy.to_encoder t in
  let decoder = Proxy.to_decoder t in

  let raw = Encore.Encoder.to_string encoder v in

  match Angstrom.parse_string decoder raw with
  | Ok v' -> Crowbar.check_eq v v'
  | Error err -> Crowbar.fail err

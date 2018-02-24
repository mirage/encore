type ('f, 't, 'is) kind =
  | Exn : ('a, 'a, exn) kind
  | Opt : ('a, 'a option, opt) kind
  | Res : ('a, ('a, error) result, res) kind
and exn = E
and opt = O
and res = R
and error = [ `Msg of string ]

let exn = Exn
let opt = Opt
let res = Res

type ('k, 'a, 'b) t =
  { to_ : 'a -> 'rb
  ; of_ : 'b -> 'ra
  ; kd  : 'kd
  ; tag : string * string }
  constraint 'k =
    < reta : ('a, 'ra, 'kd) kind
  ; retb : ('b, 'rb, 'kd) kind >

type ('a, 'b) texn =
  (< reta : ('a, 'a, exn) kind
   ; retb : ('b, 'b, exn) kind >, 'a, 'b) t

type ('a, 'b) topt =
  (< reta : ('a, 'a option, opt) kind
   ; retb : ('b, 'b option, opt) kind >, 'a, 'b) t

type ('a, 'b) tres =
  (< reta : ('a, ('a, error) result, res) kind
   ; retb : ('b, ('b, error) result, res) kind >, 'a, 'b) t

let make
  : type a b ra rb kd.
    (a, ra, kd) kind -> (b, rb, kd) kind
    -> tag:string * string
    -> fwd:(a -> rb)
    -> bwd:(b -> ra)
    -> (< reta : (a, ra, kd) kind
        ; retb : (b, rb, kd) kind >, a, b) t
  = fun k k' ~tag ~fwd ~bwd ->
          { to_ = fwd
          ; of_ = bwd
          ; kd  = (match k, k' with
  | Exn, Exn -> E
  | Opt, Opt -> O
  | Res, Res -> R)
          ; tag }

let make_exn ~tag ~fwd ~bwd = make exn exn ~tag ~fwd ~bwd
let make_opt ~tag ~fwd ~bwd = make opt opt ~tag ~fwd ~bwd
let make_res ~tag ~fwd ~bwd = make res res ~tag ~fwd ~bwd

let flip
  : (< reta : ('a, 'ra, 'kd) kind
     ; retb : ('b, 'rb, 'kd) kind >, 'a, 'b) t ->
    (< reta : ('b, 'rb, 'kd) kind
     ; retb : ('a, 'ra, 'kd) kind >, 'b, 'a) t
  = fun x -> { to_ = x.of_
             ; of_ = x.to_
             ; kd  = x.kd
             ; tag = (snd x.tag, fst x.tag) }

let compose
  : (< reta : ('a, 'ra, 'kd) kind
     ; retb : ('b, 'rb, 'kd) kind >, 'a, 'b) t ->
    (< reta : ('b, 'rb, 'kd) kind
     ; retb : ('c, 'rc, 'kd) kind >, 'b, 'c) t ->
    (< reta : ('a, 'ra, 'kd) kind
     ; retb : ('c, 'rc, 'kd) kind >, 'a, 'c) t
  = fun { to_; of_; kd; tag; } s ->
  { to_ = (fun x -> s.to_ @@ to_ x)
  ; of_ = (fun x -> of_ @@ s.of_ x)
  ; kd  = kd
  ; tag = (fst tag, snd s.tag) }

let ( % ) = compose

let commute =
  { to_ = (fun (a, b) -> (b, a))
  ; of_ = (fun (b, a) -> (a, b))
  ; kd  = E
  ; tag = ("a * b", "b * a") }

external identity : 'a -> 'a = "%identity"

let identity =
  { to_ = identity
  ; of_ = identity
  ; kd  = E
  ; tag = ("a", "a") }

let obj3 =
  { to_ = (fun ((x, y), z) -> (x, y, z))
  ; of_ = (fun (x, y, z) -> ((x, y), z))
  ; kd  = E
  ; tag = ("", "") }

let obj4 =
  { to_ = (fun (((w, x), y), z) -> (w, x, y, z))
  ; of_ = (fun (w, x, y, z) -> (((w, x), y), z))
  ; kd  = E
  ; tag = ("", "") }

let obj5 =
  { to_ = (fun ((((v, w), x), y), z) -> (v, w, x, y, z))
  ; of_ = (fun (v, w, x, y, z) -> ((((v, w), x), y), z))
  ; kd  = E
  ; tag = ("", "") }

let obj6 =
  { to_ = (fun (((((u, v), w), x), y), z) -> (u, v, w, x, y, z))
  ; of_ = (fun (u, v, w, x, y, z) -> (((((u, v), w), x), y), z))
  ; kd  = E
  ; tag = ("", "") }

exception Bijection of string * string

let fail to_ of_ = raise (Bijection (to_, of_))

let element ~tag ~compare x =
  { to_ = (fun () -> x)
  ; of_ =
      (fun x' ->
         if compare x x'
         then ()
         else fail tag "unit")
  ; kd  = E
  ; tag = (tag, "unit") }

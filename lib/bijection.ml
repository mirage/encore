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

let fwd t = t.to_
let bwd t = t.of_

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

let product : (< reta : ('a, 'ra, 'kd) kind
               ; retb : ('b, 'rb, 'kd) kind >, 'a, 'b) t ->
              (< reta : ('c, 'rc, 'kd) kind
               ; retb : ('d, 'rd, 'kd) kind >, 'c, 'd) t ->
              (< reta : ('a * 'c, 'ra * 'rc, 'kd) kind
               ; retb : ('b * 'd, 'rb * 'rd, 'kd) kind >, 'a * 'c, 'b * 'd) t
  = fun u v ->
  { to_ = (fun (a, b) -> (u.to_ a, v.to_ b))
  ; of_ = (fun (a, b) -> (u.of_ a, v.of_ b))
  ; kd = u.kd (* = v.kd *)
  ; tag = (Fmt.strf "%s * %s" (fst u.tag) (fst v.tag), Fmt.strf "%s * %s" (snd u.tag) (snd v.tag)) }

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
  ; tag = ("((a, b), c)", "(a, b, c)") }

let obj4 =
  { to_ = (fun (((w, x), y), z) -> (w, x, y, z))
  ; of_ = (fun (w, x, y, z) -> (((w, x), y), z))
  ; kd  = E
  ; tag = ("(((a, b), c), d)", "(a, b, c, d)") }

let obj5 =
  { to_ = (fun ((((v, w), x), y), z) -> (v, w, x, y, z))
  ; of_ = (fun (v, w, x, y, z) -> ((((v, w), x), y), z))
  ; kd  = E
  ; tag = ("((((a, b), c), d), e)", "(a, b, c, d, d)") }

let obj6 =
  { to_ = (fun (((((u, v), w), x), y), z) -> (u, v, w, x, y, z))
  ; of_ = (fun (u, v, w, x, y, z) -> (((((u, v), w), x), y), z))
  ; kd  = E
  ; tag = ("(((((a, b), c), d), e), f)", "(a, b, c, d, e, f)") }

module Exn =
struct
  exception Bijection of string * string

  let fail to_ of_ = raise (Bijection (to_, of_))

  let subset predicate =
    { to_ = (fun x -> if predicate x then x else fail "a with predicate" "x")
    ; of_ = (fun x -> if predicate x then x else fail "a with predicate" "x")
    ; kd = E
    ; tag = ("a with predicate", "a with predicate") }

  let element ~tag ~compare x =
    { to_ = (fun x' -> if compare x x' then () else fail tag "unit")
    ; of_ = (fun () -> x)
    ; kd  = E
    ; tag = (tag, "unit") }

  let singleton ~tag =
    let tag = tag, Fmt.strf "%s singleton" tag in
    { to_ = (fun x -> [ x ])
    ; of_ = (function
             | [ x ] -> x
             | [] | _ :: _ -> fail (snd tag) (fst tag))
    ; kd = E
    ; tag }

  let cons ~tag =
    let tag = tag, Fmt.strf "%s list" tag in
    { to_ = (fun (x, r) -> x :: r)
    ; of_ = (function
             | x :: r -> (x, r)
             | [] -> fail (snd tag) (fst tag))
    ; kd = E
    ; tag }

  let nil =
    { to_ = (fun () -> [])
    ; of_ = (function
             | [] -> ()
             | _ :: _ -> fail "nil" "unit")
    ; kd = E
    ; tag = ("unit", "nil") }

  let _fst ~tag v =
    let tag = (Fmt.strf "%s * %s" (fst v.tag) tag), (snd v.tag) in
    { to_ = (fun (x, _) -> v.to_ x)
    ; of_ = (fun x ->
      try (x, v.of_ ())
      with _ -> fail (snd tag) (fst tag))
    ; kd = E
    ; tag }

  let _snd ~tag v =
    let tag = (Fmt.strf "%s * %s" tag (fst v.tag)), (snd v.tag) in
    { to_ = (fun (_, x) -> x)
    ; of_ = (fun x ->
      try (v.of_ (), x)
      with _ -> fail (snd tag) (fst tag))
    ; kd = E
    ; tag }

  let some ~tag =
    let tag = tag, (Fmt.strf "%s option" tag) in
    { to_ = (fun x -> Some x)
    ; of_ = (function
             | Some x -> x
             | None -> fail (snd tag) (fst tag))
    ; kd = E
    ; tag }

  let none =
    let tag = "unit", "none" in
    { to_ = (fun () -> None)
    ; of_ = (function
             | Some _ -> fail (snd tag) (fst tag)
             | None -> Some ())
    ; kd = E
    ; tag }

  let string : (char list, string) texn =
    let string_of_list lst =
      let ln = List.length lst in
      let by = Bytes.create ln in

      let rec go idx = function
        | [] -> Bytes.unsafe_to_string by
        | x :: r -> Bytes.unsafe_set by idx x; go (idx + 1) r in
      go 0 lst in
    let list_of_string str =
      let ln = String.length str in
      let rec go idx acc =
        if idx >= 0 then acc else go (idx + 1) (String.unsafe_get str (ln - idx - 1) :: acc) in
      go 0 [] in
    { to_ = string_of_list
    ; of_ = list_of_string
    ; kd = E
    ; tag = ("char list", "string") }

  let safe_exn tag f x =
    try f x with _ -> fail (fst tag) (snd tag)

  let flip (a, b) = (b ,a)

  let int : (string, int) texn =
    let tag = ("string", "int") in
    make_exn ~tag
             ~fwd:(safe_exn tag int_of_string)
             ~bwd:(safe_exn (flip tag) string_of_int)

  let bool : (string, bool) texn =
    let tag = ("string", "bool") in
    make_exn ~tag
             ~fwd:(safe_exn tag bool_of_string)
             ~bwd:(safe_exn (flip tag) string_of_bool)

  let fst = _fst
  let snd = _snd
end

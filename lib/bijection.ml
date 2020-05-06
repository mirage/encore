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

type ('k, 'a, 'b) t = {
  to_ : 'a -> 'rb;
  of_ : 'b -> 'ra;
  kd : 'kd;
}
  constraint 'k = < reta : ('a, 'ra, 'kd) kind ; retb : ('b, 'rb, 'kd) kind >

type ('a, 'b) texn =
  (< reta : ('a, 'a, exn) kind ; retb : ('b, 'b, exn) kind >, 'a, 'b) t

type ('a, 'b) topt =
  ( < reta : ('a, 'a option, opt) kind ; retb : ('b, 'b option, opt) kind >,
    'a,
    'b )
  t

type ('a, 'b) tres =
  ( < reta : ('a, ('a, error) result, res) kind
    ; retb : ('b, ('b, error) result, res) kind >,
    'a,
    'b )
  t

let make :
    type a b ra rb kd.
    (a, ra, kd) kind ->
    (b, rb, kd) kind ->
    fwd:(a -> rb) ->
    bwd:(b -> ra) ->
    (< reta : (a, ra, kd) kind ; retb : (b, rb, kd) kind >, a, b) t =
 fun k k' ~fwd ~bwd ->
  {
    to_ = fwd;
    of_ = bwd;
    kd = (match (k, k') with Exn, Exn -> E | Opt, Opt -> O | Res, Res -> R);
  }

let fwd t = t.to_

let bwd t = t.of_

let make_exn ~fwd ~bwd = make exn exn ~fwd ~bwd

let make_opt ~fwd ~bwd = make opt opt ~fwd ~bwd

let make_res ~fwd ~bwd = make res res ~fwd ~bwd

let flip :
    (< reta : ('a, 'ra, 'kd) kind ; retb : ('b, 'rb, 'kd) kind >, 'a, 'b) t ->
    (< reta : ('b, 'rb, 'kd) kind ; retb : ('a, 'ra, 'kd) kind >, 'b, 'a) t =
 fun x -> { to_ = x.of_; of_ = x.to_; kd = x.kd }

let product :
    (< reta : ('a, 'ra, 'kd) kind ; retb : ('b, 'rb, 'kd) kind >, 'a, 'b) t ->
    (< reta : ('c, 'rc, 'kd) kind ; retb : ('d, 'rd, 'kd) kind >, 'c, 'd) t ->
    ( < reta : ('a * 'c, 'ra * 'rc, 'kd) kind
      ; retb : ('b * 'd, 'rb * 'rd, 'kd) kind >,
      'a * 'c,
      'b * 'd )
    t =
 fun u v ->
  {
    to_ = (fun (a, b) -> (u.to_ a, v.to_ b));
    of_ = (fun (a, b) -> (u.of_ a, v.of_ b));
    kd = u.kd (* = v.kd *);
  }

let obj3 =
  {
    to_ = (fun ((x, y), z) -> (x, y, z));
    of_ = (fun (x, y, z) -> ((x, y), z));
    kd = E;
  }

let obj4 =
  {
    to_ = (fun (((w, x), y), z) -> (w, x, y, z));
    of_ = (fun (w, x, y, z) -> (((w, x), y), z));
    kd = E;
  }

let obj5 =
  {
    to_ = (fun ((((v, w), x), y), z) -> (v, w, x, y, z));
    of_ = (fun (v, w, x, y, z) -> ((((v, w), x), y), z));
    kd = E;
  }

let obj6 =
  {
    to_ = (fun (((((u, v), w), x), y), z) -> (u, v, w, x, y, z));
    of_ = (fun (u, v, w, x, y, z) -> (((((u, v), w), x), y), z));
    kd = E;
  }

external identity : 'a -> 'a = "%identity"

module Exn = struct
  exception Bijection

  let fail () = raise Bijection

  let compose : ('a, 'b) texn -> ('b, 'c) texn -> ('a, 'c) texn =
   fun { to_; of_; _ } s ->
    { to_ = (fun x -> s.to_ @@ to_ x); of_ = (fun x -> of_ @@ s.of_ x); kd = E }

  let ( % ) = compose

  let commute =
    { to_ = (fun (a, b) -> (b, a)); of_ = (fun (b, a) -> (a, b)); kd = E }

  let identity = { to_ = identity; of_ = identity; kd = E }

  let subset predicate =
    {
      to_ = (fun x -> if predicate x then x else fail ());
      of_ = (fun x -> if predicate x then x else fail ());
      kd = E;
    }

  let element ~compare x =
    {
      to_ = (fun x' -> if compare x x' then () else fail ());
      of_ = (fun () -> x);
      kd = E;
    }

  let singleton =
    {
      to_ = (fun x -> [ x ]);
      of_ = (function [ x ] -> x | [] | _ :: _ -> fail ());
      kd = E;
    }

  let cons =
    {
      to_ = (fun (x, r) -> x :: r);
      of_ = (function x :: r -> (x, r) | [] -> fail ());
      kd = E;
    }

  let nil =
    { to_ = (fun () -> []); of_ = (function [] -> () | _ :: _ -> ()); kd = E }

  let _fst v =
    {
      to_ = (fun (x, _) -> x);
      of_ = (fun x -> try (x, v.of_ ()) with _ -> fail ());
      kd = E;
    }

  let _snd v =
    {
      to_ = (fun (_, x) -> x);
      of_ = (fun x -> try (v.of_ (), x) with _ -> fail ());
      kd = E;
    }

  let some =
    {
      to_ = (fun x -> Some x);
      of_ = (function Some x -> x | None -> fail ());
      kd = E;
    }

  let none =
    {
      to_ = (fun () -> None);
      of_ = (function Some _ -> fail () | None -> ());
      kd = E;
    }

  let string : (char list, string) texn =
    let string_of_list lst =
      let ln = List.length lst in
      let by = Bytes.create ln in
      let rec go idx = function
        | [] -> Bytes.unsafe_to_string by
        | x :: r ->
            Bytes.unsafe_set by idx x ;
            go (idx + 1) r in
      go 0 lst in
    let list_of_string str =
      let ln = String.length str in
      let rec go idx acc =
        if idx >= 0
        then acc
        else go (idx + 1) (String.unsafe_get str (ln - idx - 1) :: acc) in
      go 0 [] in
    { to_ = string_of_list; of_ = list_of_string; kd = E }

  let safe_exn f x = try f x with _ -> fail ()

  let int : (string, int) texn =
    make_exn ~fwd:(safe_exn int_of_string) ~bwd:(safe_exn string_of_int)

  let bool : (string, bool) texn =
    make_exn ~fwd:(safe_exn bool_of_string) ~bwd:(safe_exn string_of_bool)

  let of_option t =
    {
      to_ = (fun x -> match t.to_ x with Some x -> x | None -> fail ());
      of_ = (fun x -> match t.of_ x with Some x -> x | None -> fail ());
      kd = E;
    }

  let fst = _fst

  let snd = _snd
end

type ('f, 't, 'is) kind =
  | Exn : ('a, 'a, exn) kind
  | Opt : ('a, 'a option, opt) kind
  | Res : ('a, ('a, error) result, res) kind
and exn = E
and opt = O
and res = R
and error = [ `Msg of string ]

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

val make: ('a, 'ra, 'kd) kind -> ('b, 'rb, 'kd) kind
          -> tag:string * string
          -> fwd:('a -> 'rb)
          -> bwd:('b -> 'ra)
          -> (< reta : ('a, 'ra, 'kd) kind
              ; retb : ('b, 'rb, 'kd) kind >, 'a, 'b) t

val fwd: (< reta : ('a, 'ra, 'kd) kind; retb : ('b, 'rb, 'kd) kind; >, 'a, 'b) t -> 'a -> 'rb
val bwd: (< reta : ('a, 'ra, 'kd) kind; retb : ('b, 'rb, 'kd) kind; >, 'a, 'b) t -> 'b -> 'ra

val make_exn: tag:string * string -> fwd:('a -> 'b) -> bwd:('b -> 'a) -> ('a, 'b) texn
val make_opt: tag:string * string -> fwd:('a -> 'b option) -> bwd:('b -> 'a option) -> ('a, 'b) topt
val make_res: tag:string * string -> fwd:('a -> ('b, error) result) -> bwd:('b -> ('a, error) result) -> ('a, 'b) tres

val flip:
  (< reta : ('a, 'ra, 'kd) kind
   ; retb : ('b, 'rb, 'kd) kind >, 'a, 'b) t ->
  (< reta : ('b, 'rb, 'kd) kind
   ; retb : ('a, 'ra, 'kd) kind >, 'b, 'a) t

val product:
  (< reta : ('a, 'ra, 'kd) kind
   ; retb : ('b, 'rb, 'kd) kind >, 'a, 'b) t ->
  (< reta : ('c, 'rc, 'kd) kind
   ; retb : ('d, 'rd, 'kd) kind >, 'c, 'd) t ->
  (< reta : ('a * 'c, 'ra * 'rc, 'kd) kind
   ; retb : ('b * 'd, 'rb * 'rd, 'kd) kind >, 'a * 'c, 'b * 'd) t

val obj3: (('a * 'b) * 'c, 'a * 'b * 'c) texn
val obj4: ((('a * 'b) * 'c) * 'd, 'a * 'b * 'c * 'd) texn
val obj5: (((('a * 'b) * 'c) * 'd) * 'e, 'a * 'b * 'c * 'd * 'e) texn
val obj6: ((((('a * 'b) * 'c) * 'd) * 'e) * 'f, 'a * 'b * 'c * 'd * 'e * 'f) texn

module Exn:
sig
  exception Bijection of string * string

  val fail: string -> string -> 'a

  val compose: ('a, 'b) texn -> ('b, 'c) texn -> ('a, 'c) texn
  val ( % ): ('a, 'b) texn -> ('b, 'c) texn -> ('a, 'c) texn

  val commute: ('a * 'b, 'b * 'a) texn
  val identity: ('a, 'a) texn

  val subset: ('a -> bool) -> ('a, 'a) texn
  val element: tag:string -> compare:('a -> 'a -> bool) -> 'a -> ('a, unit) texn
  val singleton: tag:string -> ('a, 'a list) texn
  val cons: tag:string -> ('a * 'a list, 'a list) texn
  val nil: (unit, unit list) texn
  val some: tag:string -> ('a, 'a option) texn
  val none: (unit, unit option) texn
  val string: (char list, string) texn
  val safe_exn: (string * string) -> ('a -> 'b) -> 'a -> 'b
  val int: (string, int) texn
  val bool: (string, bool) texn
  val fst: tag:string -> ('b, unit) texn -> ('a * 'b, 'a) texn
  val snd: tag:string -> ('b, unit) texn -> ('b * 'a, 'a) texn
end

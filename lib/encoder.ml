type vec =
  { off : int option
  ; len : int option }

type 'a state = 'a Lole.state
type encoder = Lole.encoder
type bigstring = Lole.bigstring
type iovecs = Lole.IOVec.t list

type -'a t =
  { run : 'r. (encoder -> 'r state) -> encoder -> 'a -> 'r state }
type -'a s =
  { sub : 'r. (encoder -> 'r state) -> encoder -> ?off:int -> ?len:int -> 'a -> 'r state }

let peek : 'a t -> 'b t -> ('a, 'b) Either.t t =
  fun a b ->
  { run = fun k e -> function
                  | L x -> a.run k e x
                  | R y -> b.run k e y }

let char    : char  t = { run = fun k e v -> Lole.write_char v k e }
let int8    : int   t = { run = fun k e v -> Lole.write_uint8 v k e }
let beint16 : int   t = { run = fun k e v -> Lole.BE.write_uint16 v k e }
let beint32 : int32 t = { run = fun k e v -> Lole.BE.write_uint32 v k e }
let beint64 : int64 t = { run = fun k e v -> Lole.BE.write_uint64 v k e }
let leint16 : int   t = { run = fun k e v -> Lole.LE.write_uint16 v k e }
let leint32 : int32 t = { run = fun k e v -> Lole.LE.write_uint32 v k e }
let leint64 : int64 t = { run = fun k e v -> Lole.LE.write_uint64 v k e }

let bool : bool t = { run = fun k e -> function
    | true  -> char.run k e '1'
    | false -> char.run k e '0' }

let substring    : string    s = { sub = fun k e ?off ?len v -> Lole.write_string ?off ?len v k e }
let subbytes     : bytes     s = { sub = fun k e ?off ?len v -> Lole.write_bytes ?off ?len v k e }
let subbigstring : bigstring s = { sub = fun k e ?off ?len v -> Lole.write_bigstring ?off ?len v k e }

let blitter length blit : _ s = { sub = fun k e ?off ?len v ->
    Lole.write k ~blit ~length ?off ?len v e }

let whole (a : 'v s) : 'v t = { run = fun k e v -> a.sub ?off:None ?len:None k e v }
let sub (a : 'v s) : (vec * 'v) t = { run = fun k e ({ off; len; }, v) -> a.sub ?off ?len k e v }

let string    : string    t = whole substring
let bytes     : bytes     t = whole subbytes
let bigstring : bigstring t = whole subbigstring

let list ?sep a : 'a list t =
  let sep k e = match sep with
    | None -> k e
    | Some a -> a.run k e () in
  let rec run k e : _ list -> _ state = function
    | [] -> k e
    | [ x ] ->
      a.run k e x
    | x :: r ->
      a.run (sep (fun e -> run k e r)) e x in
  { run }

let nop = { run = fun k e _ -> k e }

let option f : 'a option t =
  { run = fun k e -> function
        | Some v -> f.run k e v
        | None -> k e }

let compose f g = { run = fun k e (x, y) -> f.run (fun e -> g.run k e y) e x }

let using a f =
  { run = fun k e v -> a.run k e (f v) }

let const a v =
  { run = fun k e () -> a.run k e v }

let ( >>| ) : 'a t -> ('b -> 'a) -> 'b t = using
let ( <$> ) : ('b -> 'a) -> 'a t -> 'b t = fun f p -> p >>| f
let ( <*> ) : 'a t -> 'b t -> ('a * 'b) t = compose
let ( <!> ) = const

exception Fail of string

let pure ~compare v =
  { run = fun k e v' -> if compare v v' = 0 then k e else raise (Fail "fail at the pure operator") }

let fail s =
  { run = fun _k _e _v -> raise (Fail s) }

let ( <|> ) pu pv =
  { run = fun k e v ->
        try pu.run k e v
        with Fail _ | Bijection.Exn.Bijection (_, _) -> pv.run k e v }

let prefix p r =
  { run = fun k e v -> p.run (fun e -> r.run k e v) e () }

let suffix s r =
  { run = fun k e v -> r.run (fun e -> s.run k e ()) e v }

let satisfy f =
  using char (fun x -> match f x with true -> x | false -> raise (Fail (Fmt.strf "satisfy: %c" x)))
let while0 f =
  let satisfy s =
    let l = String.length s in
    for i = 0 to l - 1
    do if not (f (String.unsafe_get s i)) then raise (Fail "while0") done;
    s in
  using string satisfy

let while1 f =
  using (while0 f) (fun s -> if String.length s = 0 then raise (Fail "while1"); s)

let bwhile0 f =
  let satisfy b =
    let l = Bigarray.Array1.dim b in
    for i = 0 to l - 1
    do if not (f (Bigarray.Array1.unsafe_get b i)) then raise (Fail "bwhile0") done;
    b in
  using bigstring satisfy

let bwhile1 f =
  using (bwhile0 f) (fun b -> if Bigarray.Array1.dim b = 0 then raise (Fail "bwhile1"); b)

let take n =
  { run = fun k e s -> if String.length s = n then string.run k e s else raise (Fail "take") }

let ( <* ) r s = suffix s r
let ( *> ) p r = prefix p r

let fix f =
  let rec p = lazy (f r)
  and r = { run = fun k e v -> Lazy.(force p).run k e v } in
  r

let run : 'a t -> (encoder -> 'r state) -> encoder -> 'a -> 'r state = fun p k e v -> p.run k e v

let l_brack = string <!> "["
let r_brack = string <!> "]"
let l_paren = string <!> "("
let r_paren = string <!> ")"
let l_brace = string <!> "{"
let r_brace = string <!> "}"

let from : ('a -> 'u t) -> 'a t = fun f -> { run = fun k e v -> (f v).run k e v }

let newline = int8 <!> 0x0a
let flush a = { run = fun k e v -> a.run (fun e -> Lole.flush k e) e v }

let bracks a = l_brack *> a <* r_brack
let parens a = l_paren *> a <* r_paren
let braces a = l_brace *> a <* r_brace
let between : unit t -> unit t -> 'a t -> 'a t = fun p s a -> p *> a <* s

let dquote = char <!> '"'
let comma = char <!> ','
let colon = char <!> ':'

let keval
  : 'v 'r. (encoder -> 'r state) -> (iovecs -> int) -> encoder -> 'v t -> 'v -> 'r
  = fun k w e t v ->
    let rec go = function
      | Lole.End v -> v
      | Lole.Continue { continue; encoder; } ->
        continue encoder |> go
      | Lole.Flush { continue; iovecs; } ->
        let len = w iovecs in
        continue len |> go in
    t.run k e v |> go

let eval w e t v = keval (fun _e -> Lole.End ()) w e (flush t) v

let to_string : type a. a t -> a -> string
  = fun t v ->
    let buf = Buffer.create 16 in
    let writer l =
      List.iter
        (function
          | { Lole.IOVec.buffer = Lole.Buffer.String s; off; len; } ->
            Buffer.add_substring buf s off len
          | { Lole.IOVec.buffer = Lole.Buffer.Bytes s; off; len; } ->
            Buffer.add_subbytes buf s off len
          | { Lole.IOVec.buffer = Lole.Buffer.Bigstring s; off; len; } ->
            for i = 0 to len - 1
            do Buffer.add_char buf (Bigarray.Array1.get s (off + i)) done)
        l;
      Lole.IOVec.lengthv l in
    eval writer (Lole.create 0x100) t v;
    Buffer.contents buf

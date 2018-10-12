type vec = {off: int option; len: int option}

type 'a state = 'a Lole.state

type encoder = Lole.encoder

type bigstring = Lole.bigstring

type iovecs = Lole.IOVec.t list

type -'a t = {run: 'r. (encoder -> 'r state) -> encoder -> 'a -> 'r state}

type -'a s =
  { sub:
      'r.    (encoder -> 'r state) -> encoder -> ?off:int -> ?len:int -> 'a
      -> 'r state }

let peek : 'a t -> 'b t -> ('a, 'b) Either.t t =
 fun a b ->
  {run= (fun k e -> function L x -> a.run k e x | R y -> b.run k e y)}

let char : char t = {run= (fun k e v -> Lole.write_char v k e)}

let int8 : int t = {run= (fun k e v -> Lole.write_uint8 v k e)}

let beint16 : int t = {run= (fun k e v -> Lole.BE.write_uint16 v k e)}

let beint32 : int32 t = {run= (fun k e v -> Lole.BE.write_uint32 v k e)}

let beint64 : int64 t = {run= (fun k e v -> Lole.BE.write_uint64 v k e)}

let leint16 : int t = {run= (fun k e v -> Lole.LE.write_uint16 v k e)}

let leint32 : int32 t = {run= (fun k e v -> Lole.LE.write_uint32 v k e)}

let leint64 : int64 t = {run= (fun k e v -> Lole.LE.write_uint64 v k e)}

let bool : bool t =
  { run=
      (fun k e -> function
        | true -> char.run k e '1'
        | false -> char.run k e '0' ) }

let substring : string s =
  {sub= (fun k e ?off ?len v -> Lole.write_string ?off ?len v k e)}

let subbytes : bytes s =
  {sub= (fun k e ?off ?len v -> Lole.write_bytes ?off ?len v k e)}

let subbigstring : bigstring s =
  {sub= (fun k e ?off ?len v -> Lole.write_bigstring ?off ?len v k e)}

let blitter length blit : _ s =
  {sub= (fun k e ?off ?len v -> Lole.write k ~blit ~length ?off ?len v e)}

let whole (a : 'v s) : 'v t =
  {run= (fun k e v -> a.sub ?off:None ?len:None k e v)}

let sub (a : 'v s) : (vec * 'v) t =
  {run= (fun k e ({off; len}, v) -> a.sub ?off ?len k e v)}

let string : string t = whole substring

let bytes : bytes t = whole subbytes

let bigstring : bigstring t = whole subbigstring

let list ?sep a : 'a list t =
  let sep k e = match sep with None -> k e | Some a -> a.run k e () in
  let rec run k e : _ list -> _ state = function
    | [] -> k e
    | [x] -> a.run k e x
    | x :: r -> a.run (sep (fun e -> run k e r)) e x
  in
  {run}

let nop = {run= (fun k e _ -> k e)}

let option f : 'a option t =
  {run= (fun k e -> function Some v -> f.run k e v | None -> k e)}

exception Fail of string

let pure ~compare v =
  { run=
      (fun k e v' ->
        if compare v v' = 0 then k e
        else raise (Fail "fail at the pure operator") ) }

let fail s = {run= (fun _k _e _v -> raise (Fail s))}

let const s =
  { run=
      (fun k e s' ->
        if String.equal s s' then Lole.write_string s' k e
        else raise (Fail (Fmt.strf "const: %s <> %s" s s')) ) }

let ( <|> ) pu pv =
  { run=
      (fun k e v ->
        try pu.run k e v with
        | Fail _ | Bijection.Exn.Bijection (_, _) -> pv.run k e v ) }

let ( <$> ) f p = {run= (fun k e v -> p.run k e (f v))}

let ( <*> ) a b = {run= (fun k e (x, y) -> a.run (fun e -> b.run k e y) e x)}

let prefix p r = {run= (fun k e v -> p.run (fun e -> r.run k e v) e ())}

let suffix s r = {run= (fun k e v -> r.run (fun e -> s.run k e ()) e v)}

exception Break

let for_all predicate s =
  let l = String.length s in
  try
    for i = 0 to l - 1 do
      if not (predicate (String.unsafe_get s i)) then raise Break
    done ;
    true
  with Break -> false

let while0 predicate =
  { run=
      (fun k e v ->
        if for_all predicate v then Lole.write_string v k e
        else raise (Fail "while0") ) }

let while1 predicate =
  { run=
      (fun k e v ->
        if String.length v > 0 && for_all predicate v then
          Lole.write_string v k e
        else raise (Fail "while1") ) }

let for_all predicate b =
  let l = Bigarray.Array1.dim b in
  try
    for i = 0 to l - 1 do
      if not (predicate b.{i}) then raise Break
    done ;
    true
  with Break -> false

let bigstring_while0 predicate =
  { run=
      (fun k e v ->
        if for_all predicate v then Lole.write_bigstring v k e
        else raise (Fail "bigstring_while0") ) }

let bigstring_while1 predicate =
  { run=
      (fun k e v ->
        if Bigarray.Array1.dim v > 0 && for_all predicate v then
          Lole.write_bigstring v k e
        else raise (Fail "bigstring_while1") ) }

let take n =
  (* XXX(dinosaure): Angstrom.take consumes input, so [take] should produce output. *)
  { run=
      (fun k e s ->
        if String.length s = n then string.run k e s else raise (Fail "take")
        ) }

let buffer = string

let bigstring_buffer = bigstring

let ( <* ) r s = suffix s r

let ( *> ) p r = prefix p r

let fix f =
  let rec p = lazy (f r)
  and r = {run= (fun k e v -> Lazy.(force p).run k e v)} in
  r

let commit = {run= (fun k e () -> Lole.flush k e)}

let keval :
      'v 'r.    (encoder -> 'r state) -> (iovecs -> int) -> encoder -> 'v t
      -> 'v -> 'r =
 fun k w e t v ->
  let rec go = function
    | Lole.End v -> v
    | Lole.Continue {continue; encoder} -> continue encoder |> go
    | Lole.Flush {continue; iovecs} ->
        let len = w iovecs in
        continue len |> go
  in
  t.run k e v |> go

let eval w e t v = keval (fun _e -> Lole.End ()) w e (t <* commit) v

let run t = t.run

module Make (S : sig
  type a

  val run : (encoder -> 'r state) -> encoder -> a -> 'r state
end) =
struct
  let x = {run= S.run}
end

let to_string : type a. a t -> a -> string =
 fun t v ->
  let buf = Buffer.create 16 in
  let writer l =
    List.iter
      (function
        | {Lole.IOVec.buffer= Lole.Buffer.String s; off; len} ->
            Buffer.add_substring buf s off len
        | {Lole.IOVec.buffer= Lole.Buffer.Bytes s; off; len} ->
            Buffer.add_subbytes buf s off len
        | {Lole.IOVec.buffer= Lole.Buffer.Bigstring s; off; len} ->
            for i = 0 to len - 1 do
              Buffer.add_char buf s.{off + i}
            done)
      l ;
    Lole.IOVec.lengthv l
  in
  eval writer (Lole.create 0x100) t v ;
  Buffer.contents buf

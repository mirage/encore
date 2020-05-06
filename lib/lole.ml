module type VALUE = sig
  type t

  val sentinel : t
  val weight : t -> int
  val merge : t -> t -> t option
  val pequal : t -> t -> bool
  val pp : t Fmt.t
end

module RBQ (V : VALUE) = struct
  module Queue = Ke.Fke.Weighted

  type t = {a: V.t array; c: int; m: int; q: (int, Bigarray_compat.int_elt) Queue.t}
  and value = V.t

  let make capacity =
    let q, capacity = Queue.create ~capacity Bigarray_compat.Int in
    { a= Array.make capacity V.sentinel
    ; c= 0
    ; m= capacity
    ; q }

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>a = %a;@ \
                         c = %d;@ \
                         m = %d;@ \
                         q = %a;@] }"
      Fmt.(Dump.array V.pp) t.a
      t.c t.m
      (Queue.dump Fmt.int) t.q

  let available t = Queue.available t.q

  let[@inline always] mask x t = x land (t.m - 1)

  let push t v =
    let i = mask t.c t in
    match Queue.push t.q i with
    | Some q ->
      t.a.(i) <- v ;
      Ok { t with c= succ t.c; q; }
    | None -> Error t

  let shift_exn t =
    let i, q = Queue.pop_exn t.q in
    (t.a.(i), { t with q })

  let cons t v =
    let i = mask t.c t in
    match Queue.cons t.q i with
    | Some q ->
      t.a.(i) <- v ;
      Ok { t with c= succ t.c; q; }
    | None -> Error t

  exception Full

  let cons_exn t v =
    match cons t v with
    | Ok t -> t
    | Error _ -> raise Full

  let weight t =
    Queue.fold (fun a i -> a + V.weight t.a.(i)) 0 t.q

  let to_list t =
    let res = ref [] in
    Queue.rev_iter (fun i -> res := t.a.(i) :: !res) t.q ;
    !res
end

type bigstring = (char, Bigarray_compat.int8_unsigned_elt, Bigarray_compat.c_layout) Bigarray_compat.Array1.t

type 'a blitter = 'a -> int -> bigstring -> int -> int -> unit

let pp_chr =
  Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

let pp_scalar : type buffer.
    get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
 fun ~get ~length ppf b ->
  let l = length b in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16) ;
    let j = ref 0 in
    while !j < 16 do
      if (i * 16) + !j < l then
        Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  " ;
      if !j mod 2 <> 0 then Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "  " ;
    j := 0 ;
    while !j < 16 do
      if (i * 16) + !j < l then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "@\n"
  done

let pp_string = pp_scalar ~get:String.get ~length:String.length
let pp_bytes = pp_scalar ~get:Bytes.get ~length:Bytes.length
let pp_bigstring = pp_scalar ~get:Bigarray_compat.Array1.get ~length:Bigarray_compat.Array1.dim

module RBA = Ke.Fke.Weighted

module Buffer = struct
  type t = Bigstring of bigstring | String of string | Bytes of Bytes.t

  let weight = function
    | Bigstring raw -> Bigarray_compat.Array1.dim raw
    | String raw -> String.length raw
    | Bytes raw -> Bytes.length raw

  let pp ppf = function
    | Bigstring b -> Fmt.pf ppf "(Bigstring %a)" (Fmt.hvbox pp_bigstring) b
    | Bytes b -> Fmt.pf ppf "(Bytes %a)" (Fmt.hvbox pp_bytes) b
    | String b -> Fmt.pf ppf "(String %a)" (Fmt.hvbox pp_string) b

  let sub buffer off len =
    match buffer with
    | Bigstring b -> Bigstring (Bigarray_compat.Array1.sub b off len)
    | String b -> String (String.sub b off len)
    | Bytes b -> Bytes (Bytes.sub b off len)
end

module IOVec = struct
  type t = {buffer: Buffer.t; off: int; len: int}

  let weight {len; _} = len

  let sentinel =
    let deadbeef = "\222\173\190\239" in
    {buffer= Buffer.String deadbeef; off= 0; len= String.length deadbeef}

  let make buffer off len = {buffer; off; len}
  let length {len; _} = len
  let lengthv = List.fold_left (fun acc x -> length x + acc) 0

  let shift {buffer; off; len} n =
    assert (n <= len) ;
    {buffer; off= off + n; len= len - n}

  let split {buffer; off; len} n =
    assert (n <= len) ;
    ( {buffer= Buffer.sub buffer off n; off= 0; len= n}
    , {buffer= Buffer.sub buffer (off + n) (len - n); off= 0; len= len - n} )

  let pequal a b =
    match (a, b) with
    | {buffer= Buffer.Bytes a; _}, {buffer= Buffer.Bytes b; _} -> a == b
    | {buffer= Buffer.Bigstring a; _}, {buffer= Buffer.Bigstring b; _} ->
      ( match Overlap.array1 a b with
        | Some (len, 0, 0) -> Bigarray_compat.Array1.dim a = len && Bigarray_compat.Array1.dim b = len
        | _ -> false )
    | _, _ -> false

  let merge a b =
    match (a, b) with
    | {buffer= Buffer.Bytes a'; _}, {buffer= Buffer.Bytes b'; _} ->
        assert (a' == b') ;
        if a.off + a.len = b.off then
          Some {buffer= Buffer.Bytes a'; off= a.off; len= a.len + b.len}
        else None
    | {buffer= Buffer.Bigstring a'; _}, {buffer= Buffer.Bigstring b'; _} ->
        assert (a' == b') ;
        if a.off + a.len = b.off then
          Some {buffer= Buffer.Bigstring a'; off= a.off; len= a.len + b.len}
        else None
    | _, _ -> None

  let pp ppf {buffer; off; len} =
    Fmt.pf ppf "{ @[<hov>buffer = %a;@ off = %d;@ len = %d:@] }"
      (Fmt.hvbox Buffer.pp) buffer off len
end

module RBS = RBQ (IOVec)

type encoder =
  { sched: RBS.t
  ; write: (char, Bigarray_compat.int8_unsigned_elt) RBA.t
  ; flush: (int * (int -> encoder -> unit)) Ke.Fke.t
  ; written: int
  ; received: int }

let pp ppf {sched; _} =
  Fmt.pf ppf "{ @[<hov>sched = %a;@ write = #queue;@] }" (Fmt.hvbox RBS.pp)
    sched

type 'v state =
  | Flush of {continue: int -> 'v state; iovecs: IOVec.t list}
  | Continue of {continue: encoder -> 'v state; encoder: encoder}
  | End of 'v

let create len =
  let write, _ = RBA.create ~capacity:len Bigarray_compat.Char in
  { sched= RBS.make (len * 2)
  ; write
  ; flush= Ke.Fke.empty
  ; written= 0
  ; received= 0 }

let from len bigarray =
  let write = RBA.from bigarray in
  { sched= RBS.make (len * 2)
  ; write
  ; flush= Ke.Fke.empty
  ; written= 0
  ; received= 0 }

let check iovec {write; _} =
  match iovec with
  | {IOVec.buffer= Buffer.Bigstring x; _} ->
    let buf = RBA.unsafe_bigarray write in
    ( match Overlap.array1 x buf with
    | Some (_, _, _) -> true
    | None -> false )
  | _ -> false

let shift_buffers n t =
  let rec aux rest acc t =
    match RBS.shift_exn t.sched with
    | iovec, shifted ->
        let len = IOVec.length iovec in
        if rest > len then
          aux (rest - len) (iovec :: acc)
            { t with
              sched= shifted
            ; write=
                (if check iovec t then RBA.N.shift_exn t.write len else t.write)
            }
        else if rest > 0 then
          let last, rest = IOVec.split iovec rest in
          ( List.rev (last :: acc)
          , { t with
              sched= RBS.cons_exn shifted rest
            ; write=
                ( if check iovec t then
                  RBA.N.shift_exn t.write (IOVec.length last)
                else t.write ) } )
        else (List.rev acc, t)
    | exception RBS.Queue.Empty -> (List.rev acc, t)
  in
  aux n [] t

let shift_flushes n t =
  let rec aux t =
    try
      let (threshold, f), flush = Ke.Fke.pop_exn t.flush in
      if
        compare (t.written + n - min_int) (threshold - min_int) >= 0
        (* unsigned int *)
      then
        let () = f n {t with flush} in
        aux {t with flush}
      else t
    with Ke.Fke.Empty -> t
  in
  aux t

let shift n t =
  let lst, t = shift_buffers n t in
  ( lst
  , shift_flushes (IOVec.lengthv lst) t
    |> fun t -> {t with written= t.written + n} )

let has t = RBS.weight t.sched

let drain drain t =
  let rec go rest t =
    match RBS.shift_exn t.sched with
    | iovec, shifted ->
        let len = IOVec.length iovec in
        if rest > len then
          go (rest - len)
            { t with
              sched= shifted
            ; write=
                (if check iovec t then RBA.N.shift_exn t.write len else t.write)
            }
        else
          { t with
            sched= RBS.cons_exn shifted (IOVec.shift iovec rest)
          ; write=
              (if check iovec t then RBA.N.shift_exn t.write rest else t.write)
          }
    | exception RBS.Queue.Empty -> t
  in
  go drain t |> fun t -> {t with written= t.written + drain}

let flush k t =
  let t = shift_flushes (has t) t in
  let continue n =
    let t = drain n t in
    k {t with written= t.written + n}
  in
  Flush {continue; iovecs= RBS.to_list t.sched}

let continue continue encoder = Continue {continue; encoder}

let rec schedule k ~length ~buffer ?(off = 0) ?len v t =
  let len = match len with Some len -> len | None -> length v - off in
  match RBS.push t.sched (IOVec.make (buffer v) off len) with
  | Ok sched -> continue k {t with sched; received= t.received + len}
  | Error _ ->
      let max = RBS.available t.sched in
      let k t =
        (schedule [@tailcall]) k ~length ~buffer ~off:(off + max)
          ~len:(len - max) v t
      in
      schedule (flush k) ~length ~buffer ~off ~len:max v t

let schedule_string =
  let length = String.length in
  let buffer x = Buffer.String x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_bytes =
  let length = Bytes.length in
  let buffer x = Buffer.Bytes x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_bigstring =
  let length = Bigarray_compat.Array1.dim in
  let buffer x = Buffer.Bigstring x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

external identity : 'a -> 'a = "%identity"

let schedulev k l t =
  let rec aux t = function
    | [] -> continue k t
    | (length, off, len, buffer) :: r ->
        schedule
          (fun t -> (aux [@tailcall]) t r)
          ~length ?off ?len ~buffer:identity buffer t
  in
  aux t l

let schedulev_bigstring k l t =
  let rec aux t = function
    | [] -> continue k t
    | buffer :: r ->
        schedule_bigstring (fun t -> (aux [@tailcall]) t r) t buffer
  in
  aux t l

let rec write k ~blit ~length ?(off = 0) ?len buffer t =
  let len = match len with Some len -> len | None -> length buffer - off in
  let available = RBA.available t.write in
  (* XXX(dinosaure): we can factorize the first and the second branch. *)
  if available >= len then
    let areas, write = RBA.N.push_exn t.write ~blit ~length ~off ~len buffer in
    schedulev_bigstring k areas {t with write}
  else if available > 0 then
    let k t =
      (write [@tailcall]) k ~blit ~length ~off:(off + available)
        ~len:(len - available) buffer t
    in
    let areas, write =
      RBA.N.push_exn t.write ~blit ~length ~off ~len:available buffer
    in
    schedulev_bigstring (flush k) areas {t with write}
  else
    let k t = (write [@tailcall]) k ~blit ~length ~off ~len buffer t in
    flush k t

let writev k l t =
  let rec aux t = function
    | [] -> continue k t
    | (blit, length, off, len, buffer) :: r ->
        write (fun t -> (aux [@tailcall]) t r) ~blit ~length ?off ?len buffer t
  in
  aux t l

let bigarray_blit_from_string src src_off dst dst_off len =
  for i = 0 to len - 1 do
    Bigarray_compat.Array1.unsafe_set dst (dst_off + i)
      (String.unsafe_get src (src_off + i))
  done

let bigarray_blit_from_bytes src src_off dst dst_off len =
  for i = 0 to len - 1 do
    Bigarray_compat.Array1.unsafe_set dst (dst_off + i)
      (Bytes.unsafe_get src (src_off + i))
  done

let bigarray_blit src src_off dst dst_off len =
  Bigarray_compat.Array1.(blit (sub src src_off len) (sub dst dst_off len))

let write_string =
  let length = String.length in
  let blit = bigarray_blit_from_string in
  fun ?(off = 0) ?len a k t -> write k ~blit ~length ~off ?len a t

let write_bytes =
  let length = Bytes.length in
  let blit = bigarray_blit_from_bytes in
  fun ?(off = 0) ?len a k t -> write k ~blit ~length ~off ?len a t

let write_bigstring =
  let length = Bigarray_compat.Array1.dim in
  let blit = bigarray_blit in
  fun ?(off = 0) ?len a k t -> write k ~blit ~length ~off ?len a t

let write_char =
  let length _ = assert false in
  let blit src src_off dst dst_off len =
    assert (src_off = 0) ;
    assert (len = 1) ;
    Bigstringaf.set dst dst_off src
  in
  fun a k t -> write k ~length ~blit ~off:0 ~len:1 a t

let write_uint8 =
  let length _ = assert false in
  let blit src src_off dst dst_off len =
    assert (src_off = 0) ;
    assert (len = 1) ;
    Bigstringaf.set dst dst_off (Char.chr (src land 0xff))
  in
  fun a k t -> write k ~length ~blit ~off:0 ~len:1 a t

module type S = sig
  val write_uint16 : int -> (encoder -> 'r state) -> encoder -> 'r state
  val write_uint32 : int32 -> (encoder -> 'r state) -> encoder -> 'r state
  val write_uint64 : int64 -> (encoder -> 'r state) -> encoder -> 'r state
end

module BE = struct
  let _length _ = assert false

  let write_uint16 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0) ;
      assert (len = 2) ;
      Bigstringaf.set_int16_be dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:2 a t

  let write_uint32 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0) ;
      assert (len = 4) ;
      Bigstringaf.set_int32_be dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:4 a t

  let write_uint64 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0) ;
      assert (len = 8) ;
      Bigstringaf.set_int64_be dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:8 a t
end

module LE = struct
  let _length _ = assert false

  let write_uint16 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0) ;
      assert (len = 2) ;
      Bigstringaf.set_int16_le dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:2 a t

  let write_uint32 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0) ;
      assert (len = 4) ;
      Bigstringaf.set_int32_le dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:4 a t

  let write_uint64 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0) ;
      assert (len = 8) ;
      Bigstringaf.set_int64_le dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:8 a t
end

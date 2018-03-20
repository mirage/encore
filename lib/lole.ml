module Option =
struct
  let map_default default f = function
    | Some v -> f v
    | None -> default
end

module type VALUE =
sig
  type t

  val sentinel : t
  val weight   : t -> int
  val merge    : t -> t -> t option
  val pequal   : t -> t -> bool
  val pp       : t Fmt.t
end

module RBQ (V : VALUE) =
struct
  module Queue = FQueue

  type t =
    { c : int
    ; w : int
    ; q : V.t Queue.t }
  and value = V.t

  let make capacity =
    { c = capacity
    ; w = 0
    ; q = Queue.empty }

  let pp ppf { c; w; q; } =
    Fmt.pf ppf "{ @[<hov>c = %d;@ \
                         w = %d;@ \
                         q = %a;@] }"
      c w
      (Fmt.hvbox (Queue.pp V.pp)) q

  let available t =
    t.c - t.w

  let push t v =
    let w = t.w + V.weight v in

    if w > t.c
    then Error t
    else Ok { t with w; q = Queue.push t.q v; }

  let shift_exn t = match Queue.shift t.q with
    | v, q -> v, { t with w = t.w - V.weight v; q; }

  let cons t v =
    let w = t.w + V.weight v in

    if w > t.c
    then Error t
    else Ok { t with w; q = Queue.cons t.q v; }

  let cons_exn t v =
    if t.w + V.weight v > t.c
    then invalid_arg "cons_exn: no enough space";

    { t with w = t.w + V.weight v
           ; q = Queue.cons t.q v }

  let weight t =
    Queue.fold (fun acc x -> acc + V.weight x) 0 t.q

  let to_list t =
    Queue.to_list t.q
end

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type 'a blitter = 'a -> int -> bigstring -> int -> int -> unit

let pp_chr =
  Fmt.using
    (function '\032' .. '\126' as x -> x
            | _ -> '.')
    Fmt.char

let pp_scalar : type buffer. get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t
  = fun ~get ~length ppf b ->
  let l = length b in

  for i = 0 to l / 16
  do Fmt.pf ppf "%08x: " (i * 16);
    let j = ref 0 in

    while !j < 16
    do if (i * 16) + !j < l
      then Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  ";

      if !j mod 2 <> 0 then Fmt.pf ppf " ";

      incr j;
    done;

    Fmt.pf ppf "  ";
    j := 0;

    while !j < 16
    do if (i * 16) + !j < l
      then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " ";

      incr j;
    done;

    Fmt.pf ppf "@\n"
  done

let pp_string    = pp_scalar ~get:String.get ~length:String.length
let pp_bytes     = pp_scalar ~get:Bytes.get ~length:Bytes.length
let pp_bigstring = pp_scalar ~get:Bigarray.Array1.get ~length:Bigarray.Array1.dim

module RBA =
struct
  type t =
    { r : int
    ; w : int
    ; c : int
    ; b : bigstring }

  let is_power_of_two x =
    (x <> 0) && ((x land (lnot x + 1)) = x)

  let create capacity =
    if not (is_power_of_two capacity)
    then invalid_arg "RBA.create: the capacity need to be a power of two";

    { r = 0
    ; w = 0
    ; c = capacity
    ; b = Bigarray.Array1.create Bigarray.char Bigarray.c_layout capacity }

  let mask t v = v land (t.c - 1)     [@@inline]
  let empty t = t.r = t.w             [@@inline]
  let size t = t.w - t.r              [@@inline]
  let available t = t.c - (t.w - t.r) [@@inline]
  let full t = size t = t.c           [@@inline]

  let pp ppf ({ r; w; c; b; } as t) =
    let pp_rb r w m ppf b =
      if (mask t r) < (mask t w)
      then
        Fmt.pf ppf "%a"
          (Fmt.hvbox pp_bigstring)
          (Bigarray.Array1.sub b (mask t r) (w - r))
      else if empty t
      then Fmt.pf ppf "<empty>"
      else Fmt.pf ppf "@[<hov>%a and %a@]"
          (Fmt.hvbox pp_bigstring) (Bigarray.Array1.sub b (mask t r) (m - (mask t r)))
          (Fmt.hvbox pp_bigstring) (Bigarray.Array1.sub b 0 (mask t w))
    in

    Fmt.pf ppf
      "{ @[<hov>r = %d;@ \
                w = %d;@ \
                c = %d;@ \
                b = %a;@] }"
      r w c (Fmt.hvbox (pp_rb r w c)) b

  let push t v =
    let[@inline] mask t v = v land (t.c - 1) in
    Bigarray.Array1.set t.b ((mask[@inlined]) t t.w) v;
    { t with w = t.w + 1 }

  let shift t _ =
    let[@inline] mask t v = v land (t.c - 1) in
    let r = Bigarray.Array1.get t.b ((mask[@inlined]) t t.r) in
    r, { t with r = t.r + 1 }

  module N =
  struct
    let push t ~blit ~length ?(off = 0) ?len v =
      let len = match len with
        | None -> length v
        | Some len -> len in
      let[@inline] mask t v = v land (t.c - 1) in

      let pre = t.c - (mask[@inlined]) t t.w in
      let extra = len - pre in

      let areas =
        if extra > 0
        then begin
          blit v off t.b ((mask[@inlined]) t t.w) pre;
          blit v (off + pre) t.b 0 extra;
          [ Bigarray.Array1.sub t.b ((mask[@inlined]) t t.w) pre
          ; Bigarray.Array1.sub t.b 0 extra ]
        end else begin
          blit v off t.b ((mask[@inlined]) t t.w) len;
          [ Bigarray.Array1.sub t.b ((mask[@inlined]) t t.w) len ]
        end
      in

      areas, { t with w = t.w + len }

    let keep t ~blit ~length ?(off = 0) ?len v =
      let len = match len with
        | None -> length v
        | Some len -> len
      in

      assert (size t >= len);

      let pre = t.c - mask t t.r in
      let extra = len - pre in

      if extra > 0
      then begin
        blit t.b (mask t t.r) v off pre;
        blit t.b 0 v (off + pre) extra;
      end else
        blit t.b (mask t t.r) v off len

    let shift t len =
      { t with r = t.r + len }
  end
end

module Buffer =
struct
  type t =
    | Bigstring of bigstring
    | String of string
    | Bytes of Bytes.t

  let weight = function
    | Bigstring raw -> Bigarray.Array1.dim raw
    | String raw -> String.length raw
    | Bytes raw -> Bytes.length raw

  let ppw_bigstring ppf b =
    let len = Bigarray.Array1.dim b in
    for i = 0 to len - 1
    do Fmt.char ppf (Bigarray.Array1.unsafe_get b i) done

  let ppw ppf = function
    | Bigstring b -> ppw_bigstring ppf b
    | String b -> Fmt.string ppf b
    | Bytes b -> Fmt.string ppf (Bytes.unsafe_to_string b)

  let pp ppf = function
    | Bigstring b ->
      Fmt.pf ppf "(Bigstring %a)"
        (Fmt.hvbox pp_bigstring) b
    | Bytes b ->
      Fmt.pf ppf "(Bytes %a)"
        (Fmt.hvbox pp_bytes) b
    | String b ->
      Fmt.pf ppf "(String %a)"
        (Fmt.hvbox pp_string) b

  let sub buffer off len = match buffer with
    | Bigstring b -> Bigstring (Bigarray.Array1.sub b off len)
    | String b -> String (String.sub b off len)
    | Bytes b -> Bytes (Bytes.sub b off len)
end

module IOVec =
struct
  type t =
    { buffer : Buffer.t
    ; off    : int
    ; len    : int }

  let weight { len; _ } = len

  let sentinel =
    let deadbeef = "\222\173\190\239" in
    { buffer = Buffer.String deadbeef; off = 0; len = String.length deadbeef }

  let make buffer off len =
    { buffer
    ; off
    ; len }

  let length { len; _ } = len

  let lengthv = List.fold_left (fun acc x -> length x + acc) 0

  let shift { buffer; off; len; } n =
    assert (n <= len);

    { buffer
    ; off = off + n
    ; len = len - n }

  let split { buffer; off; len; } n =
    assert (n <= len);

    { buffer = Buffer.sub buffer off n
    ; off = 0
    ; len = n },
    { buffer = Buffer.sub buffer (off + n) (len - n)
    ; off = 0
    ; len = len - n }

  let pequal a b = match a, b with
    | { buffer = Buffer.Bytes a; _ }, { buffer = Buffer.Bytes b; _ } -> a == b
    | { buffer = Buffer.Bigstring a; _ }, { buffer = Buffer.Bigstring b; _ } -> a == b
    | _, _ -> false

  let merge a b = match a, b with
    | { buffer = Buffer.Bytes a'; _ }, { buffer = Buffer.Bytes b'; _ } ->
      assert (a' == b');
      if a.off + a.len = b.off
      then Some { buffer = Buffer.Bytes a'; off = a.off; len = a.len + b.len }
      else None
    | { buffer = Buffer.Bigstring a'; _ }, { buffer = Buffer.Bigstring b'; _ } ->
      assert (a' == b');
      if a.off + a.len = b.off
      then Some { buffer = Buffer.Bigstring a'; off = a.off; len = a.len + b.len }
      else None
    | _, _ -> None

  let ppw ppf = function
    | { buffer = Buffer.Bigstring b
      ; off
      ; len } -> Buffer.ppw_bigstring ppf (Bigarray.Array1.sub b off len)
    | { buffer = Buffer.String b
      ; off
      ; len } -> Fmt.string ppf (String.sub b off len)
    | { buffer = Buffer.Bytes b
      ; off
      ; len } -> Fmt.string ppf (Bytes.sub_string b off len)

  let pp ppf { buffer; off; len; } =
    Fmt.pf ppf "{ @[<hov>buffer = %a;@ \
                         off = %d;@ \
                         len = %d:@] }"
      (Fmt.hvbox Buffer.pp) buffer off len
end

module RBS = RBQ(IOVec)

type encoder =
  { sched    : RBS.t
  ; write    : RBA.t
  ; flush    : (int * (int -> encoder -> unit)) FQueue.t
  ; written  : int
  ; received : int }

let pp ppf { sched; write; _ } =
  Fmt.pf ppf "{ @[<hov>sched = %a;@ \
                       write = %a;@] }"
    (Fmt.hvbox RBS.pp) sched (Fmt.hvbox RBA.pp) write

type 'v state =
  | Flush    of { continue : int -> 'v state
                ; iovecs   : IOVec.t list }
  | Continue of { continue : encoder -> 'v state
                ; encoder  : encoder }
  | End      of 'v

let create len =
  { sched = RBS.make (len * 2)
  ; write = RBA.create len
  ; flush = FQueue.empty
  ; written = 0
  ; received = 0 }

let check iovec t = match iovec with
  | { IOVec.buffer = Buffer.Bigstring bs
    ; _ } ->
    let be = Bigarray.Array1.sub t.write.RBA.b (Bigarray.Array1.dim t.write.RBA.b) 0 in

    let sub_ptr : int = Obj.magic @@ Obj.field (Obj.repr bs) 1 in
    let raw_ptr : int = Obj.magic @@ Obj.field (Obj.repr t.write.RBA.b) 1 in
    let end_ptr : int = Obj.magic @@ Obj.field (Obj.repr be) 1 in

    sub_ptr >= raw_ptr && sub_ptr <= end_ptr
  | _ -> false

let shift_buffers n t =
  let rec aux rest acc t = match RBS.shift_exn t.sched with
    | iovec, shifted ->
      let len = IOVec.length iovec in
      if rest > len
      then aux (rest - len) (iovec :: acc)
          { t with sched = shifted
                 ; write = if check iovec t
                     then RBA.N.shift t.write len
                     else t.write }
      else if rest > 0
      then let last, rest = IOVec.split iovec rest in
        List.rev (last :: acc),
        { t with sched = RBS.cons_exn shifted rest
               ; write = if check iovec t
                   then RBA.N.shift t.write (IOVec.length last)
                   else t.write }
      else List.rev acc, t
    | exception RBS.Queue.Empty ->
      List.rev acc, t
  in

  aux n [] t

let shift_flushes n t =
  let rec aux t =
    try
      let (threshold, f), flush = FQueue.shift t.flush in

      if compare (t.written + n - min_int) (threshold - min_int) >= 0 (* unsigned int *)
      then let () = f n { t with flush } in aux { t with flush }
      else t
    with FQueue.Empty -> t
  in

  aux t

let shift n t =
  let lst, t = shift_buffers n t in
  lst, (shift_flushes (IOVec.lengthv lst) t |> fun t -> { t with written = t.written + n })

let has t =
  RBS.weight t.sched

let drain drain t =
  let rec go rest t = match RBS.shift_exn t.sched with
    | iovec, shifted ->
      let len = IOVec.length iovec in
      if rest > len
      then go (rest - len) { t with sched = shifted
                                  ; write = if check iovec t
                                      then RBA.N.shift t.write len
                                      else t.write }
      else { t with sched = RBS.cons_exn shifted (IOVec.shift iovec rest)
                  ; write = if check iovec t
                      then RBA.N.shift t.write rest
                      else t.write }
    | exception RBS.Queue.Empty -> t
  in

  go drain t |> fun t -> { t with written = t.written + drain }

let flush k t =
  let t = shift_flushes (has t) t in

  let continue n =
    let t = drain n t in
    k { t with written = t.written + n }
  in

  Flush { continue
        ; iovecs = RBS.to_list t.sched }

let continue continue encoder =
  Continue { continue
           ; encoder }

let rec schedule k ~length ~buffer ?(off = 0) ?len v t =
  let len = match len with
    | Some len -> len
    | None -> length v - off in

  match RBS.push t.sched (IOVec.make (buffer v) off len) with
  | Ok sched ->
    continue k { t with sched
                      ; received = t.received + len }
  | Error _ ->
    let max = RBS.available t.sched in

    let k t = (schedule[@tailcall]) k ~length ~buffer ~off:(off + max) ~len:(len - max) v t in

    schedule (flush k)
      ~length ~buffer ~off ~len:max v t

let schedule_string =
  let length = String.length in
  let buffer x = Buffer.String x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_bytes =
  let length = Bytes.length in
  let buffer x = Buffer.Bytes x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_bigstring =
  let length = Bigarray.Array1.dim in
  let buffer x = Buffer.Bigstring x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_flush f t =
  { t with flush = FQueue.push t.flush (t.received, f) }

external identity : 'a -> 'a = "%identity"

let schedulev k l t =
  let rec aux t = function
    | [] -> continue k t
    | (length, off, len, buffer) :: r ->
      schedule (fun t -> (aux[@tailcall]) t r) ~length ?off ?len ~buffer:identity buffer t
  in aux t l

let schedulev_bigstring k l t =
  let rec aux t = function
    | [] -> continue k t
    | buffer :: r -> schedule_bigstring (fun t -> (aux[@tailcall]) t r) t buffer
  in aux t l

let rec write k ~blit ~length ?(off = 0) ?len buffer t =
  let len = match len with
    | Some len -> len
    | None -> length buffer - off in
  let available = RBA.available t.write in

  (* XXX(dinosaure): we can factorize the first and the second branch. *)
  if available >= len
  then begin
    let areas, write = RBA.N.push t.write ~blit ~length ~off ~len buffer in
    schedulev_bigstring k areas { t with write }
  end else if available > 0
  then begin
    let k t = (write[@tailcall]) k ~blit ~length ~off:(off + available) ~len:(len - available) buffer t in
    let areas, write = RBA.N.push t.write ~blit ~length ~off ~len:available buffer in
    schedulev_bigstring (flush k) areas { t with write }
  end else
    let k t = (write[@tailcall]) k ~blit ~length ~off ~len buffer t in
    flush k t

let writev k l t =
  let rec aux t = function
    | [] -> continue k t
    | (blit, length, off, len, buffer) :: r ->
      write (fun t -> (aux[@tailcall]) t r) ~blit ~length ?off ?len buffer t
  in aux t l

let bigarray_blit_from_string src src_off dst dst_off len =
  for i = 0 to len - 1
  do Bigarray.Array1.unsafe_set
      dst (dst_off + i)
      (String.unsafe_get src (src_off + i)) done

let bigarray_blit_from_bytes src src_off dst dst_off len =
  for i = 0 to len - 1
  do Bigarray.Array1.unsafe_set
      dst (dst_off + i)
      (Bytes.unsafe_get src (src_off + i)) done

let bigarray_blit src src_off dst dst_off len =
  Bigarray.Array1.(blit (sub src src_off len) (sub dst dst_off len))

let bigarray_blit_to_bytes src src_off dst dst_off len =
  for i = 0 to len - 1
  do Bytes.set dst (dst_off + i)
      (Bigarray.Array1.unsafe_get src (src_off + i)) done

let write_string =
  let length = String.length in
  let blit = bigarray_blit_from_string in
  fun ?(off = 0) ?len a k t ->
    write k ~blit ~length ~off ?len a t

let write_bytes =
  let length = Bytes.length in
  let blit = bigarray_blit_from_bytes in
  fun ?(off = 0) ?len a k t ->
    write k ~blit ~length ~off ?len a t

let write_bigstring =
  let length = Bigarray.Array1.dim in
  let blit = bigarray_blit in
  fun ?(off = 0) ?len a k t ->
    write k ~blit ~length ~off ?len a t

let write_char =
  let length _ = assert false in
  let blit src src_off dst dst_off len =
    assert (src_off = 0);
    assert (len = 1);
    EndianBigstring.BigEndian_unsafe.set_char dst dst_off src
  in
  fun a k t -> write k ~length ~blit ~off:0 ~len:1 a t

let write_uint8 =
  let length _ = assert false in
  let blit src src_off dst dst_off len =
    assert (src_off = 0);
    assert (len = 1);
    EndianBigstring.BigEndian_unsafe.set_int8 dst dst_off src
  in
  fun a k t -> write k ~length ~blit ~off:0 ~len:1 a t

module type EndianBigstringSig = EndianBigstring.EndianBigstringSig
module type EndianBytesSig = EndianBytes.EndianBytesSig

module type SE =
  sig
    val write_uint16: int -> (encoder -> 'r state) -> encoder -> 'r state
    val write_uint32: int32 -> (encoder -> 'r state) -> encoder -> 'r state
    val write_uint64: int64 -> (encoder-> 'r state) -> encoder -> 'r state
  end

module MakeE (EBigstring : EndianBigstringSig): SE =
struct
  let _length _ = assert false

  let write_uint16 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0);
      assert (len = 2);
      EBigstring.set_int16 dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:2 a t

  let write_uint32 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0);
      assert (len = 4);
      EBigstring.set_int32 dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:4 a t

  let write_uint64 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0);
      assert (len = 8);
      EBigstring.set_int64 dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:8 a t
end

module LE = MakeE(EndianBigstring.LittleEndian_unsafe)
module BE = MakeE(EndianBigstring.BigEndian_unsafe)

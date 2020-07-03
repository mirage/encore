type state = Partial of partial | Done | Fail

and partial = {
  buffer : Bigstringaf.t;
  off : int;
  len : int;
  continue : committed:int -> state;
}

type encoder = {
  buffer : Bigstringaf.t;
  mutable pos : int;
  stack : int Stack.t;
}

let io_buffer_size = 65536

let flush k0 encoder =
  if Stack.length encoder.stack > 0
  then
    if encoder.pos = Bigstringaf.length encoder.buffer then Fail else k0 encoder
  else if encoder.pos > 0
  then
    let rec k1 n =
      if n < encoder.pos
      then
        Partial
          {
            buffer = encoder.buffer;
            off = n;
            len = encoder.pos - n;
            continue = (fun ~committed:m -> k1 (n + m));
          }
      else (
        encoder.pos <- 0 ;
        k0 encoder) in
    k1 0
  else k0 encoder

let rec write_char chr k encoder =
  if encoder.pos + 1 < Bigstringaf.length encoder.buffer
  then (
    Bigstringaf.set encoder.buffer encoder.pos chr ;
    encoder.pos <- encoder.pos + 1 ;
    k encoder)
  else flush (write_char chr k) encoder

let rec write_string str k encoder =
  let len = String.length str in
  if encoder.pos + len < Bigstringaf.length encoder.buffer
  then (
    Bigstringaf.blit_from_string str ~src_off:0 encoder.buffer
      ~dst_off:encoder.pos ~len ;
    encoder.pos <- encoder.pos + len ;
    k encoder)
  else flush (write_string str k) encoder

type -'a t = { run : (encoder -> state) -> encoder -> 'a -> state } [@@unbox]

let finish encoder = flush (fun _ -> Done) encoder

let error encoder = flush (fun _ -> Fail) encoder

let emit value d =
  let encoder =
    {
      buffer = Bigstringaf.create io_buffer_size;
      stack = Stack.create ();
      pos = 0;
    } in
  d.run finish encoder value

let emit_string ?(chunk = 0x1000) value d =
  let buf = Buffer.create chunk in
  let rec go = function
    | Partial { buffer; off; len; continue } ->
        let str = Bigstringaf.substring buffer ~off ~len in
        Buffer.add_string buf str ;
        go (continue ~committed:len)
    | Done -> Buffer.contents buf
    | Fail -> invalid_arg "emit_string" in
  go (emit value d)

let char = { run = (fun k e v -> write_char v k e) }

let string str =
  { run = (fun k e v -> if v <> str then error e else write_string v k e) }

let pure ~compare v =
  { run = (fun k e v' -> if compare v v' then k e else error e) }

let choose p q =
  {
    run =
      (fun k e v ->
        Stack.push e.pos e.stack ;
        let go = function
          | Partial _ -> assert false
          | Done -> k e
          | Fail ->
              e.pos <- Stack.pop e.stack ;
              q.run k e v in
        go
          (p.run
             (fun e ->
               let _ = Stack.pop e.stack in
               Done)
             e v));
  }

let string_for_all f x =
  let rec go a i =
    if i < String.length x then go (f x.[i] && a) (succ i) else a in
  go true 0

let string_for_all_while n f x =
  let rec go a i =
    if i < String.length x && i < n
    then go (f x.[i] && a) (succ i)
    else a && i >= n in
  go true 0

let put_while1 p =
  {
    run =
      (fun k e v ->
        if String.length v > 0 && string_for_all p v
        then write_string v k e
        else error e);
  }

let put_while0 p =
  {
    run =
      (fun k e v -> if string_for_all p v then write_string v k e else error e);
  }

let put p n =
  {
    run =
      (fun k e v ->
        if string_for_all p v && String.length v = n
        then write_string v k e
        else error e);
  }

let at_least_put p n =
  {
    run =
      (fun k e v ->
        if string_for_all_while n p v then write_string v k e else error e);
  }

let range ~a ~b p =
  {
    run =
      (fun k e v ->
        let max = String.length v in
        let x = string_for_all_while a p v in
        let y = ref a in
        while !y < max && p v.[!y] do
          incr y
        done ;
        if x && !y <= b then write_string v k e else error e);
  }

let product a b =
  {
    run =
      (fun k e (u, v) ->
        let k e = b.run k e v in
        a.run k e u);
  }

let fail _err = { run = (fun _ e _ -> error e) }

let fix f =
  let rec d = lazy (f r)
  and r = { run = (fun k e v -> Lazy.(force d).run k e v) } in
  r

let ( *> ) p r = { run = (fun k e v -> p.run (fun e -> r.run k e v) e ()) }

let ( <* ) p r = { run = (fun k e v -> p.run (fun e -> r.run k e ()) e v) }

let map x f =
  { run = (fun k e v -> try x.run k e (f v) with Bij.Bijection -> error e) }

let commit = { run = (fun k e () -> flush k e) }

let peek a b =
  {
    run =
      (fun k e -> function
        | Either.L x -> a.run k e x
        | Either.R y -> b.run k e y);
  }

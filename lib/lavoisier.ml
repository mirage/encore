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
  then k0 encoder
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
  if encoder.pos + 1 <= Bigstringaf.length encoder.buffer
  then (
    Bigstringaf.set encoder.buffer encoder.pos chr ;
    encoder.pos <- encoder.pos + 1 ;
    k encoder)
  else if Stack.length encoder.stack = 0
  then flush (write_char chr k) encoder
  else Fail

let rec write_string str k encoder =
  let len = String.length str in
  if encoder.pos + len <= Bigstringaf.length encoder.buffer
  then (
    Bigstringaf.blit_from_string str ~src_off:0 encoder.buffer
      ~dst_off:encoder.pos ~len ;
    encoder.pos <- encoder.pos + len ;
    k encoder)
  else if Stack.length encoder.stack = 0
  then flush (write_string str k) encoder
  else Fail

type -'a t = { run : (encoder -> state) -> encoder -> 'a -> state; pure : bool }

let finish encoder = flush (fun _ -> Done) encoder

let error encoder = flush (fun _ -> Fail) encoder

let emit ?(chunk= io_buffer_size) value d =
  let encoder =
    {
      buffer = Bigstringaf.create chunk;
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
  let chunk = if chunk > io_buffer_size then chunk else io_buffer_size in
  go (emit ~chunk value d)

let char = { run = (fun k e v -> write_char v k e); pure = false }

let string str =
  {
    run = (fun k e v -> if v <> str then error e else write_string v k e);
    pure = String.length str = 0;
  }

let pure ~compare v =
  { run = (fun k e v' -> if compare v v' then k e else error e); pure = true }

(* XXX(dinosaure): [choose p q = choose q p], even if we execute [q] before [p],
   the result output must be the same ([choose] is associative). By this fact,
   we can take the opportunity to optimize the encoding.

   With:
   {[
     let rep1 p = fix @@ fun m -> Bij.cons <$> (p <*> (m <|> nil))
   ]}

   [m <|> nil] can be replaced by [nil <|> m]. Then, [nil] is a pure element
   which does not write anything into the output. So we can compute it safely
   and if it fails, we simply run [q].

   This optimization helps us to avoid a large stack until we reach [nil]. *)

let choose q p =
  if p.pure
  then
    {
      run =
        (fun k e v ->
          let rec go = function
            | Partial { buffer; off; len; continue } ->
                Partial
                  {
                    buffer;
                    off;
                    len;
                    continue = (fun ~committed -> go (continue ~committed));
                  }
            | Done -> k e
            | Fail -> q.run k e v in
          go (p.run (fun _ -> Done) e v));
      pure = q.pure;
    }
  else
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
      pure = false;
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
    pure = false;
  }

let put_while0 p =
  {
    run =
      (fun k e v -> if string_for_all p v then write_string v k e else error e);
    pure = false;
  }

let put p n =
  {
    run =
      (fun k e v ->
        if string_for_all p v && String.length v = n
        then write_string v k e
        else error e);
    pure = false;
  }

let at_least_put p n =
  {
    run =
      (fun k e v ->
        if string_for_all_while n p v then write_string v k e else error e);
    pure = false;
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
    pure = false;
  }

let product a b =
  {
    run =
      (fun k e (u, v) ->
        let k e = b.run k e v in
        a.run k e u);
    pure = a.pure && b.pure;
  }

let fail _err = { run = (fun _ e _ -> error e); pure = true }

let fix f =
  let rec d = lazy (f r)
  and r = { run = (fun k e v -> Lazy.(force d).run k e v); pure = false } in
  r

let ( *> ) p r =
  {
    run = (fun k e v -> p.run (fun e -> r.run k e v) e ());
    pure = p.pure && r.pure;
  }

let ( <* ) p r =
  {
    run = (fun k e v -> p.run (fun e -> r.run k e ()) e v);
    pure = p.pure && r.pure;
  }

let map x f =
  {
    run = (fun k e v -> try x.run k e (f v) with Bij.Bijection -> error e);
    pure = x.pure;
  }

let commit = { run = (fun k e () -> flush k e); pure = true }

let peek a b =
  {
    run =
      (fun k e -> function
        | Either.L x -> a.run k e x
        | Either.R y -> b.run k e y);
    pure = a.pure && b.pure;
  }

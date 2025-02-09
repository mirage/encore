type state = Partial of partial | Done | Fail

and partial = {
  buffer : string;
  off : int;
  len : int;
  continue : committed:int -> state;
}

type encoder = { dequeue : Deke.t; stack : int Stack.t }

let flush k0 encoder =
  if Stack.length encoder.stack > 0 || Deke.is_empty encoder.dequeue
  then
    k0 encoder
    (* TODO(dinosaure): or
     * [   not (Deke.is_empty encoder.dequeue
     *  && Stack.length encoder.stack = 0] *)
  else if not (Deke.is_empty encoder.dequeue)
  then
    let str = ref (Deke.pop encoder.dequeue) in
    let rec k1 n =
      if n < String.length !str
      then
        Partial
          {
            buffer = !str;
            off = n;
            len = String.length !str - n;
            continue = (fun ~committed:m -> (k1 [@tailcall]) (n + m));
          }
      else
        match Deke.pop encoder.dequeue with
        | str' ->
            str := str' ;
            (k1 [@tailcaill]) 0
        | exception Deke.Empty -> k0 encoder in
    k1 0
  else k0 encoder

(* XXX(dinosaure): pre-allocate small strings. *)
let ( <.> ) f g x = f (g x)

let _chr = Array.init 256 (String.make 1 <.> Char.unsafe_chr)

let write_char chr k encoder =
  Deke.push encoder.dequeue _chr.(Char.code chr) ;
  k encoder

let write_string str k encoder =
  Deke.push encoder.dequeue str ;
  k encoder

type -'a t = { run : (encoder -> state) -> encoder -> 'a -> state; pure : bool }

let finish encoder = flush (fun _ -> Done) encoder

let error encoder = flush (fun _ -> Fail) encoder

let emit value d =
  let encoder = { dequeue = Deke.create (); stack = Stack.create () } in
  d.run finish encoder value

let emit_string ?(chunk = 0x1000) value d =
  let buf = Buffer.create chunk in
  let rec go = function
    | Partial { buffer = str; off; len; continue } ->
        Buffer.add_substring buf str off len ;
        (go [@tailcall]) (continue ~committed:len)
    | Done -> Buffer.contents buf
    | Fail -> invalid_arg "emit_string" in
  go (emit value d)

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

let rec rem dequeue weight =
  if Deke.weight dequeue > weight
  then
    match Deke.rem dequeue with
    | _str ->
        let remaining = Deke.weight dequeue - weight in
        if remaining > 0
        then (rem [@tailcaill]) dequeue weight
        else if remaining < 0
        then assert false
    | exception Deke.Empty -> ()

let _done _ = Done

let commit p ~committed:_ = p

(* XXX(dinosaure): this part wants to optimize and avoid many calls (and grow the stack)
 * for large objects. Indeed, [map] should protect us against a [Bij.Bijection] exception.
 * However, such surrounding protection generate a cascade of [map] when for each element
 * of a larger object, we apply a function.
 *
 * We know that [Bij.Bijection] is really necesary to catch only for the choose operation.
 * Indeed, this is the only place where we need to /undo/ what we did & and relaunch the
 * generation. [unroll] assumes that [p] is pure, it should not write anything - but a flush
 * can exist inside. Such assertion allows us to catch [Bij.Bijection] at the pattern level.
 *
 * With such design, we allow OCaml to optimize the whole process in the CPS style without
 * any free-variables - so [k], [e] or [v] can be wrong but we don't care because, if [p]
 * is pure, these values should not be usable/updated finally.
 *
 * The worst case is when [p] and [q] are not pure. In that case, [Partial] can not be done
 * but the buffer size output should be enough larger to keep the result of [p] or [q]. Again,
 * in that case, we take the opportunity to catch the exception at the pattern-level (and
 * avoid the cascade of surrounded piece of code with [try ... with ...]) to continue on
 * the CPS style.
 *
 * /!\ this code asserts several things, and it try to fit into Irmin and be able to
 * serialize **large** tree objects. If you want to update it, be really aware! *)
let rec unroll ~committed p q k e v =
  match (p ~committed).run _done e v with
  | Done -> k e
  | Fail -> q.run k e v
  | Partial { buffer; off; len; continue } ->
      let p ~committed =
        { run = (fun _k _e _v -> continue ~committed); pure = false } in
      Partial { buffer; off; len; continue = unroll p q k e v }
  | exception Bij.Bijection -> q.run k e v

let _pop_and_done e =
  let _ = Stack.pop e.stack in
  Done

let unroll_and_undo p q k e v =
  Stack.push (Deke.weight e.dequeue) e.stack ;
  (* XXX(dinosaure): save. *)
  match p.run _pop_and_done e v with
  | Partial _ -> assert false
  | Done -> k e
  | Fail ->
      rem e.dequeue (Stack.pop e.stack) ;
      (* XXX(dinosaure): undo. *)
      q.run k e v
  | exception Bij.Bijection ->
      rem e.dequeue (Stack.pop e.stack) ;
      (* XXX(dinosaure): undo. *)
      q.run k e v

let choose p q =
  if p.pure
  then { run = unroll ~committed:0 (commit p) q; pure = q.pure }
  else if q.pure
  then { run = unroll ~committed:0 (commit q) p; pure = p.pure }
  else { run = unroll_and_undo p q; pure = false }

let string_for_all f x =
  let rec go a i =
    if i < String.length x then (go [@tailcall]) (f x.[i] && a) (succ i) else a
  in
  go true 0

let string_for_all_while n f x =
  let rec go a i =
    if i < String.length x && i < n
    then (go [@tailcall]) (f x.[i] && a) (succ i)
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

let map x f = { run = (fun k e v -> x.run k e (f v)); pure = x.pure }

let commit = { run = (fun k e () -> flush k e); pure = true }

let peek a b =
  {
    run =
      (fun k e -> function
        | Either.L x -> a.run k e x
        | Either.R y -> b.run k e y);
    pure = a.pure && b.pure;
  }

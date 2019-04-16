module type S = sig
  type 'a t

  val ( <$> ) : ('a, 'b) Bijection.texn -> 'a t -> 'b t
  val ( <*> ) : 'a t -> 'b t -> ('a * 'b) t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( *> ) : unit t -> 'a t -> 'a t
  val ( <* ) : 'a t -> unit t -> 'a t
  val ( $> ) : unit t -> (unit, 'a) Bijection.texn -> 'a t
  val ( <$ ) : 'a t -> (unit, 'a) Bijection.texn -> unit t
  val fix : ('a t -> 'a t) -> 'a t
  val nop : unit t
  val any : char t
  val fail : string -> 'a t
  val pure : compare:('a -> 'a -> int) -> 'a -> 'a t
  val take : int -> string t
  val peek : 'a t -> 'b t -> ('a, 'b) Either.t t
  val const : string -> string t
  val commit : unit t
  val while0 : (char -> bool) -> string t
  val while1 : (char -> bool) -> string t
  val bigstring_while0 : (char -> bool) -> Encoder.bigstring t
  val bigstring_while1 : (char -> bool) -> Encoder.bigstring t
  val buffer : string t
  val bigstring_buffer : Encoder.bigstring t

  module Option : sig
    val ( <$> ) : ('a, 'b) Bijection.topt -> 'a t -> 'b t
    val ( $> ) : unit t -> (unit, 'a) Bijection.topt -> 'a t
    val ( <$ ) : 'a t -> (unit, 'a) Bijection.topt -> unit t
  end
end

module type T = sig
  include S

  val sequence : 'a t list -> 'a list t
  val choice : 'a t list -> 'a t
  val option : 'a t -> 'a option t
  val between : unit t -> unit t -> 'a t -> 'a t
  val count : int -> 'a t -> 'a list t
  val rep0 : 'a t -> 'a list t
  val rep1 : 'a t -> 'a list t
  val sep_by0 : sep:unit t -> 'a t -> 'a list t
  val sep_by1 : sep:unit t -> 'a t -> 'a list t
  val end_by0 : sep:unit t -> 'a t -> 'a list t
  val end_by1 : sep:unit t -> 'a t -> 'a list t
  val lower : char t
  val upper : char t
  val alpha : char t
  val digit : char t
end

module Make (S : S) : T with type 'a t = 'a S.t = struct
  include S
  open Bijection

  let pure_nil () =
    (* generative *)
    let compare a b =
      match (a, b) with [], [] -> 0 | (_ :: _ | []), (_ :: _ | []) -> 1
    in
    pure ~compare []

  let pure_none () =
    (* generative *)
    let compare a b =
      match (a, b) with
      | None, None -> 0
      | (Some _ | None), (Some _ | None) -> 1
    in
    pure ~compare None

  let sequence ps =
    List.fold_right
      (fun hd tl -> Exn.cons <$> (hd <*> tl))
      ps (pure_nil ())

  let choice ps = List.fold_right ( <|> ) ps (fail "choice")
  let option p = Exn.some <$> p <|> pure_none ()

  let count n p =
    let rec make acc = function 0 -> acc | n -> make (p :: acc) (n - 1) in
    sequence (make [] n)

  let rep1 p =
    let pure_nil = pure_nil () in
    fix @@ fun m -> Exn.cons <$> (p <*> (m <|> pure_nil))

  let rep0 p = rep1 p <|> pure_nil ()
  let sep_by1 ~sep p = Exn.cons <$> (p <*> rep0 (sep *> p))
  let sep_by0 ~sep p = sep_by1 ~sep p <|> pure_nil ()
  let end_by1 ~sep p = rep1 (p <* sep)
  let end_by0 ~sep p = rep0 (p <* sep)
  let between x y v = x *> v <* y
  let is_lower = function 'a' .. 'z' -> true | _ -> false
  let is_upper = function 'A' .. 'Z' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  let lower = Exn.subset is_lower <$> any
  let upper = Exn.subset is_upper <$> any
  let digit = Exn.subset is_digit <$> any
  let alpha = Exn.subset is_alpha <$> any
end

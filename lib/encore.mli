(** {1:introduction Encore, combinators to produce encoder and decoder.}

    The goal of [encore] is to provide combinators to be able to produce an
    [angstrom]'s parser or a [lavoisier]'s encoder.

    Combinators are more limited than what [angstrom] can provide, but this
    limitation gives a chance to us to produce a [lavoisier]'s encoder and be
    able to deserialize and serialize the value in the same way.

    By this fact, we can ensure the {i isomorphism}:

    {[
      val p : 'v t

      val angstrom : 'v Angstrom.t (* = to_angstrom p *)
      val lavoisier : 'v Lavoisier.t (* = to_lavoisier p *)

      assert (emit_string (parse_string str angstrom) lavoisier = str) ;
      assert (parse_string (emit_string v lavoisier) angstrom = v) ;
    ]}

    To be able to make the serializer and the deserializer, the user must
    provide some {i bijective} elements. To be able to parse and encode an
    [int64], you must provide the way to get the value from a [string] and how
    you can encode your value to a [string]:

    {[
      let int64 : (int64, string) = Bij.v
          ~fwd:Int64.of_string
          ~bwd:Int64.to_string
    ]}

    Then, you are able to play with combinators such as:

    {[
      let p =
        let open Syntax in
        int64 <$> while1 is_digit
    ]}

    For some values such as Git values, we must respect {i isomorphism} to
    ensure to inject/extract exactly the same representation of them into a
    store

    - and ensure by this way that we will produce exactly the same hash.

    {1:example Example.}

    Let's go about the tree Git object. The formal format of it is:

    {v entry := permission ' ' name '\x00' hash tree := entry * v}

    We must describe bijective elements such as:

    {[
      let permission = Bij.v ~fwd:perm_of_string ~bwd:perm_to_string

      let hash =
        Bij.v ~fwd:Digestif.SHA1.of_raw_string ~bwd:Digestif.SHA1.to_raw_string

      type entry = { perm : permission; hash : Digestif.SHA1.t; name : string }

      let entry =
        Bij.v
          ~fwd:(fun ((perm, name), hash) -> { perm; hash; name })
          ~bwd:(fun { perm; hash; name } -> ((perm, name), hash))
    ]}

    {b Note} that these functions should raise {!Bij.Bijection} if they fail
    when they parse the given string.

    Then, the format of the [entry] can be described like:

    {[
      let entry =
        let open Encore.Syntax in
        let permission = permission <$> while1 is_not_space in
        let hash = hash <$> fixed 20 in
        let name = while1 is_not_null in
        entry
        <$> (permission
            <* (Bij.char ' ' <$> any)
            <*> (name <* (Bij.char '\x00' <$> any))
            <*> hash
            <* commit)
    ]}

    And the tree Git object can be described like:

    {[ let tree = rep0 entry ]}

    Finally, with [tree] and the design of [encore], we can ensure:

    {[
      let assert random_tree_value =
        let p = to_angstrom tree in
        let d = to_lavoisier tree in
        assert (Angstrom.parse_string ~consume:All p
                  (Lavoisier.emit_string random_tree_value d) = random_tree_value)
    ]}

    The goal of such design is to describe only one time a {i format} such as
    our tree Git object and ensure no corruption when we serialize/deserialize
    values. For our Git purpose, we ensure to keep the same SHA1 (which depends
    on contents). *)

module Bij : sig
  type ('a, 'b) t
  (** Type of a bijective element. *)

  exception Bijection

  val v : fwd:('a -> 'b) -> bwd:('b -> 'a) -> ('a, 'b) t
  (** [v ~fwd ~bwd] is a bijective element such as:

      {[ assert (bwd (fwd v) = v) ]}

      {b Note:} This assertion is not proved or checked by [v] but it is
      required then. *)

  val bwd : ('a, 'b) t -> 'b -> 'a
  (** [bwd t] is the {i backward} function of [t]. *)

  val fwd : ('a, 'b) t -> 'a -> 'b
  (** [fwd t] is the {i forward} function of [t]. *)

  val flip : ('a, 'b) t -> ('b, 'a) t
  (** [flip t] is [t] where the internal {i forward} function is replaced by the
      {i backward} function and vice-versa. *)

  val product : ('u, 'v) t -> ('x, 'y) t -> ('u * 'x, 'v * 'y) t

  val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

  val commute : ('a * 'b, 'b * 'a) t

  val identity : ('a, 'a) t

  val element : ('a -> bool) -> ('a, 'a) t

  val cons : ('a * 'a list, 'a list) t

  val char : char -> (char, unit) t

  val string : string -> (string, unit) t

  val some : ('a, 'a option) t

  val obj3 : (('a * 'b) * 'c, 'a * 'b * 'c) t

  val obj4 : ((('a * 'b) * 'c) * 'd, 'a * 'b * 'c * 'd) t

  val obj5 : (((('a * 'b) * 'c) * 'd) * 'e, 'a * 'b * 'c * 'd * 'e) t

  val obj6 :
    ((((('a * 'b) * 'c) * 'd) * 'e) * 'f, 'a * 'b * 'c * 'd * 'e * 'f) t
end

module Lavoisier = Lavoisier
module Either = Either

type 'a t
(** A [encore] combinator for values of type ['a]. *)

val to_angstrom : 'a t -> 'a Angstrom.t
(** [to_angstrom t] is the parser of [t]. *)

val to_lavoisier : 'a t -> 'a Lavoisier.t
(** [to_lavoisier t] is the encoder/serializer of [t]. *)

module Syntax : sig
  val fail : string -> 'a t
  (** [fail msg] creates a combinator that will always fail with the message
      [msg]. *)

  val map : ('a, 'b) Bij.t -> 'a t -> 'b t

  val product : 'a t -> 'b t -> ('a * 'b) t

  val commit : unit t
  (** [commit] prevents backtracking beyond the current position of the input,
      allowing the manager of the input buffer to reuse the preceding bytes for
      other purposes when the combinator is used as a parser.

      As a serializer, [commit] forces to flush the internal buffer to the
      manager of the output buffer (if the current state is not into an
      alteration - see {!choice}). *)

  val fix : ('a t -> 'a t) -> 'a t

  val pure : compare:('a -> 'a -> bool) -> 'a -> 'a t

  val peek : 'a t -> 'b t -> ('a, 'b) Either.t t
  (** [peek p q] accepts/computes [p] and returns it. Otherwise, it
      accepts/computes [q] and returns it. It does not advance the input as a
      parser or produce something into the output as a {i serializer}. *)

  val const : string -> string t

  val any : char t
  (** [any] accepts/produces any character. As a parser, it returns it. As a
      serializer, it writes it. *)

  val nil : 'a list t

  val rep1 : 'a t -> 'a list t
  (** [rep1 t] runs [t] {i one} or more times and accepts/produces a list of
      results from the runs of [t]. *)

  val rep0 : 'a t -> 'a list t
  (** [rep0 t] runs [t] {i zero} of more times and accepts/produces a list of
      results from the runs of [t]. *)

  val sep_by0 : sep:unit t -> 'a t -> 'a list t

  val sep_by1 : sep:unit t -> 'a t -> 'a list t

  val while0 : (char -> bool) -> string t
  (** [while0 p] accepts/produces a [string] which respects the predicate [p].
      For example, this description:

      {v NUMBER = *DIGIT v}

      can be translated to:

      {[ let number = while0 is_digit ]} *)

  val while1 : (char -> bool) -> string t
  (** [while1 p] accepts/produces a [string] which respects the predicate [p].
      The [string] must be not empty. This description:

      {v NUMBER = 1*DIGIT v}

      can be translated to:

      {[ let number = while1 is_digit ]} *)

  val fixed : int -> string t
  (** [fixed n] accepts/produces any [string] with [n] bytes. *)

  val choice : 'a t list -> 'a t

  val sequence : 'a t list -> 'a list t

  val count : int -> 'a t -> 'a list t

  val option : 'a t -> 'a option t

  val lower : char t
  (** [lower] accepts/produces any US-ASCII lowercase letter ['a'] .. ['z'],
      that is a byte in the range \[[0x61];[0x7A]\]. *)

  val upper : char t
  (** [upper] accepts/produces any US-ASCII uppercase letter ['A'] .. ['Z'],
      that is a byte in the range \[[0x41];[0x5A]\]. *)

  val alpha : char t
  (** [alpha] is [lower <|> upper]. *)

  val digit : char t
  (** [digit] accepts/produces any US-ASCII digit ['0'] .. ['9'], that is a byte
      in the range \[[0x30];[0x39]\]. *)

  val ( <$> ) : ('a, 'b) Bij.t -> 'a t -> 'b t

  val ( <|> ) : 'a t -> 'a t -> 'a t

  val ( *> ) : unit t -> 'a t -> 'a t

  val ( <* ) : 'a t -> unit t -> 'a t

  val ( <*> ) : 'a t -> 'b t -> ('a * 'b) t
end

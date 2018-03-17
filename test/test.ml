let () = Printexc.record_backtrace true

let to_string v =
  let encoder = Git.Minienc.create 0x100 in
  let buffer = Buffer.create 16 in

  let module B = Buffer in
  let open Git.Minienc in

  let rec go = function
    | Continue { continue; encoder; } -> go (continue encoder)
    | Flush { continue; iovecs; } ->
      List.iter
        (function
          | { IOVec.buffer = `Bigstring ba; off; len; } ->
            for i = 0 to len - 1
            do B.add_char buffer (Bigarray.Array1.get ba (off + i)) done
          | { IOVec.buffer = `String s; off; len; } ->
            B.add_substring buffer s off len
          | { IOVec.buffer = `Bytes s; off; len; } ->
            B.add_subbytes buffer s off len)
        iovecs;
      go (continue (IOVec.lengthv iovecs))
    | End v -> v in
  go (Git_unix.FS.Value.M.encoder v (flush (fun _ -> End ())) encoder);
  B.contents buffer

module Option =
struct
  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let ( >>= ) v f = map f v
end

module Minienc = Git.Minienc
module G = Git
module Git = Git_unix.FS
module Http = Git_unix.HTTP(Git)

open Encore

let safe_exn tag f x =
  try f x with _ -> Bijection.Exn.fail (fst tag) (snd tag)

let flip (a, b) = (b ,a)

module Iso =
struct
  let int64 =
    let tag = ("string", "int64") in
    Bijection.make_exn ~tag
      ~fwd:(safe_exn tag Int64.of_string)
      ~bwd:(safe_exn (flip tag) Int64.to_string)

  let hash =
    let tag = ("string", "hash") in
    Bijection.make_exn ~tag
      ~fwd:(safe_exn tag Git.Hash.of_string)
      ~bwd:(safe_exn (flip tag) Git.Hash.to_string)

  let hex =
    let tag = ("string:hex", "hash") in
    Bijection.make_exn ~tag
      ~fwd:(safe_exn tag Git.Hash.of_hex)
      ~bwd:(safe_exn (flip tag) Git.Hash.to_hex)

  let perm =
    let tag = ("string-to-sp", "perm") in
    Bijection.make_exn ~tag
      ~fwd:(safe_exn tag Git.Value.Tree.perm_of_string)
      ~bwd:(safe_exn (flip tag) Git.Value.Tree.string_of_perm)

  let entry =
    Bijection.make_exn
      ~tag:("perm * string * hash", "entry")
      ~fwd:(fun ((perm, name), node) -> { Git.Value.Tree.perm; name; node; })
      ~bwd:(fun { Git.Value.Tree.perm; name; node; } -> ((perm, name), node))

  let tree =
    let tag = ("entry list", "tree") in
    Bijection.make_exn ~tag
      ~fwd:(fun l -> Git.Value.Tree (Git.Value.Tree.of_list l))
      ~bwd:(function Git.Value.Tree l -> Git.Value.Tree.to_list l
                   | _ -> Bijection.Exn.fail "tree" "entry list")

  let chare chr =
    Bijection.Exn.element ~tag:"char" ~compare:Char.equal chr

  let stringe str =
    Bijection.Exn.element ~tag:"string" ~compare:String.equal str

  let cstruct =
    Bijection.make_exn
      ~tag:("cstruct", "bigstring")
      ~fwd:(fun x -> Cstruct.of_bigarray x)
      ~bwd:(Cstruct.to_bigarray)

  type kind =
    [ `Tree | `Commit | `Tag | `Blob ]

  let kind =
    let tag = ("string", "kind") in
    Bijection.make_exn ~tag
      ~fwd:(function "tree"   -> `Tree
                   | "commit" -> `Commit
                   | "tag"    -> `Tag
                   | "blob"   -> `Blob
                   | s -> Bijection.Exn.fail s "kind")
      ~bwd:(function `Tree   -> "tree"
                   | `Commit -> "commit"
                   | `Tag    -> "tag"
                   | `Blob   -> "blob")

  let tz_offset =
    Bijection.make_exn
      ~tag:("sign * int * int", "tz-data")
      ~fwd:(fun (sign, hours, minutes) ->
          if hours = 0 && minutes = 0
          then None
          else Some { G.User.sign; hours; minutes; })
      ~bwd:(function
          | Some { G.User.sign; hours; minutes; } -> (sign, hours, minutes)
          | None -> (`Plus, 0, 0))

  let blob =
    Bijection.make_exn
      ~tag:("cstruct", "blob")
      ~fwd:(fun x -> Git.Value.Blob (Git.Value.Blob.of_cstruct x))
      ~bwd:(function Git.Value.Blob b -> Git.Value.Blob.to_cstruct b
                   | _ -> Bijection.Exn.fail "blob" "cstruct")

  let user =
    Bijection.make_exn
      ~tag:("string * string * int64 * tz-data", "user")
      ~fwd:(fun (name, email, time, date) ->
          { G.User.name; email; date = (time, date) })
      ~bwd:(fun { G.User.name; email; date = (time, date) } -> (name, email, time, date))

  let tag =
    let to_kind = function
      | `Tree   -> Git.Value.Tag.Tree
      | `Blob   -> Git.Value.Tag.Blob
      | `Tag    -> Git.Value.Tag.Tag
      | `Commit -> Git.Value.Tag.Commit in
    let of_kind = function
      | Git.Value.Tag.Tree   -> `Tree
      | Git.Value.Tag.Blob   -> `Blob
      | Git.Value.Tag.Commit -> `Commit
      | Git.Value.Tag.Tag    -> `Tag in
    Bijection.make_exn
      ~tag:("hash * kind * string * user * string", "tag")
      ~fwd:(function
          | ((_, hash), (_, kind), (_, tag), tagger, message) ->
            Git.Value.Tag.make hash (to_kind kind) ?tagger:Option.(tagger >>= snd) ~tag message
            |> Git.Value.tag)
      ~bwd:(function
          | Git.Value.Tag tag ->
            let tagger tag = match Git.Value.Tag.tagger tag with
              | Some tagger -> Some ("tagger", tagger)
              | None -> None in
            (("object", Git.Value.Tag.obj tag),
             ("type", of_kind @@ Git.Value.Tag.kind tag),
             ("tag", Git.Value.Tag.tag tag),
             tagger tag,
             Git.Value.Tag.message tag)
          | _ -> Bijection.Exn.fail "not tag" "value")

  let commit =
    Bijection.make_exn
      ~tag:("hash * parents * user * user * fields * string", "commit")
      ~fwd:(function
          | ((_, tree), parents, (_, author), (_, committer), extra, message) ->
            Git.Value.Commit.make ~tree ~author ~committer ~extra ~parents:(List.map snd parents) message |> Git.Value.commit)
      ~bwd:(function
          | Git.Value.Commit c ->
            (("tree", Git.Value.Commit.tree c),
             (List.map (fun x -> ("parent", x)) (Git.Value.Commit.parents c)),
             ("author", Git.Value.Commit.author c),
             ("committer", Git.Value.Commit.committer c),
             Git.Value.Commit.extra c,
             Git.Value.Commit.message c)
          | _ -> Bijection.Exn.fail "commit" "value")

  let git =
    let kind_to_string = function
      | `Tree -> "tree"
      | `Commit -> "commit"
      | `Blob -> "blob"
      | `Tag -> "tag" in
    let value_to_string = function
      | Git.Value.Tree _ -> "tree"
      | Git.Value.Tag _ -> "tag"
      | Git.Value.Commit _ -> "commit"
      | Git.Value.Blob _ -> "blob" in
    Bijection.make_exn
      ~tag:("kind * value", "value")
      ~fwd:(function `Tree,   _, Git.Value.Tree l   -> Git.Value.Tree l
                   | `Tag,    _, Git.Value.Tag t    -> Git.Value.Tag t
                   | `Commit, _, Git.Value.Commit c -> Git.Value.Commit c
                   | `Blob,   _, Git.Value.Blob b   -> Git.Value.Blob b
                   | kind, _, value -> Bijection.Exn.fail (Fmt.strf "kind:%s * value:%s" (kind_to_string kind) (value_to_string value)) "value")
      ~bwd:(function Git.Value.Tree _ as t   -> `Tree,   Git.Value.F.length t, t
                   | Git.Value.Tag _ as t    -> `Tag,    Git.Value.F.length t, t
                   | Git.Value.Commit _ as t -> `Commit, Git.Value.F.length t, t
                   | Git.Value.Blob _ as t   -> `Blob,   Git.Value.F.length t, t)
end

module User =
struct
  module Meta (M: Meta.S) =
  struct
    open M
    open Bijection

    let is_not_lt chr = chr <> '<'
    let is_not_gt chr = chr <> '>'
    let is_digit = function '0' .. '9' -> true | _ -> false

    let date =
      let plus =
        make_exn
          ~tag:("unit", "`plus")
          ~fwd:(fun () -> `Plus)
          ~bwd:(function `Plus -> () | _ -> Bijection.Exn.fail "`plus" "unit")
        <$> (Iso.chare '+' <$> char) in
      let minus =
        make_exn
          ~tag:("unit", "`minus")
          ~fwd:(fun () -> `Minus)
          ~bwd:(function `Minus -> () | _ -> Bijection.Exn.fail "`minus" "unit")
        <$> (Iso.chare '-' <$> char) in
      let digit2 =
        make_exn
          ~tag:("char * char", "int")
          ~fwd:(function ('0' .. '9' as a), ('0' .. '9' as b) -> (Char.code a - 48) * 10 + (Char.code b - 48)
                       | _, _ -> Bijection.Exn.fail "char * char" "int")
          ~bwd:(fun n -> Char.chr (n / 10 + 48), Char.chr (n mod 10 + 48))
        <$> ((satisfy is_digit) <*> (satisfy is_digit)) in
      (compose obj3 Iso.tz_offset) <$> ((plus <|> minus) <*> digit2 <*> digit2)

    let chop =
      make_exn
        ~tag:("string", "chop string")
        ~fwd:(fun s -> String.sub s 0 (String.length s - 1))
        ~bwd:(fun s -> s ^ " ")

    let user =
      (compose obj4 Iso.user) <$>
      ((chop <$> ((while1 is_not_lt) <* (Iso.chare '<' <$> char)))
       <*> ((while1 is_not_gt) <* (Iso.stringe "> " <$> string "> "))
       <*> ((Iso.int64 <$> while1 is_digit) <* (Iso.chare ' ' <$> char))
       <*> date)
  end

  module Decoder = Meta(Proxy_decoder.Impl)
  module Encoder = Meta(Proxy_encoder.Impl)
end

module Make (M: Meta.S) =
struct
  module Meta = Meta.Make(M)
  open Meta
  open Bijection

  let is_not_sp chr = chr <> ' '
  let is_not_nl chr = chr <> '\x00'
  let is_not_lf chr = chr <> '\x0a'
  let is_digit = function '0' .. '9' -> true | _ -> false

  let hash = Iso.hash <$> (take Git.Hash.Digest.length)
  let hex = Iso.hex <$> (take (Git.Hash.Digest.length * 2))
  let perm = Iso.perm <$> (while1 is_not_sp)
  let name = while1 is_not_nl
  let kind = Iso.kind <$> (string "tree" <|> string "commit" <|> string "tag" <|> string "blob")

  let entry =
    Iso.entry <$>
    ((perm <* (Iso.chare ' ' <$> char))
     <*> (name <* (Iso.chare '\x00' <$> char))
     <*> hash)

  let value =
    let sep = Iso.stringe "\n " <$> string "\n " in
    sep_by0 ~sep (while0 is_not_lf)

  let extra =
    (while1 (fun chr -> (is_not_sp chr) && (is_not_lf chr)) <* (Iso.chare ' ' <$> char))
    <*> (value <* (Iso.chare '\x0a' <$> char))

  let binding ?key value =
    let value = (value <$> (while1 is_not_lf <* (Iso.chare '\x0a' <$> char))) in

    match key with
    | Some key ->
      ((string key) <* (Iso.chare ' ' <$> char)) <*> value
    | None ->
      (while1 is_not_sp <* (Iso.chare ' ' <$> char)) <*> value

  let user_of_string =
    make_exn
      ~tag:("string", "user")
      ~fwd:(fun s -> match Angstrom.parse_string User.Decoder.user s with
          | Ok v -> v
          | Error _ -> Bijection.Exn.fail "string" "user")
      ~bwd:(Encoder.to_string User.Encoder.user)

  let tag =
    binding ~key:"object" Iso.hex
    <*> binding ~key:"type" Iso.kind
    <*> binding ~key:"tag" identity
    <*> (option (binding ~key:"tagger" user_of_string))
    <*> while1 (fun _ -> true)

  let commit =
    binding ~key:"tree" Iso.hex
    <*> (rep0 (binding ~key:"parent" Iso.hex))
    <*> binding ~key:"author" user_of_string
    <*> binding ~key:"committer" user_of_string
    <*> (rep0 extra)
    <*> while1 (fun _ -> true)

  let tree = Iso.tree <$> (rep0 entry)
  let tag = (compose obj5 Iso.tag) <$> tag
  let commit = (compose obj6 Iso.commit) <$> commit
  let blob = (compose Iso.cstruct Iso.blob) <$> (bwhile0 (fun _ -> true))

  let length = Iso.int64 <$> while1 is_digit

  let git =
    let value kind p =
      ((Iso.kind <$> string kind) <* (Iso.chare ' ' <$> char))
      <*> (length <* (Iso.chare '\x00' <$> char))
      <*> p in

    (compose obj3 Iso.git)
    <$> ((value "commit" commit)
         <|> (value "tag" tag)
         <|> (value "tree" tree)
         <|> (value "blob" blob))
end

let pp_string = Minienc.pp_scalar ~get:String.get ~length:String.length
let str = Alcotest.testable pp_string String.equal

let store_err x = `Store x

module A = Make(Proxy_decoder.Impl)
module C = Make(Proxy_encoder.Impl)

let read_parse_and_write t hash =
  let ( >>|= ) = Lwt_result.bind in
  let ( >>!= ) v f = Lwt_result.map_err f v in

  Git.read t hash >>!= store_err >>|= fun t ->
  let raw = to_string t in

  match Angstrom.parse_string A.git raw with
  | Ok t' ->
     let raw' = Encoder.to_string C.git t' in

     Alcotest.(check str) (Fmt.strf "raw: %a" Git.Hash.pp hash) raw raw';
     Alcotest.(check (module Git.Hash)) (Fmt.strf "hash: %a" Git.Hash.pp hash) hash (Git.Value.digest t');
     Alcotest.(check (module Git.Value)) (Fmt.strf "value: %a" Git.Hash.pp hash) t t';

     Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (`Angstrom (hash, err)))

let run t : (unit Alcotest.test_case list, Git.error) result Lwt.t =
  let module A = Make(Proxy_decoder.Impl) in
  let module C = Make(Proxy_encoder.Impl) in

  let open Lwt.Infix in
  let ( >>|= ) = Lwt_result.bind in

  Git.Ref.graph t >>|= fun map ->
  let master = Git.Reference.Map.find Git.Reference.master map in
  Git.fold t (fun hashes ?name:_ ~length:_ hash _ -> Lwt.return (hash :: hashes)) ~path:(Fpath.v "/") [] master >>= fun hashes ->
  Lwt_list.map_p
    (fun hash ->
      Alcotest_lwt.test_case
        (Fmt.strf "%a" Git.Hash.pp hash)
        `Quick
        (fun _ () -> read_parse_and_write t hash >>= function
                     | Ok () -> Lwt.return ()
                     | Error (`Angstrom (hash, err)) ->
                        Alcotest.failf "Retrieve an error on %a: %s" Git.Hash.pp hash err
                     | Error (`Store err) ->
                        Alcotest.failf "Retrieve a store error: %a" Git.pp_error err)
      |> Lwt.return)
    hashes >>= fun tests -> Lwt.return (Ok tests)

let pwd = Unix.getcwd ()
let repository = Uri.of_string "https://github.com/dinosaure/encore.git"

let clone_and_make () =
  let ( >>!= ) v f = Lwt_result.map_err f v in
  let ( >>|= ) = Lwt_result.bind in

  Git.create ~root:Fpath.(v pwd / "repository") () >>!= store_err >>|= fun t ->
  Http.clone t
    ~reference:Git.Reference.(master, master) repository
  >>|= fun () -> Fmt.(pf stdout) "Repository %a cloned at %a.\n%!" Uri.pp_hum repository Fpath.pp Fpath.(v pwd / "repository"); run t >>!= store_err

let iso =
  let open Bijection in
  let test_fwd ~global value t expect input =
    Alcotest.test_case (Fmt.strf "%s (fwd)" global)
      `Quick
      (fun () -> Alcotest.(check value) (Fmt.strf "%a" (Alcotest.pp value) expect) expect (fwd t input)) in
  let test_bwd ~global value t input expect =
    Alcotest.test_case (Fmt.strf "%s (bwd)" global)
      `Quick
      (fun () -> Alcotest.(check value) (Fmt.strf "%a" (Alcotest.pp value) expect) expect (fwd t input)) in
  [ test_fwd ~global:"string"   Alcotest.string          Exn.string                 "foo"      ['f'; 'o'; 'o'; ]
  ; test_fwd ~global:"string"   Alcotest.string          Exn.string                 ""         []
  ; test_bwd ~global:"string"   Alcotest.string          Exn.string                 ['f'; 'o'; 'o'; ] "foo"
  ; test_bwd ~global:"string"   Alcotest.string          Exn.string                 []         ""
  ; test_fwd ~global:"int"      Alcotest.int             Exn.int                    42         "42"
  ; test_bwd ~global:"int"      Alcotest.int             Exn.int                    "42"       42
  ; test_fwd ~global:"int"      Alcotest.int             Exn.int                    (-42)      "-42"
  ; test_bwd ~global:"int"      Alcotest.int             Exn.int                    "-42"      (-42)
  ; test_fwd ~global:"bool"     Alcotest.bool            Exn.bool                   true       "true"
  ; test_bwd ~global:"bool"     Alcotest.bool            Exn.bool                   "true"     true
  ; test_fwd ~global:"bool"     Alcotest.bool            Exn.bool                   false      "false"
  ; test_bwd ~global:"bool"     Alcotest.bool            Exn.bool                   "false"    false
  ; test_fwd ~global:"identity" Alcotest.int             identity                   42         42
  ; test_bwd ~global:"identity" Alcotest.int             identity                   42         42
  ; test_fwd ~global:"commute"  Alcotest.(pair int int)  commute                    (1, 2)     (2, 1)
  ; test_bwd ~global:"commute"  Alcotest.(pair int int)  commute                    (1, 2)     (2, 1)
  ; test_fwd ~global:"compose"  Alcotest.(pair int int)  (compose commute commute)  (1, 2)     (1, 2)
  ; test_fwd ~global:"inverse"  Alcotest.int             Exn.int                    42         "42"
  ; test_bwd ~global:"inverse"  Alcotest.string          (flip Exn.int)             42         "42"
  ; test_fwd ~global:"inverse"  Alcotest.string          (flip Exn.int)             "42"       42
  ; test_bwd ~global:"inverse"  Alcotest.int             Exn.int                    "42"       42
  ; test_fwd ~global:"product"  Alcotest.(pair int bool) (product Exn.int Exn.bool) (42, true) ("42", "true")
  ; ]

module type COMBINATOR =
  sig
    type sentinel

    module Make : functor (M: Meta.S) -> sig val p: sentinel M.t end
  end

let make
    : type sentinel.
           (module COMBINATOR with type sentinel = sentinel) ->
           ((sentinel -> string) * (string -> sentinel))
  = fun (module Combinator) ->
  let to_string v =
    let module Enc = Combinator.Make(Proxy_encoder.Impl) in
    Encoder.to_string Enc.p v in
  let of_string s =
    let module Dec = Combinator.Make(Proxy_decoder.Impl) in
    match Angstrom.parse_string Dec.p s with
    | Ok v -> v
    | Error err -> invalid_arg err in
  to_string, of_string

let combinator =
  let make_test
      : type sentinel.
             string
             -> (module COMBINATOR with type sentinel = sentinel)
             -> sentinel Alcotest.testable
             -> sentinel
             -> string
             -> unit Alcotest.test_case list
    = fun name (module Combinator) value sentinel s ->
    let to_string, of_string = make (module Combinator) in

    [ Alcotest.test_case name `Quick
        (fun () ->
          Alcotest.(check value) "decode" (of_string s) sentinel)
    ; Alcotest.test_case name `Quick
        (fun () ->
          Alcotest.(check string) "encode" (to_string sentinel) s) ] in

  let open Bijection in

  List.concat
    [ make_test "( *> )"
        (module
           (struct
             type sentinel = unit

             module Make =
               functor (S: Meta.S) -> struct
                 include Meta.Make(S)

                 let a = Exn.element ~tag:"char" ~compare:Char.equal 'a' <$> char
                 let b = Exn.element ~tag:"char" ~compare:Char.equal 'b' <$> char
                 let p : sentinel S.t = a *> b end end))
        Alcotest.unit () "ab"
    ; make_test "( <* )"
        (module
           (struct
             type sentinel = unit

             module Make =
               functor (S: Meta.S) -> struct
                 include Meta.Make(S)

                 let a = Exn.element ~tag:"char" ~compare:Char.equal 'a' <$> char
                 let b = Exn.element ~tag:"char" ~compare:Char.equal 'b' <$> char
                 let p : sentinel S.t = a <* b end end))
        Alcotest.unit () "ab"
    ; (let combinator =
         (module
            (struct
              type sentinel = char

              module Make =
                functor (S: Meta.S) -> struct
                  include Meta.Make(S)

                  let a = Exn.subset ((=) 'a') <$> char
                  let b = Exn.subset ((=) 'b') <$> char
                  let c = Exn.subset ((=) 'c') <$> char
                  let p : sentinel S.t = choice [ a; b; c; ] end end) : COMBINATOR with type sentinel = char) in
       List.concat
         [ make_test "choice" combinator Alcotest.char 'a' "a"
         ; make_test "choice" combinator Alcotest.char 'b' "b"
         ; make_test "choice" combinator Alcotest.char 'c' "c" ])
    ; make_test "option"
        (module
           (struct
             type sentinel = char option

             module Make =
               functor (S: Meta.S) -> struct
                 include Meta.Make(S)

                 let p : sentinel S.t = option char end end))
        Alcotest.(option char) (Some 'a') "a"
    ; (let combinator =
         (module
            (struct
              type sentinel = char list

              module Make =
                functor (S: Meta.S) -> struct
                  include Meta.Make(S)

                  let p : sentinel S.t = count 3 char end end) : COMBINATOR with type sentinel = char list) in
       List.concat
         [ make_test "count" combinator Alcotest.(list char) [ 'b'; 'a'; 'r'; ] "bar" ])
    ; (let combinator =
         (module
            (struct
              type sentinel = char list

              module Make =
                functor (S: Meta.S) -> struct
                  include Meta.Make(S)

                  let p : sentinel S.t = rep0 char end end) : COMBINATOR with type sentinel = char list) in
       List.concat
         [ make_test "rep0" combinator Alcotest.(list char) [ 'b'; 'a'; 'r'; ] "bar"
         ; make_test "rep0" combinator Alcotest.(list char) [] "" ])
    ; make_test "rep1"
        (module
           (struct
             type sentinel = char list

             module Make =
               functor (S: Meta.S) -> struct
                 include Meta.Make(S)

                 let p : sentinel S.t = rep1 char end end))
        Alcotest.(list char) [ 'b'; 'a'; 'r'; ] "bar"
    ; (let combinator =
         (module
            (struct
              type sentinel = char list

              module Make =
                functor (S: Meta.S) -> struct
                  include Meta.Make(S)

                  let comma = Exn.element ~tag:"char" ~compare:Char.equal ',' <$> char
                  let p : sentinel S.t = sep_by0 ~sep:comma char end end) : COMBINATOR with type sentinel = char list) in
       List.concat
         [ make_test "sep_by0" combinator Alcotest.(list char) [ 'b'; 'a'; 'r'; ] "b,a,r"
         ; make_test "sep_by0" combinator Alcotest.(list char) [] "" ])
    ; make_test "sep_sep1"
        (module
           (struct
             type sentinel = char list

             module Make =
               functor (S: Meta.S) -> struct
                 include Meta.Make(S)

                  let comma = Exn.element ~tag:"char" ~compare:Char.equal ',' <$> char
                 let p : sentinel S.t = sep_by1 ~sep:comma char end end))
        Alcotest.(list char) [ 'b'; 'a'; 'r'; ] "b,a,r"
    ; (let combinator =
         (module
            (struct
              type sentinel = char list

              module Make =
                functor (S: Meta.S) -> struct
                  include Meta.Make(S)

                  let comma = Exn.element ~tag:"char" ~compare:Char.equal ',' <$> char
                  let p : sentinel S.t = end_by0 ~sep:comma char end end) : COMBINATOR with type sentinel = char list) in
       List.concat
         [ make_test "end_by0" combinator Alcotest.(list char) [ 'b'; 'a'; 'r'; ] "b,a,r,"
         ; make_test "end_by0" combinator Alcotest.(list char) [] ""
         ; make_test "end_by0" combinator Alcotest.(list char) [ 'b' ] "b," ])
    ; make_test "sep_sep1"
        (module
           (struct
             type sentinel = char list

             module Make =
               functor (S: Meta.S) -> struct
                 include Meta.Make(S)

                  let comma = Exn.element ~tag:"char" ~compare:Char.equal ',' <$> char
                 let p : sentinel S.t = end_by1 ~sep:comma char end end))
        Alcotest.(list char) [ 'b'; 'a'; 'r'; ] "b,a,r,"
    ; make_test "sequence"
        (module
           (struct
             type sentinel = char list

             module Make =
               functor (S: Meta.S) -> struct
                 include Meta.Make(S)

                 let p : sentinel S.t = sequence [char; char; char; ] end end))
        Alcotest.(list char) ['b'; 'a'; 'r'; ] "bar" ]

let main () =
  let open Lwt.Infix in

  clone_and_make () >>= function
  | Ok tests ->
     Alcotest.run "isomorpism" [ "isomorphism", iso
                               ; "combinator", combinator
                               ; "encore", tests ];
     Lwt.return ()
  | Error err ->
     Fmt.(pf stderr) "Retrieve an error: %a.\n%!" Http.pp_error err;
     Lwt.return ()

let () = Lwt_main.run (main ())

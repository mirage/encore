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

open Encore

let safe_exn tag f x =
  try f x with _ -> Bijection.fail (fst tag) (snd tag)

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
                   | _ -> Bijection.fail "tree" "entry list")

  let chare chr =
    Bijection.element ~tag:"char" ~compare:Char.equal chr

  let stringe str =
    Bijection.element ~tag:"string" ~compare:String.equal str

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
                   | s -> Bijection.fail s "kind")
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
                   | _ -> Bijection.fail "blob" "cstruct")

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
          | _ -> Bijection.fail "not tag" "value")

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
          | _ -> Bijection.fail "commit" "value")

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
                   | kind, _, value -> Bijection.fail (Fmt.strf "kind:%s * value:%s" (kind_to_string kind) (value_to_string value)) "value")
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
          ~bwd:(function `Plus -> () | _ -> Bijection.fail "`plus" "unit")
        <$> (char <$ Iso.chare '+') in
      let minus =
        make_exn
          ~tag:("unit", "`minus")
          ~fwd:(fun () -> `Minus)
          ~bwd:(function `Minus -> () | _ -> Bijection.fail "`minus" "unit")
        <$> (char <$ Iso.chare '-') in
      let digit2 =
        make_exn
          ~tag:("char * char", "int")
          ~fwd:(function ('0' .. '9' as a), ('0' .. '9' as b) -> (Char.code a - 48) * 10 + (Char.code b - 48)
                       | _, _ -> Bijection.fail "char * char" "int")
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
      ((chop <$> ((while1 is_not_lt) <* (char <$ Iso.chare '<')))
       <*> ((while1 is_not_gt) <* (string "> "<$ Iso.stringe "> "))
       <*> ((Iso.int64 <$> while1 is_digit) <* (char <$ Iso.chare ' '))
       <*> date)
  end

  module Decoder = Meta(Proxy_angstrom.Impl)
  module Encoder = Meta(Proxy_condorcet.Impl)
end

module Make (M: Meta.S) =
struct
  open M
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
    ((perm <* (char <$ Iso.chare ' '))
     <*> (name <* (char <$ Iso.chare '\x00'))
     <*> hash)

  let value =
    let sep = string "\n " <$ Iso.stringe "\n " in
    list ~sep (while0 is_not_lf)

  let extra =
    (while1 (fun chr -> (is_not_sp chr) && (is_not_lf chr)) <* (char <$ Iso.chare ' '))
    <*> (value <* (satisfy ((=) '\x0a') <$ Iso.chare '\x0a'))

  let binding ?key value =
    let value = (value <$> (while1 is_not_lf <* (char <$ Iso.chare '\x0a'))) in

    match key with
    | Some key ->
      ((string key) <* (char <$ Iso.chare ' ')) <*> value
    | None ->
      (while1 is_not_sp <* (char <$ Iso.chare ' ')) <*> value

  let user_of_string =
    make_exn
      ~tag:("string", "user")
      ~fwd:(fun s -> match Angstrom.parse_string User.Decoder.user s with
          | Ok v -> v
          | Error _ -> Bijection.fail "string" "user")
      ~bwd:(Condorcet.to_string User.Encoder.user)

  let tag =
    binding ~key:"object" Iso.hex
    <*> binding ~key:"type" Iso.kind
    <*> binding ~key:"tag" identity
    <*> (option (binding ~key:"tagger" user_of_string))
    <*> while1 (fun _ -> true)

  let commit =
    binding ~key:"tree" Iso.hex
    <*> (list (binding ~key:"parent" Iso.hex))
    <*> binding ~key:"author" user_of_string
    <*> binding ~key:"committer" user_of_string
    <*> (list extra)
    <*> while1 (fun _ -> true)

  let tree = Iso.tree <$> (list entry)
  let tag = (compose obj5 Iso.tag) <$> tag
  let commit = (compose obj6 Iso.commit) <$> commit
  let blob = (compose Iso.cstruct Iso.blob) <$> (bwhile0 (fun _ -> true))

  let length = Iso.int64 <$> while1 is_digit

  let git =
    let value kind p =
      ((Iso.kind <$> string kind) <* (char <$ Iso.chare ' '))
      <*> (length <* (char <$ Iso.chare '\x00'))
      <*> p in

    (compose obj3 Iso.git)
    <$> ((value "commit" commit)
         <|> (value "tag" tag)
         <|> (value "tree" tree)
         <|> (value "blob" blob))
end

let pp_string = Minienc.pp_scalar ~get:String.get ~length:String.length

let ret = ref true

let main root =
  let module A = Make(Proxy_angstrom.Impl) in
  let module C = Make(Proxy_condorcet.Impl) in

  let open Lwt.Infix in
  let ( >>!= ) = Lwt_result.bind in

  Git.create ~root () >>!= fun t ->
  Git.Ref.graph t >>!= fun map ->
  let master = Git.Reference.Map.find Git.Reference.master map in
  Git.iter t
    (fun hash t ->
       let raw = to_string t in

       (match Angstrom.parse_string A.git raw with
        | Ok t' ->
          (try
             let raw' = Condorcet.to_string C.git t' in

             if not (String.equal raw raw')
             then begin
               ret := false;

               Fmt.(pf stdout) "value source: %a.\n%!" Git.Value.pp t;
               Fmt.(pf stdout) "value result: %a.\n%!" Git.Value.pp t';

               Fmt.(pf stdout) "source: %a.\n%!" (Fmt.hvbox pp_string) raw;
               Fmt.(pf stdout) "result: %a.\n%!" (Fmt.hvbox pp_string) raw';
             end;

             Fmt.(pf stdout) "%a: hash equal: %b.\n%!" Git.Hash.pp hash Git.(Hash.equal hash (Value.digest t'));
             Lwt.return ()
           with
           | Condorcet.Fail err ->
             ret := false;
             Fmt.(pf stderr) "Got a condorcet error (%s) (%a): %a.\n%!"
               err
               Git.Hash.pp hash
               (Fmt.hvbox pp_string) raw;
             Lwt.return ()
           | Bijection.Bijection (a, b) ->
             ret := false;
             Fmt.(pf stderr) "Got a bijection error from %s to %s (%a): %a.\n%!"
               a b
               Git.Hash.pp hash
               (Fmt.hvbox pp_string) raw;
             Lwt.return ());
        | Error err ->
          ret := false;
          Fmt.(pf stderr) "Got an error (%a): %s.\n%!"
            Git.Hash.pp (Git.Value.digest t) err;
          Lwt.return ();
        | exception Bijection.Bijection (a, b) ->
          ret := false;
          Fmt.(pf stderr) "Got a bijection error from %s to %s (%a): %a.\n%!"
            a b
            Git.Hash.pp hash
            (Fmt.hvbox pp_string) raw;
          Lwt.return ()
        | exception Condorcet.Fail err ->
          ret := false;
          Fmt.(pf stderr) "Got a condorcet error (%s) (%a): %a.\n%!"
            err
            Git.Hash.pp hash
            (Fmt.hvbox pp_string) raw;
          Lwt.return ())) master >>= fun () ->
  Lwt.return (Ok ())

let () =
  (match Lwt_main.run (main (Fpath.v Sys.argv.(1))) with
   | Ok () -> ()
   | Error err -> Fmt.(pf stderr) "Got a git error: %a.\n%!" Git.pp_error err);
  if !ret then exit 0 else exit 1

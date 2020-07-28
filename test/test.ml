let () = Printexc.record_backtrace true

open Encore

let make_test :
    type v.
    string ->
    v t ->
    v Alcotest.testable ->
    v ->
    string ->
    unit Alcotest.test_case =
 fun name t value v str ->
  let of_string str =
    let p = to_angstrom t in
    match Angstrom.parse_string ~consume:Prefix p str with
    | Ok v -> v
    | Error err -> invalid_arg err in
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(check value) "decode" (of_string str) v

let make_reject : type v. string -> v t -> string -> unit Alcotest.test_case =
 fun name t str ->
  let of_string str =
    let p = to_angstrom t in
    match Angstrom.parse_string ~consume:Prefix p str with
    | Ok v -> v
    | Error err -> invalid_arg err in
  Alcotest.test_case name `Quick @@ fun () ->
  try
    ignore @@ of_string str ;
    Alcotest.failf "test %s works" name
  with _ -> ()

let parser =
  let any =
    [
      make_test "any" Syntax.any Alcotest.char 'f' "foo";
      make_test "any" Syntax.any Alcotest.char 'b' "bar";
      make_reject "any" Syntax.any "";
    ] in
  let a_and_b =
    let c =
      let a = Bij.element (Char.equal 'a') in
      let b = Bij.element (Char.equal 'b') in
      Syntax.(a <$> any <*> (b <$> any)) in
    [
      make_test "(<*>)" c Alcotest.(pair char char) ('a', 'b') "ab";
      make_reject "(<*>)" c "ac";
      make_reject "(<*>)" c "cb";
      make_reject "(<*>)" c "a";
      make_reject "(<*>)" c "b";
    ] in
  let fail =
    let c = Syntax.fail "fail" in
    [
      make_reject "fail" c "foo";
      make_reject "fail" c "bar";
      make_reject "fail" c "";
    ] in
  let pure =
    [
      make_test "pure"
        (Syntax.pure ~compare:(fun () () -> true) ())
        Alcotest.unit () "foo";
      make_test "pure"
        (Syntax.pure ~compare:(fun () () -> true) ())
        Alcotest.unit () "bar";
      make_test "pure"
        (Syntax.pure ~compare:(fun () () -> true) ())
        Alcotest.unit () "";
      make_test "pure"
        Syntax.(pure ~compare:Char.equal 'a' <*> any)
        Alcotest.(pair char char)
        ('a', 'b') "bar";
      make_test "pure"
        Syntax.(pure ~compare:Char.equal 'b' <*> any)
        Alcotest.(pair char char)
        ('b', 'b') "bar";
    ] in
  let a_or_b =
    let a_or_b =
      let open Syntax in
      Bij.element (Char.equal 'a')
      <$> any
      <|> (Bij.element (Char.equal 'b') <$> any) in
    let b_or_a =
      let open Syntax in
      Bij.element (Char.equal 'b')
      <$> any
      <|> (Bij.element (Char.equal 'a') <$> any) in
    [
      make_test "(<|>)" a_or_b Alcotest.char 'a' "a";
      make_test "(<|>)" b_or_a Alcotest.char 'a' "a";
      make_reject "(<|>)" a_or_b "c";
      make_reject "(<|>)" b_or_a "c";
    ] in
  List.concat [ any; a_and_b; fail; pure; a_or_b ]

let make_test : type v. string -> v t -> v -> string -> unit Alcotest.test_case
    =
 fun name t v str ->
  let to_string v =
    let d = to_lavoisier t in
    Lavoisier.emit_string v d in
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(check string) "encode" (to_string v) str

let make_reject : type v. string -> v t -> v -> unit Alcotest.test_case =
 fun name t v ->
  let to_string v =
    let d = to_lavoisier t in
    Lavoisier.emit_string v d in
  Alcotest.test_case name `Quick @@ fun () ->
  try
    ignore @@ to_string v ;
    Alcotest.failf "test %s works" name
  with _ -> ()

let printer =
  let any = [ make_test "any" Syntax.any 'f' "f" ] in
  let a_and_b =
    let c =
      let a = Bij.element (Char.equal 'a') in
      let b = Bij.element (Char.equal 'b') in
      Syntax.(a <$> any <*> (b <$> any)) in
    [
      make_test "(<*>)" c ('a', 'b') "ab";
      make_reject "(<*>)" c ('a', 'c');
      make_reject "(<*>)" c ('c', 'b');
      make_reject "(<*>)" c ('c', 'c');
    ] in
  let fail = [ make_reject "fail" (Syntax.fail "fail") () ] in
  let pure =
    [
      make_test "pure" Syntax.(pure ~compare:(fun () () -> true) ()) () "";
      make_test "pure" Syntax.(pure ~compare:( = ) 42) 42 "";
      make_reject "pure" Syntax.(pure ~compare:( = ) 42) 24;
    ] in
  let a_or_b =
    let a_or_b =
      let open Syntax in
      Bij.element (Char.equal 'a')
      <$> any
      <|> (Bij.element (Char.equal 'b') <$> any) in
    let b_or_a =
      let open Syntax in
      Bij.element (Char.equal 'b')
      <$> any
      <|> (Bij.element (Char.equal 'a') <$> any) in
    [
      make_test "(<|>)" a_or_b 'a' "a";
      make_test "(<|>)" a_or_b 'b' "b";
      make_test "(<|>)" b_or_a 'a' "a";
      make_test "(<|>)" b_or_a 'b' "b";
      make_reject "(<|>)" a_or_b 'c';
      make_reject "(<|>)" b_or_a 'c';
    ] in
  List.concat [ any; fail; a_and_b; pure; a_or_b ]

let make : type v. v t -> (v -> string) * (string -> v) =
 fun t ->
  let to_string v =
    let d = to_lavoisier t in
    Lavoisier.emit_string v d in
  let of_string str =
    let p = to_angstrom t in
    match Angstrom.parse_string ~consume:All p str with
    | Ok v -> v
    | Error err -> invalid_arg err in
  (to_string, of_string)

let make_test :
    type v.
    string ->
    v t ->
    v Alcotest.testable ->
    v ->
    string ->
    unit Alcotest.test_case list =
 fun name t value v str ->
  let to_string, of_string = make t in
  [
    Alcotest.test_case name `Quick (fun () ->
        Alcotest.(check value) "decode" (of_string str) v);
    Alcotest.test_case name `Quick (fun () ->
        Alcotest.(check string) "encode" (to_string v) str);
  ]

let make_reject :
    type v. string -> v t -> v -> string -> unit Alcotest.test_case list =
 fun name t v str ->
  let to_string, of_string = make t in
  [
    Alcotest.test_case name `Quick (fun () ->
        try
          ignore @@ of_string str ;
          Alcotest.failf "test %s works" name
        with _ -> ());
    Alcotest.test_case name `Quick (fun () ->
        try
          ignore @@ to_string v ;
          Alcotest.failf "test %s works" name
        with _ -> ());
  ]

let combinator =
  let test0 =
    [
      make_test "( *> )"
        Syntax.(
          let a = Bij.char 'a' <$> any in
          let b = Bij.char 'b' <$> any in
          a *> b)
        Alcotest.unit () "ab";
      make_test "( <* )"
        Syntax.(
          let a = Bij.char 'a' <$> any in
          let b = Bij.char 'b' <$> any in
          a <* b)
        Alcotest.unit () "ab";
    ] in
  let test1 =
    let c =
      let open Syntax in
      let a = Bij.element (Char.equal 'a') <$> any in
      let b = Bij.element (Char.equal 'b') <$> any in
      let c = Bij.element (Char.equal 'c') <$> any in
      choice [ a; b; c ] in
    [
      make_test "choice" c Alcotest.char 'a' "a";
      make_test "choice" c Alcotest.char 'b' "b";
      make_test "choice" c Alcotest.char 'c' "c";
      make_reject "choice" c 'd' "d";
    ] in
  let test2 =
    let c = Syntax.(option any) in
    let fail = Syntax.(option (fail "fail")) in
    [
      make_test "option" c Alcotest.(option char) (Some 'a') "a";
      make_reject "option (reject)" fail (Some ()) "";
      make_reject "option (reject)" fail None "";
      make_reject "option (reject)" fail (Some ()) "42";
      make_reject "option (reject)" fail None "42";
    ] in
  let test3 =
    let c = Syntax.(count 3 any) in
    [ make_test "count" c Alcotest.(list char) [ 'b'; 'a'; 'r' ] "bar" ] in
  let test4 =
    let c = Syntax.(rep0 any) in
    [
      make_test "rep0" c Alcotest.(list char) [ 'b'; 'a'; 'r' ] "bar";
      make_test "rep0" c Alcotest.(list char) [] "";
    ] in
  let test5 =
    let c = Syntax.(rep1 any) in
    [
      make_test "rep1" c Alcotest.(list char) [ 'b'; 'a'; 'r' ] "bar";
      make_reject "rep1 (reject)" c [] "";
    ] in
  let test6 =
    let c =
      let open Syntax in
      let comma = Bij.char ',' <$> any in
      sep_by0 ~sep:comma any in
    [
      make_test "sep_by0" c Alcotest.(list char) [ 'b'; 'a'; 'r' ] "b,a,r";
      make_test "sep_by0" c Alcotest.(list char) [] "";
    ] in
  let test7 =
    let c =
      let open Syntax in
      let comma = Bij.char ',' <$> any in
      sep_by1 ~sep:comma any in
    [
      make_test "sep_by1" c Alcotest.(list char) [ 'b'; 'a'; 'r' ] "b,a,r";
      make_reject "sep_by1 (reject)" c [] "";
    ] in
  let test8 =
    let c = Syntax.lower in
    [
      make_test "lower:a" c Alcotest.char 'a' "a";
      make_test "lower:z" c Alcotest.char 'z' "z";
      make_reject "lower:A" c 'A' "A";
      make_reject "lower:Z" c 'Z' "Z";
      make_reject "lower:0" c '0' "0";
      make_reject "lower:9" c '9' "9";
    ] in
  let test9 =
    let c = Syntax.upper in
    [
      make_test "upper:A" c Alcotest.char 'A' "A";
      make_test "upper:Z" c Alcotest.char 'Z' "Z";
      make_reject "upper:a" c 'a' "a";
      make_reject "upper:z" c 'z' "z";
      make_reject "upper:0" c '0' "0";
      make_reject "upper:9" c '9' "9";
    ] in
  let test10 =
    let c = Syntax.alpha in
    [
      make_test "alpha:A" c Alcotest.char 'A' "A";
      make_test "alpha:a" c Alcotest.char 'a' "a";
      make_test "alpha:Z" c Alcotest.char 'Z' "Z";
      make_test "alpha:z" c Alcotest.char 'z' "z";
      make_reject "alpha:0" c '0' "0";
      make_reject "alpha:9" c '9' "9";
    ] in
  let test11 =
    let c = Syntax.digit in
    [
      make_reject "digit:A" c 'A' "A";
      make_reject "digit:a" c 'a' "a";
      make_reject "digit:Z" c 'Z' "Z";
      make_reject "digit:z" c 'z' "z";
      make_test "digit:0" c Alcotest.char '0' "0";
      make_test "digit:9" c Alcotest.char '9' "9";
    ] in
  let test12 =
    [
      make_test "sequence"
        Syntax.(sequence [ any; any; any ])
        Alcotest.(list char)
        [ 'b'; 'a'; 'r' ] "bar";
      make_test "fooz | bar"
        Syntax.(
          Bij.string "fooz"
          <$> const "fooz"
          <*> fail "fail"
          <|> (Bij.string "bar" <$> const "bar" <*> commit))
        Alcotest.(pair unit unit)
        ((), ()) "bar";
      make_test "bar | fooz"
        Syntax.(
          Bij.string "bar"
          <$> const "bar"
          <*> commit
          <|> (Bij.string "fooz" <$> const "fooz" <*> fail "fail"))
        Alcotest.(pair unit unit)
        ((), ()) "bar";
    ] in
  List.concat
    [
      test0;
      test1;
      test2;
      test3;
      test4;
      test5;
      test6;
      test7;
      test8;
      test9;
      test10;
      test11;
      test12;
    ]
  |> List.concat

let () =
  Alcotest.run "isomorpism"
    [ ("combinator", combinator); ("parser", parser); ("printer", printer) ]

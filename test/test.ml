let () = Printexc.record_backtrace true

open Encore

(*
let iso =
  let open Bij in
  let test_fwd ~global value t expect input =
    Alcotest.test_case (Fmt.strf "%s (fwd)" global) `Quick (fun () ->
        Alcotest.(check value)
          (Fmt.strf "%a" (Alcotest.pp value) expect)
          expect (fwd t input)) in
  let test_reject_fwd ~global ~name ~exn t input =
    Alcotest.test_case (Fmt.strf "%s (fwd)" global) `Quick (fun () ->
        Alcotest.check_raises name exn (fun () -> ignore @@ fwd t input)) in
  let test_bwd ~global value t input expect =
    Alcotest.test_case (Fmt.strf "%s (bwd)" global) `Quick (fun () ->
        Alcotest.(check value)
          (Fmt.strf "%a" (Alcotest.pp value) expect)
          expect (fwd t input)) in
  let test_reject_bwd ~global ~name ~exn t input =
    Alcotest.test_case (Fmt.strf "%s (fwd)" global) `Quick (fun () ->
        Alcotest.check_raises name exn (fun () -> ignore @@ bwd t input)) in
  [
    test_fwd ~global:"string" Alcotest.string string "foo" [ 'f'; 'o'; 'o' ];
    test_fwd ~global:"string" Alcotest.string string "" [];
    test_bwd ~global:"string" Alcotest.string string [ 'f'; 'o'; 'o' ] "foo";
    test_bwd ~global:"string" Alcotest.string string [] "";
    test_fwd ~global:"int" Alcotest.int int 42 "42";
    test_bwd ~global:"int" Alcotest.int int "42" 42;
    test_fwd ~global:"int" Alcotest.int int (-42) "-42";
    test_bwd ~global:"int" Alcotest.int int "-42" (-42);
    test_reject_fwd ~global:"int" ~name:"empty string"
      ~exn:Bij.Bij int "";
    test_reject_fwd ~global:"int" ~name:"invalid" ~exn:Bij.Bij
      Exn.int "invalid";
    test_fwd ~global:"bool" Alcotest.bool Exn.bool true "true";
    test_bwd ~global:"bool" Alcotest.bool Exn.bool "true" true;
    test_fwd ~global:"bool" Alcotest.bool Exn.bool false "false";
    test_bwd ~global:"bool" Alcotest.bool Exn.bool "false" false;
    test_reject_fwd ~global:"bool" ~name:"empty string"
      ~exn:Bij.Exn.Bij Exn.bool "";
    test_reject_fwd ~global:"bool" ~name:"invalid" ~exn:Bij.Exn.Bij
      Exn.bool "invalid";
    test_fwd ~global:"identity" Alcotest.int Exn.identity 42 42;
    test_bwd ~global:"identity" Alcotest.int Exn.identity 42 42;
    test_fwd ~global:"commute" Alcotest.(pair int int) Exn.commute (1, 2) (2, 1);
    test_bwd ~global:"commute" Alcotest.(pair int int) Exn.commute (1, 2) (2, 1);
    test_fwd ~global:"compose"
      Alcotest.(pair int int)
      Exn.(compose commute commute)
      (1, 2) (1, 2);
    test_fwd ~global:"inverse" Alcotest.int Exn.int 42 "42";
    test_bwd ~global:"inverse" Alcotest.string (flip Exn.int) 42 "42";
    test_fwd ~global:"inverse" Alcotest.string (flip Exn.int) "42" 42;
    test_bwd ~global:"inverse" Alcotest.int Exn.int "42" 42;
    test_fwd ~global:"product"
      Alcotest.(pair int bool)
      (product Exn.int Exn.bool) (42, true) ("42", "true");
    test_reject_fwd ~global:"product" ~name:"empty string"
      ~exn:Bij.Exn.Bij (product Exn.int Exn.bool) ("invalid", "true");
    test_reject_fwd ~global:"product" ~name:"empty string"
      ~exn:Bij.Exn.Bij (product Exn.int Exn.bool) ("true", "invalid");
    test_fwd ~global:"subset" Alcotest.char (Exn.subset (( = ) 'a')) 'a' 'a';
    test_bwd ~global:"subset" Alcotest.char (Exn.subset (( = ) 'a')) 'a' 'a';
    test_reject_fwd ~global:"subset" ~name:"a" ~exn:Bij.Exn.Bij
      (Exn.subset (( = ) 'a'))
      'b';
    test_reject_bwd ~global:"subset" ~name:"a" ~exn:Bij.Exn.Bij
      (Exn.subset (( = ) 'a'))
      'b';
  ]
*)

let parser =
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
    Alcotest.(check value) "decode" (of_string str) v in
  let make_reject : type v. string -> v t -> string -> unit Alcotest.test_case =
   fun name t str ->
    let of_string str =
      let p = to_angstrom t in
      match Angstrom.parse_string ~consume:Prefix p str with
      | Ok v -> v
      | Error err -> invalid_arg err in
    Alcotest.test_case name `Quick @@ fun () ->
    try
      let _ = of_string str in
      Alcotest.failf "test %s works" name
    with _ -> () in
  List.concat
    [
      [
        make_test "any" Syntax.any Alcotest.char 'f' "foo";
        make_test "any" Syntax.any Alcotest.char 'b' "bar";
        make_reject "any" Syntax.any "";
      ];
      (let c =
         let a = Bij.element (Char.equal 'a') in
         let b = Bij.element (Char.equal 'b') in
         Syntax.(a <$> any <*> (b <$> any)) in
       [
         make_test "(<*>)" c Alcotest.(pair char char) ('a', 'b') "ab";
         make_reject "(<*>)" c "ac";
         make_reject "(<*>)" c "cb";
         make_reject "(<*>)" c "a";
         make_reject "(<*>)" c "b";
       ]);
      (let c = Syntax.fail "fail" in
       [
         make_reject "fail" c "foo";
         make_reject "fail" c "bar";
         make_reject "fail" c "";
       ]);
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
      ];
      (let a_or_b =
         Syntax.(
           Bij.element (Char.equal 'a')
           <$> any
           <|> (Bij.element (Char.equal 'b') <$> any)) in
       let b_or_a =
         Syntax.(
           Bij.element (Char.equal 'b')
           <$> any
           <|> (Bij.element (Char.equal 'a') <$> any)) in
       [
         make_test "(<|>)" a_or_b Alcotest.char 'a' "a";
         make_test "(<|>)" b_or_a Alcotest.char 'a' "a";
         make_reject "(<|>)" a_or_b "c";
         make_reject "(<|>)" b_or_a "c";
       ]);
    ]

let printer =
  let make_test :
      type v. string -> v t -> v -> string -> unit Alcotest.test_case =
   fun name t v str ->
    let to_string v =
      let d = to_lavoisier t in
      Lavoisier.emit_string v d in
    Alcotest.test_case name `Quick @@ fun () ->
    Alcotest.(check string) "encode" (to_string v) str in
  let make_reject : type v. string -> v t -> v -> unit Alcotest.test_case =
   fun name t v ->
    let to_string v =
      let d = to_lavoisier t in
      Lavoisier.emit_string v d in
    Alcotest.test_case name `Quick @@ fun () ->
    try
      let _ = to_string v in
      Alcotest.failf "test %s works" name
    with _ -> () in
  List.concat
    [
      [ make_test "any" Syntax.any 'f' "f" ];
      (let c =
         let a = Bij.element (Char.equal 'a') in
         let b = Bij.element (Char.equal 'b') in
         Syntax.(a <$> any <*> (b <$> any)) in
       [
         make_test "(<*>)" c ('a', 'b') "ab";
         make_reject "(<*>)" c ('a', 'c');
         make_reject "(<*>)" c ('c', 'b');
         make_reject "(<*>)" c ('c', 'c');
       ]);
      [ make_reject "fail" (Syntax.fail "fail") () ];
      [
        make_test "pure" Syntax.(pure ~compare:(fun () () -> true) ()) () "";
        make_test "pure" Syntax.(pure ~compare:( = ) 42) 42 "";
        make_reject "pure" Syntax.(pure ~compare:( = ) 42) 24;
      ];
      (let a_or_b =
         Syntax.(
           Bij.element (Char.equal 'a')
           <$> any
           <|> (Bij.element (Char.equal 'b') <$> any)) in
       let b_or_a =
         Syntax.(
           Bij.element (Char.equal 'b')
           <$> any
           <|> (Bij.element (Char.equal 'a') <$> any)) in
       [
         make_test "(<|>)" a_or_b 'a' "a";
         make_test "(<|>)" a_or_b 'b' "b";
         make_test "(<|>)" b_or_a 'a' "a";
         make_test "(<|>)" b_or_a 'b' "b";
         make_reject "(<|>)" a_or_b 'c';
         make_reject "(<|>)" b_or_a 'c';
       ]);
    ]

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

let combinator =
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
    ] in
  let make_reject :
      type v. string -> v t -> v -> string -> unit Alcotest.test_case list =
   fun name t v str ->
    let to_string, of_string = make t in
    [
      Alcotest.test_case name `Quick (fun () ->
          try
            let _ = of_string str in
            Alcotest.failf "test %s works" name
          with _ -> ());
      Alcotest.test_case name `Quick (fun () ->
          try
            let _ = to_string v in
            Alcotest.failf "test %s works" name
          with _ -> ());
    ] in
  List.concat
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
      (let c =
         let open Syntax in
         let a = Bij.element (Char.equal 'a') <$> any in
         let b = Bij.element (Char.equal 'b') <$> any in
         let c = Bij.element (Char.equal 'c') <$> any in
         choice [ a; b; c ] in
       List.concat
         [
           make_test "choice" c Alcotest.char 'a' "a";
           make_test "choice" c Alcotest.char 'b' "b";
           make_test "choice" c Alcotest.char 'c' "c";
           make_reject "choice" c 'd' "d";
         ]);
      (let c = Syntax.(option any) in
       let fail = Syntax.(option (fail "fail")) in
       List.concat
         [
           make_test "option" c Alcotest.(option char) (Some 'a') "a";
           make_reject "option (reject)" fail (Some ()) "";
           make_reject "option (reject)" fail None "";
           make_reject "option (reject)" fail (Some ()) "42";
           make_reject "option (reject)" fail None "42";
         ]);
      (let c = Syntax.(count 3 any) in
       List.concat
         [ make_test "count" c Alcotest.(list char) [ 'b'; 'a'; 'r' ] "bar" ]);
      (let c = Syntax.(rep0 any) in
       List.concat
         [
           make_test "rep0" c Alcotest.(list char) [ 'b'; 'a'; 'r' ] "bar";
           make_test "rep0" c Alcotest.(list char) [] "";
         ]);
      (let c = Syntax.(rep1 any) in
       List.concat
         [
           make_test "rep1" c Alcotest.(list char) [ 'b'; 'a'; 'r' ] "bar";
           make_reject "rep1 (reject)" c [] "";
         ]);
      (let c =
         let open Syntax in
         let comma = Bij.char ',' <$> any in
         sep_by0 ~sep:comma any in
       List.concat
         [
           make_test "sep_by0" c Alcotest.(list char) [ 'b'; 'a'; 'r' ] "b,a,r";
           make_test "sep_by0" c Alcotest.(list char) [] "";
         ]);
      (let c =
         let open Syntax in
         let comma = Bij.char ',' <$> any in
         sep_by1 ~sep:comma any in
       List.concat
         [
           make_test "sep_by1" c Alcotest.(list char) [ 'b'; 'a'; 'r' ] "b,a,r";
           make_reject "sep_by1 (reject)" c [] "";
         ]);
      (*
      (let combinator =
         (module struct
           type sentinel = char list

           module Make (S : Meta.S) = struct
             include Meta.Make (S)

             let comma = Exn.element ~compare:Char.equal ',' <$> any

             let p : sentinel S.t = end_by0 ~sep:comma any
           end
         end : COMBINATOR
           with type sentinel = char list) in
       List.concat
         [
           make_test "end_by0" combinator
             Alcotest.(list char)
             [ 'b'; 'a'; 'r' ] "b,a,r,";
           make_test "end_by0" combinator Alcotest.(list char) [] "";
           make_test "end_by0" combinator Alcotest.(list char) [ 'b' ] "b,";
         ]);
      (let combinator =
         (module struct
           type sentinel = char list

           module Make (S : Meta.S) = struct
             include Meta.Make (S)

             let comma = Exn.element ~compare:Char.equal ',' <$> any

             let p : sentinel S.t = end_by1 ~sep:comma any
           end
         end : COMBINATOR
           with type sentinel = char list) in
       List.concat
         [
           make_test "end_by1" combinator
             Alcotest.(list char)
             [ 'b'; 'a'; 'r' ] "b,a,r,";
           make_reject "end_by1" combinator [] "";
         ]);
*)
      (let c = Syntax.lower in
       List.concat
         [
           make_test "lower:a" c Alcotest.char 'a' "a";
           make_test "lower:z" c Alcotest.char 'z' "z";
           make_reject "lower:A" c 'A' "A";
           make_reject "lower:Z" c 'Z' "Z";
           make_reject "lower:0" c '0' "0";
           make_reject "lower:9" c '9' "9";
         ]);
      (let c = Syntax.upper in
       List.concat
         [
           make_test "upper:A" c Alcotest.char 'A' "A";
           make_test "upper:Z" c Alcotest.char 'Z' "Z";
           make_reject "upper:a" c 'a' "a";
           make_reject "upper:z" c 'z' "z";
           make_reject "upper:0" c '0' "0";
           make_reject "upper:9" c '9' "9";
         ]);
      (let c = Syntax.alpha in
       List.concat
         [
           make_test "alpha:A" c Alcotest.char 'A' "A";
           make_test "alpha:a" c Alcotest.char 'a' "a";
           make_test "alpha:Z" c Alcotest.char 'Z' "Z";
           make_test "alpha:z" c Alcotest.char 'z' "z";
           make_reject "alpha:0" c '0' "0";
           make_reject "alpha:9" c '9' "9";
         ]);
      (let c = Syntax.digit in
       List.concat
         [
           make_reject "digit:A" c 'A' "A";
           make_reject "digit:a" c 'a' "a";
           make_reject "digit:Z" c 'Z' "Z";
           make_reject "digit:z" c 'z' "z";
           make_test "digit:0" c Alcotest.char '0' "0";
           make_test "digit:9" c Alcotest.char '9' "9";
         ]);
      make_test "sequence"
        Syntax.(sequence [ any; any; any ])
        Alcotest.(list char)
        [ 'b'; 'a'; 'r' ] "bar";
    ]

let () =
  Alcotest.run "isomorpism"
    [
      (* ("isomorphism", iso); *)
      ("combinator", combinator);
      ("parser", parser);
      ("printer", printer);
    ]

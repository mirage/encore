let () = Printexc.record_backtrace true

open Encore

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
  [ test_fwd ~global:"string"   Alcotest.string          Exn.string                    "foo"      ['f'; 'o'; 'o'; ]
  ; test_fwd ~global:"string"   Alcotest.string          Exn.string                    ""         []
  ; test_bwd ~global:"string"   Alcotest.string          Exn.string                    ['f'; 'o'; 'o'; ] "foo"
  ; test_bwd ~global:"string"   Alcotest.string          Exn.string                    []         ""
  ; test_fwd ~global:"int"      Alcotest.int             Exn.int                       42         "42"
  ; test_bwd ~global:"int"      Alcotest.int             Exn.int                       "42"       42
  ; test_fwd ~global:"int"      Alcotest.int             Exn.int                       (-42)      "-42"
  ; test_bwd ~global:"int"      Alcotest.int             Exn.int                       "-42"      (-42)
  ; test_fwd ~global:"bool"     Alcotest.bool            Exn.bool                      true       "true"
  ; test_bwd ~global:"bool"     Alcotest.bool            Exn.bool                      "true"     true
  ; test_fwd ~global:"bool"     Alcotest.bool            Exn.bool                      false      "false"
  ; test_bwd ~global:"bool"     Alcotest.bool            Exn.bool                      "false"    false
  ; test_fwd ~global:"identity" Alcotest.int             Exn.identity                  42         42
  ; test_bwd ~global:"identity" Alcotest.int             Exn.identity                  42         42
  ; test_fwd ~global:"commute"  Alcotest.(pair int int)  Exn.commute                   (1, 2)     (2, 1)
  ; test_bwd ~global:"commute"  Alcotest.(pair int int)  Exn.commute                   (1, 2)     (2, 1)
  ; test_fwd ~global:"compose"  Alcotest.(pair int int)  Exn.(compose commute commute) (1, 2)     (1, 2)
  ; test_fwd ~global:"inverse"  Alcotest.int             Exn.int                       42         "42"
  ; test_bwd ~global:"inverse"  Alcotest.string          (flip Exn.int)                42         "42"
  ; test_fwd ~global:"inverse"  Alcotest.string          (flip Exn.int)                "42"       42
  ; test_bwd ~global:"inverse"  Alcotest.int             Exn.int                       "42"       42
  ; test_fwd ~global:"product"  Alcotest.(pair int bool) (product Exn.int Exn.bool)    (42, true) ("42", "true")
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

                 let a = Exn.element ~tag:"char" ~compare:Char.equal 'a' <$> any
                 let b = Exn.element ~tag:"char" ~compare:Char.equal 'b' <$> any
                 let p : sentinel S.t = a *> b end end))
        Alcotest.unit () "ab"
    ; make_test "( <* )"
        (module
           (struct
             type sentinel = unit

             module Make =
               functor (S: Meta.S) -> struct
                 include Meta.Make(S)

                 let a = Exn.element ~tag:"char" ~compare:Char.equal 'a' <$> any
                 let b = Exn.element ~tag:"char" ~compare:Char.equal 'b' <$> any
                 let p : sentinel S.t = a <* b end end))
        Alcotest.unit () "ab"
    ; (let combinator =
         (module
            (struct
              type sentinel = char

              module Make =
                functor (S: Meta.S) -> struct
                  include Meta.Make(S)

                  let a = Exn.subset ((=) 'a') <$> any
                  let b = Exn.subset ((=) 'b') <$> any
                  let c = Exn.subset ((=) 'c') <$> any
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

                 let p : sentinel S.t = option any end end))
        Alcotest.(option char) (Some 'a') "a"
    ; (let combinator =
         (module
            (struct
              type sentinel = char list

              module Make =
                functor (S: Meta.S) -> struct
                  include Meta.Make(S)

                  let p : sentinel S.t = count 3 any end end) : COMBINATOR with type sentinel = char list) in
       List.concat
         [ make_test "count" combinator Alcotest.(list char) [ 'b'; 'a'; 'r'; ] "bar" ])
    ; (let combinator =
         (module
            (struct
              type sentinel = char list

              module Make =
                functor (S: Meta.S) -> struct
                  include Meta.Make(S)

                  let p : sentinel S.t = rep0 any end end) : COMBINATOR with type sentinel = char list) in
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

                 let p : sentinel S.t = rep1 any end end))
        Alcotest.(list char) [ 'b'; 'a'; 'r'; ] "bar"
    ; (let combinator =
         (module
            (struct
              type sentinel = char list

              module Make =
                functor (S: Meta.S) -> struct
                  include Meta.Make(S)

                  let comma = Exn.element ~tag:"char" ~compare:Char.equal ',' <$> any
                  let p : sentinel S.t = sep_by0 ~sep:comma any end end) : COMBINATOR with type sentinel = char list) in
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

                  let comma = Exn.element ~tag:"char" ~compare:Char.equal ',' <$> any
                 let p : sentinel S.t = sep_by1 ~sep:comma any end end))
        Alcotest.(list char) [ 'b'; 'a'; 'r'; ] "b,a,r"
    ; (let combinator =
         (module
            (struct
              type sentinel = char list

              module Make =
                functor (S: Meta.S) -> struct
                  include Meta.Make(S)

                  let comma = Exn.element ~tag:"char" ~compare:Char.equal ',' <$> any
                  let p : sentinel S.t = end_by0 ~sep:comma any end end) : COMBINATOR with type sentinel = char list) in
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

                  let comma = Exn.element ~tag:"char" ~compare:Char.equal ',' <$> any
                 let p : sentinel S.t = end_by1 ~sep:comma any end end))
        Alcotest.(list char) [ 'b'; 'a'; 'r'; ] "b,a,r,"
    ; make_test "sequence"
        (module
           (struct
             type sentinel = char list

             module Make =
               functor (S: Meta.S) -> struct
                 include Meta.Make(S)

                 let p : sentinel S.t = sequence [ any; any; any; ] end end))
        Alcotest.(list char) ['b'; 'a'; 'r'; ] "bar" ]

let () =
  Alcotest.run "isomorpism"
    [ "isomorphism", iso
    ; "combinator", combinator ];

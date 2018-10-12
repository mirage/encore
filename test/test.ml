let () = Printexc.record_backtrace true

open Encore

let iso =
  let open Bijection in
  let test_fwd ~global value t expect input =
    Alcotest.test_case (Fmt.strf "%s (fwd)" global) `Quick (fun () ->
        Alcotest.(check value)
          (Fmt.strf "%a" (Alcotest.pp value) expect)
          expect (fwd t input) )
  in
  let test_reject_fwd ~global ~name ~exn t input =
    Alcotest.test_case (Fmt.strf "%s (fwd)" global) `Quick (fun () ->
        Alcotest.check_raises name exn (fun () -> ignore @@ fwd t input) )
  in
  let test_bwd ~global value t input expect =
    Alcotest.test_case (Fmt.strf "%s (bwd)" global) `Quick (fun () ->
        Alcotest.(check value)
          (Fmt.strf "%a" (Alcotest.pp value) expect)
          expect (fwd t input) )
  in
  let test_reject_bwd ~global ~name ~exn t input =
    Alcotest.test_case (Fmt.strf "%s (fwd)" global) `Quick (fun () ->
        Alcotest.check_raises name exn (fun () -> ignore @@ bwd t input) )
  in
  [ test_fwd ~global:"string" Alcotest.string Exn.string "foo" ['f'; 'o'; 'o']
  ; test_fwd ~global:"string" Alcotest.string Exn.string "" []
  ; test_bwd ~global:"string" Alcotest.string Exn.string ['f'; 'o'; 'o'] "foo"
  ; test_bwd ~global:"string" Alcotest.string Exn.string [] ""
  ; test_fwd ~global:"int" Alcotest.int Exn.int 42 "42"
  ; test_bwd ~global:"int" Alcotest.int Exn.int "42" 42
  ; test_fwd ~global:"int" Alcotest.int Exn.int (-42) "-42"
  ; test_bwd ~global:"int" Alcotest.int Exn.int "-42" (-42)
  ; test_reject_fwd ~global:"int" ~name:"empty string"
      ~exn:(Bijection.Exn.Bijection ("string", "int"))
      Exn.int ""
  ; test_reject_fwd ~global:"int" ~name:"invalid"
      ~exn:(Bijection.Exn.Bijection ("string", "int"))
      Exn.int "invalid"
  ; test_fwd ~global:"bool" Alcotest.bool Exn.bool true "true"
  ; test_bwd ~global:"bool" Alcotest.bool Exn.bool "true" true
  ; test_fwd ~global:"bool" Alcotest.bool Exn.bool false "false"
  ; test_bwd ~global:"bool" Alcotest.bool Exn.bool "false" false
  ; test_reject_fwd ~global:"bool" ~name:"empty string"
      ~exn:(Bijection.Exn.Bijection ("string", "bool"))
      Exn.bool ""
  ; test_reject_fwd ~global:"bool" ~name:"invalid"
      ~exn:(Bijection.Exn.Bijection ("string", "bool"))
      Exn.bool "invalid"
  ; test_fwd ~global:"identity" Alcotest.int Exn.identity 42 42
  ; test_bwd ~global:"identity" Alcotest.int Exn.identity 42 42
  ; test_fwd ~global:"commute" Alcotest.(pair int int) Exn.commute (1, 2) (2, 1)
  ; test_bwd ~global:"commute" Alcotest.(pair int int) Exn.commute (1, 2) (2, 1)
  ; test_fwd ~global:"compose"
      Alcotest.(pair int int)
      Exn.(compose commute commute)
      (1, 2) (1, 2)
  ; test_fwd ~global:"inverse" Alcotest.int Exn.int 42 "42"
  ; test_bwd ~global:"inverse" Alcotest.string (flip Exn.int) 42 "42"
  ; test_fwd ~global:"inverse" Alcotest.string (flip Exn.int) "42" 42
  ; test_bwd ~global:"inverse" Alcotest.int Exn.int "42" 42
  ; test_fwd ~global:"product"
      Alcotest.(pair int bool)
      (product Exn.int Exn.bool) (42, true) ("42", "true")
  ; test_reject_fwd ~global:"product" ~name:"empty string"
      ~exn:(Bijection.Exn.Bijection ("string", "int"))
      (product Exn.int Exn.bool) ("invalid", "true")
  ; test_reject_fwd ~global:"product" ~name:"empty string"
      ~exn:(Bijection.Exn.Bijection ("string", "bool"))
      (product Exn.int Exn.bool) ("true", "invalid")
  ; test_fwd ~global:"subset" Alcotest.char (Exn.subset (( = ) 'a')) 'a' 'a'
  ; test_bwd ~global:"subset" Alcotest.char (Exn.subset (( = ) 'a')) 'a' 'a'
  ; test_reject_fwd ~global:"subset" ~name:"a"
      ~exn:(Bijection.Exn.Bijection ("a with predicate", "x"))
      (Exn.subset (( = ) 'a'))
      'b'
  ; test_reject_bwd ~global:"subset" ~name:"a"
      ~exn:(Bijection.Exn.Bijection ("a with predicate", "x"))
      (Exn.subset (( = ) 'a'))
      'b' ]

module type COMBINATOR = sig
  type sentinel

  module Make (M : Meta.S) : sig
    val p : sentinel M.t
  end
end

let make : type sentinel.
       (module COMBINATOR with type sentinel = sentinel)
    -> (sentinel -> string) * (string -> sentinel) =
 fun (module Combinator) ->
  let to_string v =
    let module Enc = Combinator.Make (Proxy_encoder.Impl) in
    Encoder.to_string Enc.p v
  in
  let of_string s =
    let module Dec = Combinator.Make (Proxy_decoder.Impl) in
    match Angstrom.parse_string Dec.p s with
    | Ok v -> v
    | Error err -> invalid_arg err
  in
  (to_string, of_string)

let combinator =
  let make_test : type sentinel.
         string
      -> (module COMBINATOR with type sentinel = sentinel)
      -> sentinel Alcotest.testable
      -> sentinel
      -> string
      -> unit Alcotest.test_case list =
   fun name (module Combinator) value sentinel s ->
    let to_string, of_string = make (module Combinator) in
    [ Alcotest.test_case name `Quick (fun () ->
          Alcotest.(check value) "decode" (of_string s) sentinel )
    ; Alcotest.test_case name `Quick (fun () ->
          Alcotest.(check string) "encode" (to_string sentinel) s ) ]
  in
  let make_reject : type sentinel.
         string
      -> (module COMBINATOR with type sentinel = sentinel)
      -> sentinel
      -> string
      -> unit Alcotest.test_case list =
   fun name (module Combinator) sentinel s ->
    let to_string, of_string = make (module Combinator) in
    [ Alcotest.test_case name `Quick (fun () ->
          try
            let _ = of_string s in
            Alcotest.failf "test %s works" name
          with _ -> () )
    ; Alcotest.test_case name `Quick (fun () ->
          try
            let _ = to_string sentinel in
            Alcotest.failf "test %s works" name
          with _ -> () ) ]
  in
  let open Bijection in
  List.concat
    [ make_test "( *> )"
        ( module struct
          type sentinel = unit

          module Make (S : Meta.S) = struct
            include Meta.Make (S)

            let a = Exn.element ~tag:"char" ~compare:Char.equal 'a' <$> any

            let b = Exn.element ~tag:"char" ~compare:Char.equal 'b' <$> any

            let p : sentinel S.t = a *> b
          end
        end )
        Alcotest.unit () "ab"
    ; make_test "( <* )"
        ( module struct
          type sentinel = unit

          module Make (S : Meta.S) = struct
            include Meta.Make (S)

            let a = Exn.element ~tag:"char" ~compare:Char.equal 'a' <$> any

            let b = Exn.element ~tag:"char" ~compare:Char.equal 'b' <$> any

            let p : sentinel S.t = a <* b
          end
        end )
        Alcotest.unit () "ab"
    ; (let combinator =
         ( module struct
           type sentinel = char

           module Make (S : Meta.S) = struct
             include Meta.Make (S)

             let a = Exn.subset (( = ) 'a') <$> any

             let b = Exn.subset (( = ) 'b') <$> any

             let c = Exn.subset (( = ) 'c') <$> any

             let p : sentinel S.t = choice [a; b; c]
           end
         end
         : COMBINATOR
           with type sentinel = char )
       in
       List.concat
         [ make_test "choice" combinator Alcotest.char 'a' "a"
         ; make_test "choice" combinator Alcotest.char 'b' "b"
         ; make_test "choice" combinator Alcotest.char 'c' "c"
         ; make_reject "choice" combinator 'd' "d" ])
    ; (let combinator =
         ( module struct
           type sentinel = char option

           module Make (S : Meta.S) = struct
             include Meta.Make (S)

             let p : sentinel S.t = option any
           end
         end
         : COMBINATOR
           with type sentinel = char option )
       in
       let fail =
         ( module struct
           type sentinel = unit option

           module Make (S : Meta.S) = struct
             include Meta.Make (S)

             let p : sentinel S.t = option (fail "error")
           end
         end
         : COMBINATOR
           with type sentinel = unit option )
       in
       List.concat
         [ make_test "option" combinator Alcotest.(option char) (Some 'a') "a"
         ; make_reject "option (reject)" fail (Some ()) ""
         ; make_reject "option (reject)" fail None ""
         ; make_reject "option (reject)" fail (Some ()) "42"
         ; make_reject "option (reject)" fail None "42" ])
    ; (let combinator =
         ( module struct
           type sentinel = char list

           module Make (S : Meta.S) = struct
             include Meta.Make (S)

             let p : sentinel S.t = count 3 any
           end
         end
         : COMBINATOR
           with type sentinel = char list )
       in
       List.concat
         [ make_test "count" combinator
             Alcotest.(list char)
             ['b'; 'a'; 'r'] "bar" ])
    ; (let combinator =
         ( module struct
           type sentinel = char list

           module Make (S : Meta.S) = struct
             include Meta.Make (S)

             let p : sentinel S.t = rep0 any
           end
         end
         : COMBINATOR
           with type sentinel = char list )
       in
       List.concat
         [ make_test "rep0" combinator
             Alcotest.(list char)
             ['b'; 'a'; 'r'] "bar"
         ; make_test "rep0" combinator Alcotest.(list char) [] "" ])
    ; (let combinator =
         ( module struct
           type sentinel = char list

           module Make (S : Meta.S) = struct
             include Meta.Make (S)

             let p : sentinel S.t = rep1 any
           end
         end
         : COMBINATOR
           with type sentinel = char list )
       in
       List.concat
         [ make_test "rep1" combinator
             Alcotest.(list char)
             ['b'; 'a'; 'r'] "bar"
         ; make_reject "rep1 (reject)" combinator [] "" ])
    ; (let combinator =
         ( module struct
           type sentinel = char list

           module Make (S : Meta.S) = struct
             include Meta.Make (S)

             let comma =
               Exn.element ~tag:"char" ~compare:Char.equal ',' <$> any

             let p : sentinel S.t = sep_by0 ~sep:comma any
           end
         end
         : COMBINATOR
           with type sentinel = char list )
       in
       List.concat
         [ make_test "sep_by0" combinator
             Alcotest.(list char)
             ['b'; 'a'; 'r'] "b,a,r"
         ; make_test "sep_by0" combinator Alcotest.(list char) [] "" ])
    ; (let combinator =
         ( module struct
           type sentinel = char list

           module Make (S : Meta.S) = struct
             include Meta.Make (S)

             let comma =
               Exn.element ~tag:"char" ~compare:Char.equal ',' <$> any

             let p : sentinel S.t = sep_by1 ~sep:comma any
           end
         end
         : COMBINATOR
           with type sentinel = char list )
       in
       List.concat
         [ make_test "sep_by1" combinator
             Alcotest.(list char)
             ['b'; 'a'; 'r'] "b,a,r"
         ; make_reject "sep_by1 (reject)" combinator [] "" ])
    ; (let combinator =
         ( module struct
           type sentinel = char list

           module Make (S : Meta.S) = struct
             include Meta.Make (S)

             let comma =
               Exn.element ~tag:"char" ~compare:Char.equal ',' <$> any

             let p : sentinel S.t = end_by0 ~sep:comma any
           end
         end
         : COMBINATOR
           with type sentinel = char list )
       in
       List.concat
         [ make_test "end_by0" combinator
             Alcotest.(list char)
             ['b'; 'a'; 'r'] "b,a,r,"
         ; make_test "end_by0" combinator Alcotest.(list char) [] ""
         ; make_test "end_by0" combinator Alcotest.(list char) ['b'] "b," ])
    ; (let combinator =
         ( module struct
           type sentinel = char list

           module Make (S : Meta.S) = struct
             include Meta.Make (S)

             let comma =
               Exn.element ~tag:"char" ~compare:Char.equal ',' <$> any

             let p : sentinel S.t = end_by1 ~sep:comma any
           end
         end
         : COMBINATOR
           with type sentinel = char list )
       in
       List.concat
         [ make_test "end_by1" combinator
             Alcotest.(list char)
             ['b'; 'a'; 'r'] "b,a,r,"
         ; make_reject "end_by1" combinator [] "" ])
    ; make_test "sequence"
        ( module struct
          type sentinel = char list

          module Make (S : Meta.S) = struct
            include Meta.Make (S)

            let p : sentinel S.t = sequence [any; any; any]
          end
        end )
        Alcotest.(list char)
        ['b'; 'a'; 'r'] "bar" ]

let () =
  Alcotest.run "isomorpism" [("isomorphism", iso); ("combinator", combinator)]

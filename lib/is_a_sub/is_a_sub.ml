type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

external is_a_sub : ('a, 'b) bigarray -> int -> ('a, 'b) bigarray -> int -> bool = "caml_is_a_sub" [@@noalloc]

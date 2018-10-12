type +'a t

exception Empty

val empty : 'a t

val is_empty : 'a t -> bool

val push : 'a t -> 'a -> 'a t

val shift : 'a t -> 'a * 'a t

val cons : 'a t -> 'a -> 'a t

val map_last : ('a -> 'a) -> 'a t -> 'a t

type 'a sequence = ('a -> unit) -> unit

val to_seq : 'a t -> 'a sequence

val iter : ('a -> unit) -> 'a t -> unit

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val to_list : 'a t -> 'a list

val of_list : 'a list -> 'a t

val pp : 'a Fmt.t -> 'a t Fmt.t

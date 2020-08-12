type t

exception Empty

val is_empty : t -> bool

val weight : t -> int

val create : unit -> t

val push : t -> string -> unit

val pop : t -> string

val rem : t -> string

(* (c) 2013 Simon Cruanes
 * (c) 2020 Romain Calascibetta *)

type cell =
  | One of string
  | Two of string * string
  | Three of string * string * string

type inner_node = {
  mutable cell : cell;
  mutable next : inner_node;
  mutable prev : inner_node;
}

type node = Empty | Node of inner_node

type t = { mutable cur : node; mutable size : int; mutable weight : int }

let weight { weight; _ } = weight

let create () = { cur = Empty; size = 0; weight = 0 }

let _incr_size d = d.size <- d.size + 1

let _decr_size d = d.size <- d.size - 1

let _incr_weight d str = d.weight <- d.weight + String.length str

let _decr_weight d str = d.weight <- d.weight - String.length str

let is_empty d = d.size = 0

let push d str =
  _incr_size d ;
  _incr_weight d str ;
  match d.cur with
  | Empty ->
      let rec node = { cell = One str; prev = node; next = node } in
      d.cur <- Node node
  | Node cur -> (
      let last = cur.prev in
      match last.cell with
      | One a -> last.cell <- Two (a, str)
      | Two (a, b) -> last.cell <- Three (a, b, str)
      | Three _ ->
          let elt = { cell = One str; next = cur; prev = last } in
          last.next <- elt ;
          cur.prev <- elt)

exception Empty

let _take_back_node n =
  match n.cell with
  | One x -> (true, x)
  | Two (x, y) ->
      n.cell <- One x ;
      (false, y)
  | Three (x, y, z) ->
      n.cell <- Two (x, y) ;
      (false, z)

let _remove_node n =
  let next = n.next in
  n.prev.next <- next ;
  next.prev <- n.prev

let rem d =
  match d.cur with
  | Empty -> raise Empty
  | Node cur ->
      if cur == cur.prev
      then (
        let is_zero, str = _take_back_node cur in
        _decr_size d ;
        _decr_weight d str ;
        if is_zero then d.cur <- Empty ;
        str)
      else
        let n = cur.prev in
        let is_zero, str = _take_back_node n in
        _decr_size d ;
        _decr_weight d str ;
        if is_zero then _remove_node n ;
        str

let _take_front_node n =
  match n.cell with
  | One x -> (true, x)
  | Two (x, y) ->
      n.cell <- One y ;
      (false, x)
  | Three (x, y, z) ->
      n.cell <- Two (y, z) ;
      (false, x)

let pop d =
  match d.cur with
  | Empty -> raise Empty
  | Node cur ->
      if cur.prev == cur
      then (
        let is_zero, str = _take_front_node cur in
        _decr_size d ;
        _decr_weight d str ;
        if is_zero then d.cur <- Empty ;
        str)
      else
        let is_zero, str = _take_front_node cur in
        _decr_size d ;
        _decr_weight d str ;
        if is_zero
        then (
          cur.prev.next <- cur.next ;
          cur.next.prev <- cur.prev ;
          d.cur <- Node cur.next) ;
        str

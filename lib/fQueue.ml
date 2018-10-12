type 'a digit = Zero | One of 'a | Two of 'a * 'a | Three of 'a * 'a * 'a

type 'a t =
  | Shallow of 'a digit
  | Deep of {s: int; f: 'a digit; m: ('a * 'a) t Lazy.t; r: 'a digit}

let empty = Shallow Zero

exception Empty

let _one x = Shallow (One x)

let _two x y = Shallow (Two (x, y))

let _deep s f m r =
  assert (f <> Zero && r <> Zero) ;
  Deep {s; f; m; r}

let is_empty = function
  | Shallow Zero -> true
  | Shallow (One _ | Two _ | Three _) | Deep _ -> false

let _empty = Lazy.from_val empty

let rec push : 'a. 'a t -> 'a -> 'a t =
 fun q x ->
  match q with
  | Shallow Zero -> _one x
  | Shallow (One y) -> Shallow (Two (y, x))
  | Shallow (Two (y, z)) -> Shallow (Three (y, z, x))
  | Shallow (Three (y, z, z')) -> _deep 4 (Two (y, z)) _empty (Two (z', x))
  | Deep {r= Zero; _} -> assert false
  | Deep {s; f; m; r= One y} -> _deep (s + 1) f m (Two (y, x))
  | Deep {s; f; m; r= Two (y, z)} -> _deep (s + 1) f m (Three (y, z, x))
  | Deep {s; f; m= (lazy q'); r= Three (y, z, z')} ->
      _deep (s + 1) f (lazy (push q' (y, z))) (Two (z', x))

let map_last_digit f = function
  | Zero -> Zero
  | One x -> One (f x)
  | Two (x, y) -> Two (x, f y)
  | Three (x, y, z) -> Three (x, y, f z)

let map_last : 'a. ('a -> 'a) -> 'a t -> 'a t =
 fun f -> function
  | Shallow v -> Shallow (map_last_digit f v)
  | Deep ({r; _} as deep) -> Deep {deep with r= map_last_digit f r}

let rec shift : 'a. 'a t -> 'a * 'a t =
 fun q ->
  match q with
  | Shallow Zero -> raise Empty
  | Shallow (One x) -> (x, empty)
  | Shallow (Two (x, y)) -> (x, Shallow (One y))
  | Shallow (Three (x, y, z)) -> (x, Shallow (Two (y, z)))
  | Deep {f= Zero; _} -> assert false
  | Deep {s; f= One x; m= (lazy q'); r} ->
      if is_empty q' then (x, Shallow r)
      else
        let (y, z), q' = shift q' in
        (x, _deep (s - 1) (Two (y, z)) (Lazy.from_val q') r)
  | Deep {s; f= Two (x, y); m; r} -> (x, _deep (s - 1) (One y) m r)
  | Deep {s; f= Three (x, y, z); m; r} -> (x, _deep (s - 1) (Two (y, z)) m r)

let rec cons : 'a. 'a t -> 'a -> 'a t =
 fun q x ->
  match q with
  | Shallow Zero -> Shallow (One x)
  | Shallow (One y) -> Shallow (Two (x, y))
  | Shallow (Two (y, z)) -> Shallow (Three (x, y, z))
  | Shallow (Three (y, z, z')) -> _deep 4 (Two (x, y)) _empty (Two (z, z'))
  | Deep {f= Zero; _} -> assert false
  | Deep {s; f= One y; m; r} -> _deep (s + 1) (Two (x, y)) m r
  | Deep {s; f= Two (y, z); m; r} -> _deep (s + 1) (Three (x, y, z)) m r
  | Deep {s; f= Three (y, z, z'); m= (lazy q'); r} ->
      _deep (s + 1) (Two (x, y)) (lazy (cons q' (z, z'))) r

let _digit_to_seq d k =
  match d with
  | Zero -> ()
  | One x -> k x
  | Two (x, y) -> k x ; k y
  | Three (x, y, z) -> k x ; k y ; k z

type 'a sequence = ('a -> unit) -> unit

let rec to_seq : 'a. 'a t -> 'a sequence =
 fun q k ->
  match q with
  | Shallow d -> _digit_to_seq d k
  | Deep {f; m= (lazy q'); r; _} ->
      _digit_to_seq f k ;
      to_seq q' (fun (x, y) -> k x ; k y) ;
      _digit_to_seq r k

let iter f q = to_seq q f

let _fold_digit f acc d =
  match d with
  | Zero -> acc
  | One x -> f acc x
  | Two (x, y) -> f (f acc x) y
  | Three (x, y, z) -> f (f (f acc x) y) z

let rec fold : 'a 'b. ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b =
 fun func acc q ->
  match q with
  | Shallow d -> _fold_digit func acc d
  | Deep {f; m= (lazy q'); r; _} ->
      let acc = _fold_digit func acc f in
      let acc = fold (fun acc (x, y) -> func (func acc x) y) acc q' in
      _fold_digit func acc r

let to_list q =
  let l = ref [] in
  to_seq q (fun x -> l := x :: !l) ;
  List.rev !l

let of_list l = List.fold_left push empty l

let pp ppv ppf q =
  Fmt.pf ppf "[ %a ]"
    (Fmt.hvbox (Fmt.list ~sep:(Fmt.unit ";@ ") ppv))
    (to_list q)

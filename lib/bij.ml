let ( <.> ) f g x = f (g x)

let identity x = x

type ('a, 'b) t = { to_ : 'a -> 'b; of_ : 'b -> 'a }

let v ~fwd ~bwd = { to_ = fwd; of_ = bwd }

let bwd { of_; _ } = of_

let fwd { to_; _ } = to_

let flip { of_; to_ } = { to_ = of_; of_ = to_ }

let product u v =
  {
    to_ = (fun (a, b) -> (u.to_ a, v.to_ b));
    of_ = (fun (a, b) -> (u.of_ a, v.of_ b));
  }

let compose u v = { to_ = u.to_ <.> v.to_; of_ = u.of_ <.> v.of_ }

let commute = { to_ = (fun (a, b) -> (b, a)); of_ = (fun (b, a) -> (a, b)) }

let identity = { to_ = identity; of_ = identity }

exception Bijection

let cons =
  {
    to_ = (fun (x, r) -> x :: r);
    of_ =
      (function
      | x :: r -> (x, r)
      | [] ->
          Fmt.epr "GOT AN EMPTY LIST.\n%!" ;
          raise Bijection);
  }

let char chr =
  {
    to_ = (fun chr' -> if Char.equal chr chr' then () else raise Bijection);
    of_ = (fun () -> chr);
  }

let string str =
  {
    to_ = (fun str' -> if String.equal str str' then () else raise Bijection);
    of_ = (fun () -> str);
  }

let element p =
  {
    to_ = (fun v -> if p v then v else raise Bijection);
    of_ = (fun v -> if p v then v else raise Bijection);
  }

let some =
  {
    to_ = (fun x -> Some x);
    of_ = (function Some x -> x | None -> raise Bijection);
  }

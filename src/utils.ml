(* My little std lib. *)

let flip f x y = f y x

module Option = struct

  let get = function
    | Some x -> x
    | None -> raise Not_found

  let bind x f = match x with
    | None -> None
    | Some x -> f x

  let map x f = match x with
    | None -> None
    | Some x -> Some (f x)
end

(* Not tail rec *)
let concat_optlist l =
  let aux x l = match x with
    | None -> l
    | Some x -> x :: l
  in
  List.fold_right aux l []

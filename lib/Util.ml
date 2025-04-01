(** Applies [f] to [x] [n] times. Tail-recursive. *)
let rec church_l n f x =
  if n <= 0 then
    x
  else
    church_l (pred n) f (f x)

(** Applies [f] to [x] [n] times. Not tail-recursive. *)
let rec church_r n f x =
  if n <= 0 then
    x
  else
    f (church_r (pred n) f x)

(** Splits [l] in two, where the first list is the greatest prefix of [l] with all elements satisfying the predicate
    [f]. *)
let span l ~f = (Core.List.take_while l ~f, Core.List.drop_while l ~f)

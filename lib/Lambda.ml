type lambda = Var of string | Abs of string * lambda | App of lambda * lambda

module VarSet = Set.Make (String)

(** Constructs the set of free variables for a given lambda expression. *)
let rec free = function
  | Var x -> VarSet.singleton x
  | Abs (x, e) -> VarSet.(diff (free e) (singleton x))
  | App (lhs, rhs) -> VarSet.union (free lhs) (free rhs)

(** Infix operator for application. *)
let ( -@ ) lhs rhs = App (lhs, rhs)

(** Folds a list of binders and constructs a lambda expression. The number of abstractions equals the number of binders.
    Fails if the binder list is empty. *)
let rec unfold_binders binders e =
  match binders with
  | [] -> failwith "Binders absent"
  | [ binder ] -> Abs (binder, e)
  | b :: bs -> Abs (b, unfold_binders bs e)

(** Checks if a lambda expression is a combinator, i.e., contains no free variables. *)
let is_combinator e = VarSet.is_empty (free e)

(** Substitutes [e] for [x] in a given lambda expression. *)
let rec subst x e = function
  | Var y as var ->
      if x = y then
        e
      else
        var
  | Abs (y, e') as abs ->
      if x = y then
        abs
      else
        Abs (y, subst x e e')
  | App (lhs, rhs) -> App (subst x e lhs, subst x e rhs)

(** Performs β-reduction once. *)
let rec beta_reduce_once = function
  | App (Abs (n, lhs), rhs) -> subst n rhs lhs
  | App (lhs, rhs) -> App (beta_reduce_once lhs, beta_reduce_once rhs)
  | Abs (n, e) -> Abs (n, beta_reduce_once e)
  | Var n -> Var n

(** Performs β-reduction [n] times. *)
let beta_reduce n = Util.church_l n beta_reduce_once

(** Performs β-reduction until normal form is reached. May hang on some lambda expressions, be careful! :) *)
let rec beta_reduce_until_nf e =
  let step = beta_reduce_once e in
  if step = e then
    e
  else
    beta_reduce_until_nf step

module Expressions = struct
  let lId = Abs ("x", Var "x")
  let lTrue = Abs ("x", Abs ("y", Var "x"))
  let lFalse = Abs ("x", Abs ("y", Var "y"))
  let lIf = Abs ("pred", Abs ("then", Abs ("else", Var "pred" -@ Var "then" -@ Var "else")))
  let l0 = Abs ("f", Abs ("x", Var "x"))
  let l1 = Abs ("f", Abs ("x", Var "f" -@ Var "x"))
  let l2 = Abs ("f", Abs ("x", Var "f" -@ (Var "f" -@ Var "x")))
  let l3 = Abs ("f", Abs ("x", Var "f" -@ (Var "f" -@ (Var "f" -@ Var "x"))))
  let lSuccl = Abs ("n", Abs ("f", Abs ("x", Var "n" -@ Var "f" -@ (Var "f" -@ Var "x"))))
  let lPower = Abs ("n", Abs ("m", Var "m" -@ Var "n"))
  let lK = lTrue
  let lConst = lK
  let lS = Abs ("g", Abs ("f", Abs ("x", Var "g" -@ Var "x" -@ (Var "f" -@ Var "x"))))
  let lI = lId
  let lomega = Abs ("x", Var "x" -@ Var "x")
  let lOmega = lomega -@ lomega
  let lY = Abs ("f", Abs ("x", Var "f" -@ (Var "x" -@ Var "x")) -@ Abs ("x", Var "f" -@ (Var "x" -@ Var "x")))
end

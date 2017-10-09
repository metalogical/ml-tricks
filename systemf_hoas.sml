structure SystemFType = struct
  datatype typ =
    FORALL of typ -> typ
  | ARR of typ * typ
  | DUMMY of int

  val counter = ref 0
  fun dummy () = let val x = !counter val _ = counter := x + 1 in DUMMY x end

  fun eq (ARR (ty1_1, ty1_2)) (ARR (ty2_1, ty2_2)) =
        eq ty1_1 ty2_1 andalso eq ty1_2 ty2_2
    | eq (FORALL f1) (FORALL f2) =
        let val ty = dummy() in eq (f1 ty) (f2 ty) end
    | eq (DUMMY i) (DUMMY j) = i = j
    | eq _ _ = false
end

signature SystemF = sig
  type typ
  type term

  val lam : typ -> (term -> term) -> term
  val tlam : (typ -> term) -> term
  val app : term -> term -> term
  val tapp : term -> typ -> term

  val forall : (typ -> typ) -> typ
  val arr : typ -> typ -> typ
end

structure Static :> SystemF = struct
  open SystemFType
  type term = typ

  fun lam (t : typ) (f : term -> term) = f t
  fun tlam (f : typ -> term) = FORALL f
  fun app (ARR (ty1, ty2)) (t : term) = case eq ty1 t of true => ty2
  fun tapp (FORALL f) (t : term) = f t

  val forall = FORALL
  fun arr ty1 ty2 = ARR (ty1, ty2)
end

functor Program(Semantics : SystemF) = struct
  open Semantics
  val _ = app (lam (forall (fn t => t)) (fn x => x)) (tlam (fn t => lam t (fn x => x)))
end

(* This actually runs in SML/NJ! *)

signature EQ = sig
  type ('a, 'b) eq
  val refl : ('a, 'a) eq
  val subst_r : ('a, 'b) eq -> 'a -> 'b
  val subst_l : ('b, 'a) eq -> 'a -> 'b
end

structure Eq : EQ = struct
  type ('a, 'b) eq = ('a -> 'b) * ('b -> 'a) (* isomorphism *)

  val refl = (fn x => x, fn x => x)
  fun subst_r (r, l) = r
  fun subst_l (r, l) = l
end


structure Exp : sig
  type 'a exp

  val Plus : int exp * int exp -> int exp
  val Int : int -> int exp
  val Str : string -> string exp
  val Concat : string exp * string exp -> string exp
  val Str_to_int : string exp -> int exp

  val eval : 'a exp -> 'a
end = struct
  open Eq

  datatype 'a exp =
    PLUS of ('a, int) eq * int exp * int exp
  | INT of ('a, int) eq * int
  | STR of ('a, string) eq * string
  | CONCAT of ('a, string) eq * string exp * string exp
  | STR_TO_INT of ('a, int) eq * string exp
  
  (* Because of the restricted way to build ('a, 'b) eq witnesses,
   * we actually can't make a PLUS with any type other then int exp :) *)
  fun Plus (e1, e2) = PLUS (Eq.refl, e1, e2)
  fun Int i = INT (Eq.refl, i)
  fun Str s = STR (Eq.refl, s)
  fun Concat (s1, s2) = CONCAT (Eq.refl, s1, s2)
  fun Str_to_int s = STR_TO_INT (Eq.refl, s)
  
  fun eval (e : 'a exp) : 'a = case e of
    (* Eq.subst_l here allows us to coerce the type of each case to 'a instead
     * of int or string *)
    PLUS (eq, e1, e2) => subst_l eq (eval_int e1 + eval_int e2)
  | INT (eq, n) => subst_l eq n
  | STR (eq, s) => subst_l eq s
  | CONCAT (eq, e1, e2) => subst_l eq (eval_string e1 ^ eval_string e2)
  | STR_TO_INT (eq, e) => subst_l eq (Option.valOf (Int.fromString (eval_string e)))

  (* unfortunately, SML doesn't support polymorphic recursion, but we don't
   * actually need these functions in e.g. OCaml (they're an artifact of SML not
   * this GADT hack). *)
  and eval_int (e : int exp) : int = case e of
    PLUS (_, e1, e2) => eval_int e1 + eval_int e2
  | INT (_, n) => n
  | STR_TO_INT (eq, e) => Option.valOf (Int.fromString (eval_string e))

  and eval_string (e : string exp) : string = case e of
    STR (_, s) => s
  | CONCAT (_, e1, e2) => eval_string e1 ^ eval_string e2
end
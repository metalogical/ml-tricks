(* Use of "Fold" technique (http://mlton.org/Fold) to implement infix arithmetic *)
fun fold acc step = step acc


datatype operator = Add | Mul | Sub | Div
type operation = int * operator
type 'a accumulator = operation list * 'a


val arithmetic = fn z => fold ([], fn x => x) z
fun ` (acc : ('a -> 'b) accumulator) (x : 'a) =
  let
    val (ops, f) = acc
  in fold (ops, f x) end
(* in reality, done should parse the final accumulator and evaluate to an int,
*  but that's too much work, so we just return the accumulator for this example. *)
fun done (acc : int accumulator) =
  let
    val (l, n) = acc
  in
    (l, n)
  end


fun make_op (oper : operator) (acc : int accumulator) =
  let
    val (l, n) = acc
  in
    fold ((n, oper) :: l, fn x => x)
  end
val `+ = fn x => make_op Add x
val `* = fn x => make_op Mul x
val `- = fn x => make_op Sub x
val `/ = fn x => make_op Div x


(* example of use *)
fun factorial 0 = 1
  | factorial n = n * factorial (n - 1)

val ([(3, Mul), (2, Add)], 6) =
  arithmetic
    `2 `+ `3 `* `factorial `3
  done

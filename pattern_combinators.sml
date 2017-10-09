structure Pattern = struct
  fun $ out inp = out inp
  fun ? out inp = out

  fun SOME_ p out (SOME x) = p out x
  fun NONE_ out NONE = out

  fun op::! (p1, p2) out (x :: xs) = p2 (p1 out x) xs
  fun nil_ out [] = out

  fun op=>> (p, out) = p out
  fun op|| (p1out, p2out) inp = p1out inp handle Match => p2out inp

  fun match inp pout = pout inp ()
end

structure Test = struct
  open Pattern
  infixr 5 ::!
  infix 3 =>>
  infixr 2 ||

  fun len l = match l (
      nil_ =>> (fn() => 0)
  ||  (? ::! $) =>> (fn xs => fn() => 1 + len xs))
end

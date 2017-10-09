module type SIG = sig type t end

module Applicative : sig
    type 'a u
    module F : (X : SIG) -> (sig type t = X.t u end)
end = struct
    type 'a u = ...
    module F (X : SIG) = struct
        type t = X.t u
    end
end

module A = Applicative.F (struct type t = int end)
module B = Applicative.F (struct type t = int end)
(* now A.t = B.t = int Applicative.u *)
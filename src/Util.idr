module Util

%default total

public export
infixl 4 <**>

public export
(<**>) : Applicative f => f a -> f (a -> b) -> f b
(<**>) = flip (<*>)

public export
data Const a b = MkConst a

public export
getConst : Const a b -> a
getConst (MkConst a) = a

public export
Functor (Const m) where
  map _ (MkConst v) = MkConst v

public export
Monoid m => Applicative (Const m) where
  pure _ = MkConst neutral
  (MkConst a) <*> (MkConst b) = MkConst (a <+> b)
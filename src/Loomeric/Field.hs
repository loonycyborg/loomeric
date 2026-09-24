{-# LANGUAGE MagicHash #-}
module Loomeric.Field where

import GHC.Float
import GHC.Exts

import Prelude (($))

import Loomeric.Group
import Loomeric.Ring
import Loomeric.Ratio

infixr 8 **

{- | === A characteristic 0 algebraic field

    A 'Ring' that is group both under addition and multiplication.
    Characteristic 0 fields have an unique homomorphism from rationals allowing
    'fromRational' to be implemented.
-}
class (MultiplicativeGroup a, Ring a) => Field a where
    fromRational :: Rational -> a

type Fractional a = Field a

{- | === An exponential field

    A 'Field' that is closed under exponentation, perhaps with some limitations
    like roots of negative numbers.
-}
class Field a => ExponentialField a where
    (**) :: a -> a -> a
    exp :: a -> a
    log :: a -> a

instance Field Float where
    fromRational x = rationalToFloat (numerator x) (denominator x)

instance ExponentialField Float where
    (F# a) ** (F# b) = F# $ powerFloat# a b
    exp (F# x) = F# $ expFloat# x
    log (F# x) = F# $ logFloat# x

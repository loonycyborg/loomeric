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

class Field a => TrigonometricField a where
    pi :: a
    sin :: a -> a
    cos :: a -> a
    tan :: a -> a
    asin :: a -> a
    acos :: a -> a
    atan :: a -> a
    sinh :: a -> a
    cosh :: a -> a
    tanh :: a -> a
    asinh :: a -> a
    acosh :: a -> a
    atanh :: a -> a

instance Field Float where
    fromRational x = rationalToFloat (numerator x) (denominator x)

instance ExponentialField Float where
    (F# a) ** (F# b) = F# $ powerFloat# a b
    exp (F# x) = F# $ expFloat# x
    log (F# x) = F# $ logFloat# x

instance TrigonometricField Float where
    pi = 3.141592653589793238
    sin (F# x) = F# $ sinFloat# x
    cos (F# x) = F# $ cosFloat# x
    tan (F# x) = F# $ tanFloat# x
    asin (F# x) = F# $ asinFloat# x
    acos (F# x) = F# $ acosFloat# x
    atan (F# x) = F# $ atanFloat# x
    sinh (F# x) = F# $ sinhFloat# x
    cosh (F# x) = F# $ coshFloat# x
    tanh (F# x) = F# $ tanhFloat# x
    asinh (F# x) = F# $ asinhFloat# x
    acosh (F# x) = F# $ acoshFloat# x
    atanh (F# x) = F# $ atanhFloat# x

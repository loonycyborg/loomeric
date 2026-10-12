module Loomeric.Data.Conversions.Partial where

import GHC.Num.Integer
import GHC.Num.Natural
import GHC.Float (Float(..), isFloatInfinite, isFloatNaN)

import Prelude (($), (.), (||), (/=), Ord(..), Maybe(..), Word(..), Int(..))

import Loomeric.Data.Conversions.Integer
import Loomeric.Data.Conversions.Rational
import Loomeric.Data.Conversions.Float
import Loomeric.Data.Ratio
import Loomeric.Algebra.Ring

class Countable a where
    toNumber :: a -> (Maybe Rational, Maybe Integer, Maybe Natural)

instance Countable Word where
    toNumber = toNumber . toNatural

instance Countable Natural where
    toNumber x = (Just $ toRational x, Just $ toInteger x, Just x)

instance Countable Int where
    toNumber = toNumber . toInteger

instance Countable Integer where
    toNumber x = (Just $ toRational x, Just $ toInteger x, to_natural x) where
        to_natural x = case signum x of
            -1 -> Nothing
            _  -> Just $ toNatural $ absG x

instance Countable Rational where
    toNumber x = case (numerator x, denominator x) of
        (i, 1) -> toNumber i
        _      -> (Just x, Nothing, Nothing)

instance Countable Float where
    toNumber x = if 0 /= isFloatNaN x || 0 /= isFloatInfinite x then
        (Nothing, Nothing, Nothing) else
        toNumber $ toRational x

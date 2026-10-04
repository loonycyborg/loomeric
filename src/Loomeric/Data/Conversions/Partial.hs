module Loomeric.Data.Conversions.Partial where

import GHC.Num.Integer
import GHC.Num.Natural

import Prelude (($), (.), Maybe(..), Word(..), Int(..))

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
            _  -> Just $ toNatural $ signTruncate @Natural $ abs x

instance Countable Rational where
    toNumber x = case (numerator x, denominator x) of
        (i, 1) -> toNumber i
        _      -> (Just x, Nothing, Nothing)

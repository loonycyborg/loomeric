{-# LANGUAGE NamedDefaults #-}
module LoomericPrelude where
import Prelude hiding (Num(..), Real(..), Integral(..), Fractional(..), Floating(..), RealFrac(..), RealFloat(..), Rational, subtract, even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac, product, sum)

import Loomeric.Group
import Loomeric.Conversions
import Loomeric.Ratio

default AdditiveSemigroup (Int, Float)
default MultiplicativeSemigroup (Int, Float)

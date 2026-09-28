{-# LANGUAGE NamedDefaults #-}
module LoomericPrelude where
import Prelude hiding (Num(..), Real(..), Integral(..), Fractional(..), Floating(..), RealFrac(..), RealFloat(..), Rational, subtract, even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac, product, sum)

import Loomeric.Algebra.Group
import Loomeric.Algebra.Ring
import Loomeric.Algebra.Field
import Loomeric.Data.Conversions
import Loomeric.Data.Integer
import Loomeric.Data.Ratio
import Loomeric.Algebra.Module
import Loomeric.Algebra.VectorSpace

default AdditiveSemigroup (Int, Float)
default MultiplicativeSemigroup (Int, Float)

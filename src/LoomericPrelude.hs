{-# LANGUAGE NamedDefaults #-}
module LoomericPrelude (
    module LoomericPrelude,
    module Prelude,
    module Generics.SOP,
    module Loomeric.Algebra.Group,
    module Loomeric.Algebra.Ring,
    module Loomeric.Algebra.Field,
    module Loomeric.Data.Conversions,
    module Loomeric.Data.Integer,
    module Loomeric.Data.Ratio,
    module Loomeric.Algebra.Module,
    module Loomeric.Algebra.VectorSpace
    ) where
import Prelude hiding (Num(..), Real(..), Integral(..), Fractional(..), Floating(..), RealFrac(..), RealFloat(..), Rational, subtract, even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac, product, sum)

import Generics.SOP

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

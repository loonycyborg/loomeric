{-# LANGUAGE DefaultSignatures #-}
module Loomeric.Module where
import Data.MonoTraversable
import Data.Type.Equality

import Loomeric.Group
import Loomeric.Ring

class (Semiring s, AdditiveMonoid a) => SemiModule s a where
    (*^) :: s -> a -> a
    default (*^) :: (MonoFunctor a, Element a ~ s) => s -> a -> a
    scale *^ x = omap (scale *) x
    (^*) :: a -> s -> a
    default (^*) :: (MonoFunctor a, Element a ~ s) => a -> s -> a
    x ^* scale = omap (* scale) x

class (Ring s, AdditiveGroup a, SemiModule s a) => Module s a

instance Semiring a => SemiModule a a where
    (*^) = (*)
    (^*) = (*)

class SemiModule s a => Normed s a where
    norm :: a -> s

instance OrderedSemiring a => Normed a a where
    norm = abs

{-# LANGUAGE DataKinds, TypeFamilies, FunctionalDependencies, AllowAmbiguousTypes, UndecidableInstances, UndecidableSuperClasses #-}
module Loomeric.Algebra.VectorSpace where

import GHC.TypeLits
import Data.Kind

import Loomeric.Algebra.Module
import Loomeric.Algebra.Group
import Loomeric.Algebra.Field

class (Module s a, AdditiveGroup a, Field s, Coords coords s a, 1 <= Dimension coords) => VectorSpace (coords :: [Symbol]) s a | a coords -> s where
    (^/) :: a -> s -> a
    x ^/ y = x ^* inverse y
    (/^) :: s -> a -> a
    x /^ y = inverse x *^ y

class (VectorSpace n s a, Normed s a) => NormedVectorSpace n s a | a n -> s

type Dimension :: [Symbol] -> Nat
type family Dimension sl where
    Dimension '[] = TypeError (Text "Illegal vector space without dimensions")
    Dimension (_ : '[]) = 1
    Dimension (_ : xs) = Dimension xs + 1

class HasCoordinate (n :: Symbol) s a | n a -> s where
    basisVec :: a
    coord :: a -> s

type Coords :: [Symbol] -> Type -> Type -> Constraint
type family Coords sl s a where
    Coords '[]     s a = ()
    Coords (c : l) s a = (HasCoordinate c s a, Coords l s a)

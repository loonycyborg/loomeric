{-# LANGUAGE DataKinds, TypeFamilies, FunctionalDependencies, AllowAmbiguousTypes, UndecidableInstances, UndecidableSuperClasses #-}
module Loomeric.Algebra.VectorSpace where

import GHC.TypeLits
import Data.Kind
import Data.Proxy

import Generics.SOP

import Loomeric.Algebra.Module
import Loomeric.Algebra.Group
import Loomeric.Algebra.Field

class (Module s v, AdditiveGroup v, Field s, 1 <= Dimension coords, All (HasCoordinate v s) coords) => VectorSpace v s (coords :: [Symbol]) | v -> coords s where
    (^/) :: v -> s -> v
    x ^/ y = x ^* inverse y
    (/^) :: s -> v -> v
    x /^ y = inverse x *^ y
    basis :: [v]
    basis = hcollapse (hcpure (Proxy @(HasCoordinate v s)) f :: NP (K v) coords) where
        f :: forall (n :: Symbol) . HasCoordinate v s n => K v n
        f = K (basisVec @v @s @n)
    coords :: v -> [s]
    coords x = hcollapse (hcpure (Proxy @(HasCoordinate v s)) f :: NP (K s) coords) where
        f :: forall (n :: Symbol) . HasCoordinate v s n => K s n
        f = K (coord @v @s @n x)

class (VectorSpace v s coords, Normed s v) => NormedVectorSpace v s coords | v -> coords s

class (NormedVectorSpace v s coords) => InnerProductSpace v s coords | v -> coords s where
    dot :: v -> v -> s

infixr 8 `dot`

type Dimension :: [Symbol] -> Nat
type family Dimension sl where
    Dimension '[] = TypeError (Text "Illegal vector space without dimensions")
    Dimension (_ : '[]) = 1
    Dimension (_ : xs) = Dimension xs + 1

class HasCoordinate v s (c :: Symbol) | c v -> s where
    basisVec :: v
    coord :: v -> s

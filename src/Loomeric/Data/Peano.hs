{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilyDependencies, NoStarIsType, TypeAbstractions, UndecidableInstances #-}
module Loomeric.Data.Peano where
import Prelude (error, ($), Int, Integer, Show(..), Eq(..), Bool(..), Either(..), otherwise, Enum(..), (||), Ordering (EQ, LT, GT), Semigroup(..))
import Data.Kind
import Data.Maybe
import GHC.TypeNats
import Data.Type.Bool
import Data.Type.Equality
import Control.Category
import GHC.TypeLits.Compare
import GHC.Natural (naturalToInteger)

import Loomeric.Algebra.Group
import Loomeric.Data.Conversions.Integer
import Loomeric.Algebra.Ring
import Loomeric.Algebra.Partial
import Loomeric.Data.Integer

data Peano :: Nat -> Type where
    Zero :: Peano 0
    Succ :: Peano n -> Peano (n + 1)

deriving instance Show (Peano n)

data SomePeano = forall n . KnownNat n => SomePeano (Peano n)

deriving instance Show SomePeano
instance Eq SomePeano where
    (SomePeano Zero)     == (SomePeano Zero)     = True
    x                    == (SomePeano Zero)     = False
    (SomePeano Zero)     == x                    = False
    (SomePeano (Succ x)) == (SomePeano (Succ y)) = SomePeano x == SomePeano y

instance Enum SomePeano where
    toEnum = fromNatural . absG . toInteger
    fromEnum = fromInteger . toInteger

addP :: Peano a -> Peano b -> Peano (a + b)
addP x Zero = x
addP x (Succ y) = Succ (addP x y)

subP :: b <= a => Peano a -> Peano b -> Peano (a - b)
subP Zero     Zero     = Zero
subP (Succ x) Zero     = Succ x
subP (Succ x) (Succ y) = subP x y

mulP :: Peano a -> Peano b -> Peano (a * b)
mulP x Zero = Zero
mulP x (Succ y) = addP x (mulP x y)

instance AdditiveSemigroup SomePeano where
    (SomePeano x) + (SomePeano y) = SomePeano $ addP x y

instance AdditiveMonoid SomePeano where
    zero = SomePeano Zero

instance MultiplicativeSemigroup SomePeano where
    (SomePeano x) * (SomePeano y) = SomePeano $ mulP x y

instance MultiplicativeMonoid SomePeano where
    one = SomePeano $ Succ Zero

instance Semiring SomePeano where
    fromNatural 0 = SomePeano Zero
    fromNatural n = case fromNatural (n -! 1) of
        SomePeano m -> SomePeano $ Succ m

instance AdditivePartialGroup SomePeano where
    SomePeano (x :: Peano a) -? SomePeano (y :: Peano b) = case (natSing @b) %<=? (natSing @a) of
        LE Refl -> Just $ SomePeano $ subP x y
        NLE _ _ -> Nothing
    (-!!) = ((.) . (.)) fromJust (-?) -- Authentic -!! implementation would require unsafeCoerce to force b <= a to be true
                                      -- but that can directly cause segfaults which is even worse than integer underflow

instance PeanoSystem SomePeano where
    increment (SomePeano x) = SomePeano $ Succ x
    decrement (SomePeano Zero)     = Nothing
    decrement (SomePeano (Succ x)) = Just $ SomePeano x

instance ToNatural SomePeano where
    toNatural (SomePeano Zero)     = 0
    toNatural (SomePeano (Succ x)) = 1 + toNatural (SomePeano x)

instance ToInteger SomePeano where
    toInteger x = naturalToInteger $ toNatural x

data SPeano :: Nat -> Nat -> Type where
    SPeano :: Peano a -> Peano b -> SPeano a b

sNormalize :: forall is_positive a b . (is_positive ~ (b <=? a), KnownNat a, KnownNat b) => SPeano a b -> SPeano (If is_positive (a - b) 0) (If is_positive 0 (b - a))
sNormalize (SPeano x x') = case (natSing @b) %<=? (natSing @a) of
    LE Refl       -> SPeano (subP x x') Zero
    NLE Refl Refl -> SPeano Zero (subP x' x)

deriving instance Show (SPeano s n)

data SomeSPeano where
    SomeSPeano :: (KnownNat a, KnownNat b) => SPeano a b -> SomeSPeano

deriving instance Show SomeSPeano

addSP :: (KnownNat a, KnownNat b) => SPeano a b -> SPeano c d -> SPeano (a + c) (b + d)
addSP (SPeano x x') (SPeano y y') = SPeano (addP x y) (addP x' y')

subSP :: (KnownNat a, KnownNat b) => SPeano a b -> SPeano c d -> SPeano (a + d) (b + c)
subSP (SPeano x x') (SPeano y y') = SPeano (addP x y') (addP x' y)

mulSP :: (KnownNat a, KnownNat b, KnownNat c, KnownNat d) => SPeano a b -> SPeano c d -> SPeano (a * c + b * d) (a * d + c * b)
mulSP (SPeano x x') (SPeano y y') = SPeano (addP (mulP x y) (mulP x' y')) (addP (mulP x y') (mulP x' y))

instance AdditiveSemigroup SomeSPeano where
    SomeSPeano a + SomeSPeano b = SomeSPeano $ addSP a b

instance AdditiveMonoid SomeSPeano where
    zero = SomeSPeano $ SPeano Zero Zero

instance AdditiveGroup SomeSPeano where
    negate (SomeSPeano (SPeano x x')) = SomeSPeano $ SPeano x' x
    (SomeSPeano x) - (SomeSPeano y) = SomeSPeano $ subSP x y

instance MultiplicativeSemigroup SomeSPeano where
    SomeSPeano x * SomeSPeano y = SomeSPeano $ mulSP x y

instance MultiplicativeMonoid SomeSPeano where
    one = SomeSPeano $ SPeano (Succ Zero) Zero

instance Semiring SomeSPeano where
    fromNatural n = case fromNatural n of
        SomePeano x -> SomeSPeano $ SPeano x Zero

instance Ring SomeSPeano where
    fromInteger n = case (signum n, fromNatural $ absG n) of
        (-1, SomePeano x) -> SomeSPeano $ SPeano Zero x
        (_ , SomePeano x) -> SomeSPeano $ SPeano x Zero

instance ToInteger SomeSPeano where
    toInteger (SomeSPeano (SPeano x x')) = toInteger (SomePeano x) - toInteger (SomePeano x')

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilyDependencies, NoStarIsType, TypeAbstractions, UndecidableInstances #-}
module Loomeric.Peano where
import Prelude (error, ($), Int, Integer, Show(..), Eq(..), Bool(..), Either(..), otherwise, Enum(..), (||), Ordering (EQ, LT, GT), Semigroup(..))
import Data.Kind
import Data.Maybe
import GHC.TypeNats
import Data.Type.Bool
import Data.Singletons.Bool
import Data.Type.Equality
import GHC.TypeLits
import Data.Type.Ord
import Control.Category

import Loomeric.Group
import Loomeric.Conversions
import Loomeric.Ring
import Loomeric.Integer

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

instance AdditivePartialGroup SomePeano

instance PeanoSystem SomePeano where
    increment (SomePeano x) = SomePeano $ Succ x
    decrement (SomePeano Zero)     = Nothing
    decrement (SomePeano (Succ x)) = Just $ SomePeano x

instance ToInteger SomePeano where
    toInteger (SomePeano Zero)     = 0
    toInteger (SomePeano (Succ x)) = 1 + toInteger (SomePeano x)

data PSign = PPlus | PMinus deriving (Eq, Show)
instance Semigroup PSign where
    x <> y | x == y    = PPlus
           | otherwise = PMinus

type family PSign2Bool a = r | r -> a where
    PSign2Bool PPlus  = True
    PSign2Bool PMinus = False

data SPeano :: PSign -> Nat -> Type where
    SPositive :: Peano n -> SPeano PPlus n
    SNegative :: 1 <= n => Peano n -> SPeano PMinus n

peanoSign :: SPeano s a -> PSign
peanoSign SPositive {} = PPlus
peanoSign SNegative {} = PMinus

peanoAbs :: SPeano s a -> Peano a
peanoAbs (SPositive x) = x
peanoAbs (SNegative x) = x

deriving instance Show (SPeano s n)

data SomeSPeano where
    SomeSPeano :: (KnownNat n) => SPeano s n -> SomeSPeano

deriving instance Show SomeSPeano

type family InvertS s = r | r -> s where
    InvertS PPlus  = PMinus
    InvertS PMinus = PPlus

type family AddSign ord s1 s2 where
    AddSign EQ _      _      = PPlus
    AddSign GT s      s      = s
    AddSign GT PPlus  PMinus = PPlus
    AddSign GT PMinus PPlus  = PMinus
    AddSign LT s1     s2     = AddSign GT s2 s1

type family AddAbs ord s1 a s2 b where
    AddAbs EQ s  a s  a = a + a
    AddAbs EQ _  _ _  _ = 0
    AddAbs _  s  a s  b = a + b
    AddAbs GT _  a _  b = a - b
    AddAbs LT _  a _  b = b - a

addSP :: forall ord s1 a s2 b s3 c . (
    c ~ AddAbs ord s1 a s2 b,
    s3 ~ AddSign ord s1 s2,
    ord ~ Compare a b,
    KnownNat a, KnownNat b
    ) => SPeano s1 a -> SPeano s2 b -> SPeano s3 c
addSP x y = case (cmpNat (natSing @a) (natSing @b), x, y) of
    (EQI, SPositive x,        SPositive y)        -> SPositive $ addP x y
    (EQI, SPositive x,        SNegative y)        -> SPositive Zero
    (EQI, SNegative x,        SPositive y)        -> SPositive Zero
    (EQI, SNegative x,        SNegative y)        -> SPositive $ addP x y
    (GTI, SPositive x,        SPositive y)        -> SPositive $ addP x y
    (GTI, SPositive x,        SNegative y)        -> SPositive $ subP x y
    (GTI, SNegative (Succ x), SPositive y)        -> SNegative $ subP (Succ x) y
    (GTI, SNegative x,        SNegative y)        -> SNegative $ addP x y
    (LTI, SPositive x,        SPositive y)        -> SPositive $ addP x y
    (LTI, SPositive x,        SNegative (Succ y)) -> SNegative $ subP (Succ y) x
    (LTI, SNegative x,        SPositive y)        -> SPositive $ subP y x
    (LTI, SNegative x,        SNegative y)        -> SNegative $ addP x y

type family MulSign s1 a s2 b where
    MulSign s _ s _ = PPlus
    MulSign _ _ _ _ = PMinus

mulSPNZ :: (Compare a 0 ~ GT, Compare b 0 ~ GT) => SPeano s1 a -> SPeano s2 b -> SPeano (MulSign s1 a s2 b) (a * b)
mulSPNZ (SPositive x)        (SPositive y)        = SPositive $ mulP x y
mulSPNZ (SPositive (Succ x)) (SNegative (Succ y)) = SNegative $ mulP (Succ x) (Succ y)
mulSPNZ (SNegative (Succ x)) (SPositive (Succ y)) = SNegative $ mulP (Succ x) (Succ y)
mulSPNZ (SNegative x)        (SNegative y)        = SPositive $ mulP x y

mulSP :: (KnownNat a, KnownNat b) => SPeano s1 a -> SPeano s2 b -> SPeano (If (Compare a 0 == EQ || Compare b 0 == EQ) PPlus (MulSign s1 a s2 b)) (a * b)
mulSP (SPositive Zero) b = SPositive Zero
mulSP a (SPositive Zero) = SPositive Zero
mulSP @a @b a b = case (cmpNat (natSing @a) (natSing @0), cmpNat (natSing @b) (natSing @0)) of
    (GTI, GTI) -> mulSPNZ a b

instance AdditiveSemigroup SomeSPeano where
    SomeSPeano (x :: SPeano s1 a) + SomeSPeano (y :: SPeano s2 b) = case (x, y, cmpNat (natSing @a) (natSing @b)) of
        (SPositive a, SPositive b, GTI) -> SomeSPeano $ addSP x y
        (SPositive a, SPositive b, LTI) -> SomeSPeano $ addSP x y
        (SPositive a, SPositive b, EQI) -> SomeSPeano $ addSP x y
        --(SPositive a, SNegative b, GTI) -> SomeSPeano $ addSP x y
        (SPositive a, SNegative b, EQI) -> SomeSPeano $ addSP x y
        (SPositive a, SNegative b, LTI) -> SomeSPeano $ addSP x y
        --(SNegative a, SPositive b, GTI) -> SomeSPeano $ addSP x y
        (SNegative a, SPositive b, EQI) -> SomeSPeano $ addSP x y
        (SNegative a, SPositive b, LTI) -> SomeSPeano $ addSP x y
        (SNegative a, SNegative b, GTI) -> SomeSPeano $ addSP x y
        (SNegative a, SNegative b, EQI) -> SomeSPeano $ addSP x y
        (SNegative a, SNegative b, LTI) -> SomeSPeano $ addSP x y

instance AdditiveMonoid SomeSPeano where
    zero = SomeSPeano $ SPositive Zero

instance AdditiveGroup SomeSPeano where
    negate (SomeSPeano x) = SomeSPeano $ mulSP (SNegative $ Succ Zero) x
    x - y = x + negate y

instance MultiplicativeSemigroup SomeSPeano where
    SomeSPeano x * SomeSPeano y = SomeSPeano $ mulSP x y

instance MultiplicativeMonoid SomeSPeano where
    one = SomeSPeano $ SPositive $ Succ Zero

instance Semiring SomeSPeano where
    fromNatural n = case fromNatural n of
        SomePeano x -> SomeSPeano $ SPositive x

instance Ring SomeSPeano where
    fromInteger n = case (signum n, fromNatural $ signTruncate $ abs n) of
        (0, _) -> SomeSPeano $ SPositive Zero
        (1, SomePeano x) -> SomeSPeano $ SPositive x
        (-1, SomePeano (Succ x)) -> SomeSPeano $ SNegative (Succ x)

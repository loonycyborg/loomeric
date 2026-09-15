{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilyDependencies, NoStarIsType, TypeAbstractions, UndecidableInstances #-}
module Loomeric.Peano where
import Prelude (error, ($), Int, Integer, Show(..), Eq(..), Bool(..), otherwise, Enum(..), (||), Ordering (EQ, LT, GT), Semigroup(..))
import Data.Kind
import Data.Maybe
import GHC.TypeNats
import Data.Type.Bool
import Data.Singletons.Bool
import Data.Type.Equality
import Control.Category

import Loomeric.Group
import Loomeric.Conversions

data Peano :: Nat -> Type where
    Zero :: Peano 0
    Succ :: Peano n -> Peano (n + 1)

deriving instance Show (Peano n)

data SomePeano = forall n . SomePeano (Peano n)

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

subSP :: forall p a b . (SBoolI p, p ~ (b <=? a), If p (b <= a) (a + 1 <= b)) => Peano a -> Peano b -> SPeano (If p PPlus PMinus) (If p (a - b) (b - a))
subSP x y = case sbool @p of
    STrue  -> SPositive $ subP x y
    SFalse -> SNegative $ subP y x

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
    SomeSPeano :: (SBoolI sign, sign ~ PSign2Bool s, KnownNat n) => SPeano s n -> SomeSPeano

deriving instance Show SomeSPeano

type family SubAbsCmp o a b where
    SubAbsCmp 'EQ a b = 0
    SubAbsCmp 'GT a b = a - b
    SubAbsCmp 'LT a b = b - a

type SubAbs a b = SubAbsCmp (CmpNat a b) a b

type family SubSignCmp o a b where
    SubSignCmp 'EQ a b = PPlus
    SubSignCmp 'GT a b = PPlus
    SubSignCmp 'LT a b = PMinus

type SubSign a b = SubSignCmp (CmpNat a b) a b

type family InvertedS x where
    InvertedS PPlus  = PMinus
    InvertedS PMinus = PPlus

type family CombineS x y where
    CombineS PPlus  PPlus  = PPlus
    CombineS PMinus PMinus = PPlus
    CombineS _      _      = PMinus

type family AddSign diffsign is_zero s1 a s2 b where
    AddSign _        True    _ _ _      _ = PPlus
    AddSign diffsign _ PPlus   a PPlus  b = PPlus
    AddSign diffsign _ PPlus   a PMinus b = If diffsign PPlus PMinus
    AddSign diffsign _ PMinus  a s2     b = InvertedS (AddSign diffsign False PPlus a (InvertedS s2) b)

type family AddAbs diffsign s1 a s2 b where
    AddAbs diffsign s a s b = a + b
    AddAbs diffsign _ a _ b = If diffsign (a - b) (b - a)

addSP :: forall abs_diff_positive is_zero s1 a s2 b s3 c . (
    c ~ AddAbs abs_diff_positive s1 a s2 b,
    s3 ~ AddSign abs_diff_positive is_zero s1 a s2 b,
    abs_diff_positive ~ (b <=? a), SBoolI abs_diff_positive,
    is_zero ~ (Not (s1 == s2) && (a == b)), SBoolI is_zero,
    If is_zero (a ~ b) (1 <= c) 
    ) => SPeano s1 a -> SPeano s2 b -> SPeano s3 c
addSP (SPositive x) (SPositive y) = SPositive $ addP x y
addSP (SPositive x) (SNegative y) = case (sbool @abs_diff_positive, sbool @is_zero) of
    (STrue, SFalse)  -> SPositive $ subP x y
    (SFalse, SFalse) -> SNegative $ subP y x
    (STrue, STrue)   -> SPositive Zero
addSP (SNegative x) (SNegative y) = SNegative $ addP x y
addSP (SNegative x) (SPositive y) = case (sbool @abs_diff_positive, sbool @is_zero) of
    (STrue, SFalse)  -> SNegative $ subP x y
    (SFalse, SFalse) -> SPositive $ subP y x
    (STrue, STrue)   -> SPositive Zero

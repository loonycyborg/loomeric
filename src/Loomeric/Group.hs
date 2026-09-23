{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples, DefaultSignatures #-}
module Loomeric.Group where

import Data.Ord
import Data.Foldable1
import Data.Foldable hiding (sum)
import GHC.Exts
import GHC.Num.Primitives (absI#, sgnI#)
import Data.List.NonEmpty as NE
import GHC.Num.Natural
import GHC.Num.Integer
import GHC.Float ( integerToFloat#, naturalToFloat# )
import GHC.Natural ( naturalToInteger )
import Prelude (($), fst, snd, not, uncurry, Eq(..), error)
import Control.Applicative
import Control.Monad
import Control.Category
import Data.Maybe
import Data.Bool

import Loomeric.Conversions

infixr 8 **
infixl 7 *, /, /?, `quot`, `rem`
infixl 6 +, -, -?, -!

{- | === A semigroup under addition

   An alebraic system that has operation '+' called "addition" which satisfies
   the associativity axiom:

   * @(a + b) + c = a + (b + c)@
-}
class AdditiveSemigroup a where
    (+) :: a -> a -> a
    sum1 :: Foldable1 f => f a -> a
    sum1 = foldl1' (+)

{- | === A monoid under addition

    An additive semigroup that also has an element called 'zero' which satisfies:

    * @a + zero = zero@
    * @zero + a = zero@
-}
class AdditiveSemigroup a => AdditiveMonoid a where
    zero :: a
    sum :: Foldable f => f a -> a
    sum = foldl' (+) zero

{- | === A partial group under addition

    An additive monoid that has subtraction operation that it is only actually defined for
    /some/ pairs of elements. Partial subtraction '-?' returns 'Nothing' for elements which can't be
    subtracted. It should satisfy:

    * @a -? a = Just zero@
    * If @a -? b = Just c@ then @a = c + b@
-}
class AdditiveMonoid a => AdditivePartialGroup a where
    (-?) :: a -> a -> Maybe a
    default (-?) :: PeanoSystem a => a -> a -> Maybe a
    a -? b = case decrement b of
        Nothing -> Just a
        Just p  -> (-? p) =<< decrement a
    -- | Calls 'error' for elements that can't be subtracted instead
    (-!) :: a -> a -> a
    a -! b = fromJust $ (a -? b) <|> error "Subtraction underflow"

{- | === A group under addition

    An additive monoid that for each element @a@ has has element @negate a@ which satisfies:

    * @a + negate a = zero@

    and a subtraction operation '-' which satisfies:

    * @a - b = a + negate b@
-}
class AdditiveMonoid a => AdditiveGroup a where
    negate :: a -> a
    (-) :: a -> a -> a

{- | === A semigroup under multiplication

    An algebraic system that has multiply operation '*' that satisfies:

    * @(a * b) * c = a * (b * c)@ (Associativity)
-}
class MultiplicativeSemigroup a where
    (*) :: a -> a -> a
    product1 :: Foldable1 f => f a -> a
    product1 = foldl1' (*)

{- | === A monoid under multiplication

    A multiplicative semigroup that has element called 'one' that satisfies:

    * @one * a = one@
    * @a * one = one@
-}
class MultiplicativeSemigroup a => MultiplicativeMonoid a where
    one :: a
    product :: Foldable f => f a -> a
    product = foldl' (*) one

{- | === A partial group under multiplication

    A multiplicative monoid that has partial division operation '/?' that satisfies:

    * @a /? a = Just one@
    * if @a /? b = Just c@ then @a = c * b@

    '/?' returns 'Nothing' for elements for which division isn't defined
-}
class MultiplicativeMonoid a => MultiplicativePartialGroup a where
    (/?) :: a -> a -> Maybe a
    default (/?) :: EuclideanDomain a => a -> a -> Maybe a
    a /? b = case quotRem a b of
        (result, z) | z == zero -> Just result
        _                       -> Nothing

{- | === A group under multiplication

    A multiplicative monoid that for each element @a@ has has element @inverse a@ which satisfies:

    * @a * inverse a = one@

    and a division operation '/' which satisfies:

    * @a / b = a * inverse b@
-}
class MultiplicativeMonoid a => MultiplicativeGroup a where
    inverse :: a -> a
    (/) :: a -> a -> a

type Fractional a = MultiplicativeGroup a

{- | === An algebraic semiring

    An algebraic structure that is both monoid under addition and monoid under multiplication.
    It also should satisfy:

    * @zero * a = zero@ (Left-annihilation)
    * @a * zero = zero@ (Right-annihilation)
    * @a * (b + c) = a * c + b * c@ (Left distributivity)
    * @(b + c) * a = b * a + c * a@ (Right distributivity)

    Each semiring will have an unique homomorphism from natural numbers which is represented by
    'fromNatural' method
-}
class (AdditiveMonoid a, MultiplicativeMonoid a) => Semiring a where
    linearCombination :: Foldable f => f (a, a) -> a
    linearCombination = sum . concatMap ((:[]) . uncurry (*))
    fromNatural :: Natural -> a

{- | === An algebraic ring

    A 'Semiring' that is also a group under addition. For each ring there is an unique homomorphism
    from integer which is represented by 'fromInteger' method.
-}
class (AdditiveGroup a, Semiring a) => Ring a where
    fromInteger :: Integer -> a

{- | === An algebraic field

    A 'Ring' that is group both under addition and multiplication allowing for the full suite of
    basic algebraic operations.
-}
class (MultiplicativeGroup a, Ring a) => Field a where
    (**) :: a -> a -> a
    exp :: a -> a
    log :: a -> a

class (Semiring a, Ord a) => OrderedSemiring a where
    signum :: a -> a
    abs :: a -> a
    abs x = signum x * x
    absG :: (SignedType b ~ a, SignTruncate b) => a -> b
    absG = signTruncate . abs

class (OrderedSemiring a, Ring a) => OrderedRing a

type Num a = OrderedRing a

{- | === A Peano-like set

    A set with zero element and operation that takes an element and returns the next one.
    Only types where /all/ elements can be built from 'zero' by chain of increments
    should implement this typeclass.
-}
class (Semiring a, AdditivePartialGroup a) => PeanoSystem a where
    increment :: a -> a
    decrement :: a -> Maybe a

{- | === Euclidean Domain

    An ordered 'Semiring' or 'Ring' of integers or similar objects that have partial
    division defined as well as GCD and LCM. 'Fields' and other structures that are
    closed under division (except by zero) should not define this typeclass.
-}
class (OrderedSemiring a, MultiplicativePartialGroup a) => EuclideanDomain a where
    quotRem :: a -> a -> (a,a)
    quot :: a -> a -> a
    quot = (fst <$>) . quotRem
    rem :: a -> a -> a
    rem = (snd <$>) . quotRem

type Integral a = EuclideanDomain a

instance AdditiveSemigroup Word where
    W# a + W# b = W# $ plusWord# a b

instance AdditiveMonoid Word where
    zero = 0

instance AdditivePartialGroup Word where
    (W# a) -? (W# b) = case subWordC# a b of
        (# result, 0# #) -> Just $ W# result
        (# _     , _  #) -> Nothing

instance MultiplicativeSemigroup Word where
    W# a * W# b = W# $ timesWord# a b

instance MultiplicativeMonoid Word where
    one = 1

instance MultiplicativePartialGroup Word

instance Semiring Word where
    fromNatural = naturalToWord

instance OrderedSemiring Word where
    signum x = bool 1 0 $ x == 0
    abs = id

instance PeanoSystem Word where
    increment (W# x) = W# $ plusWord# x 1##
    decrement (W# x) = if isTrue# $ x `eqWord#` 0## then
        Nothing
            else
        Just $ W# $ minusWord# x 1##

instance EuclideanDomain Word where
    quotRem (W# x) (W# y) = case quotRemWord# x y of
        (# r, c #) -> (W# r, W# c)

instance AdditiveSemigroup Int where
    I# a + I# b = I# (a +# b)

instance AdditiveMonoid Int where
    zero = 0

instance AdditiveGroup Int where
    I# a - I# b = I# (a -# b)
    negate (I# a) = I# $ negateInt# a

instance MultiplicativeSemigroup Int where
    I# a * I# b = I# (a *# b)

instance MultiplicativeMonoid Int where
    one = 1

instance MultiplicativePartialGroup Int

instance Semiring Int where
    fromNatural = integerToInt . naturalToInteger
instance Ring Int where
    fromInteger = integerToInt
instance OrderedSemiring Int where
    abs (I# x) = I# $ absI# x
    signum (I# x) = I# $ sgnI# x
instance OrderedRing Int

instance EuclideanDomain Int where
    quotRem (I# a) (I# b) = case quotRemInt# a b of
        (# x, y #) -> (I# x, I# y)

instance AdditiveSemigroup Float where
    F# a + F# b = F# $ plusFloat# a b

instance AdditiveMonoid Float where
    zero = 0

instance AdditiveGroup Float where
    F# a - F# b = F# $ minusFloat# a b
    negate (F# a) = F# $ negateFloat# a

instance MultiplicativeSemigroup Float where
    F# a * F# b = F# $ timesFloat# a b

instance MultiplicativeMonoid Float where
    one = 1

instance MultiplicativeGroup Float where
    F# a / F# b = F# $ divideFloat# a b
    inverse (F# a) = F# $ divideFloat# 1.0# a

instance Semiring Float where
    fromNatural x = F# $ naturalToFloat# x
instance Ring Float where
    fromInteger x = F# $ integerToFloat# x
instance OrderedSemiring Float where
    signum x
        | x > 0     = 1
        | x < 0     = -1
        | otherwise = x
    abs (F# x) = F# $ fabsFloat# x
instance OrderedRing Float
instance Field Float where
    (F# a) ** (F# b) = F# $ powerFloat# a b
    exp (F# x) = F# $ expFloat# x
    log (F# x) = F# $ logFloat# x

instance AdditiveSemigroup Natural where
    (+) = naturalAdd

instance AdditiveMonoid Natural where
    zero = naturalZero

instance AdditivePartialGroup Natural where
    x -? y = case naturalSub x y of
        (# (# #) | #) -> Nothing
        (# | res #)   -> Just res

instance MultiplicativeSemigroup Natural where
    (*) = naturalMul

instance MultiplicativeMonoid Natural where
    one = naturalOne

instance MultiplicativePartialGroup Natural

instance Semiring Natural where
    fromNatural = id
instance OrderedSemiring Natural where
    signum = naturalSignum
    abs = id

instance PeanoSystem Natural where
    increment x = naturalAdd x one
    decrement x = x -? one

instance EuclideanDomain Natural where
    quotRem = naturalQuotRem

instance AdditiveSemigroup Integer where
    (+) = integerAdd

instance AdditiveMonoid Integer where
    zero = integerZero

instance AdditiveGroup Integer where
    (-) = integerSub
    negate = integerNegate

instance MultiplicativeSemigroup Integer where
    (*) = integerMul

instance MultiplicativeMonoid Integer where
    one = integerOne

instance MultiplicativePartialGroup Integer

instance Semiring Integer where
    fromNatural = naturalToInteger
instance Ring Integer where
    fromInteger = id

instance OrderedSemiring Integer where
    abs = integerAbs
    signum = integerSignum

instance OrderedRing Integer

instance EuclideanDomain Integer where
    quotRem = integerQuotRem

subtract :: AdditiveGroup a => a -> a -> a
subtract x y = y - x

even :: EuclideanDomain a => a -> Bool
even x = x `rem` (one + one) == zero

odd :: EuclideanDomain a => a -> Bool
odd = not . even

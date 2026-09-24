{-# LANGUAGE MagicHash #-}
module Loomeric.Group where

import Data.Foldable1
import Data.Foldable hiding (sum)
import GHC.Exts
import GHC.Num.Natural
import GHC.Num.Integer
import Prelude (($), fst, snd, not, uncurry, Eq(..), error)

infixl 7 *, /
infixl 6 +, -

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

{- | === A group under multiplication

    A multiplicative monoid that for each element @a@ has has element @inverse a@ which satisfies:

    * @a * inverse a = one@

    and a division operation '/' which satisfies:

    * @a / b = a * inverse b@
-}
class MultiplicativeMonoid a => MultiplicativeGroup a where
    inverse :: a -> a
    (/) :: a -> a -> a

instance AdditiveSemigroup Word where
    W# a + W# b = W# $ plusWord# a b

instance AdditiveMonoid Word where
    zero = 0

instance MultiplicativeSemigroup Word where
    W# a * W# b = W# $ timesWord# a b

instance MultiplicativeMonoid Word where
    one = 1

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

instance AdditiveSemigroup Natural where
    (+) = naturalAdd

instance AdditiveMonoid Natural where
    zero = naturalZero

instance MultiplicativeSemigroup Natural where
    (*) = naturalMul

instance MultiplicativeMonoid Natural where
    one = naturalOne

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

subtract :: AdditiveGroup a => a -> a -> a
subtract x y = y - x

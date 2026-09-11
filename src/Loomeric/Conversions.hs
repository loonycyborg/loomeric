module Loomeric.Conversions where
import Prelude (($), Int, Word)
import GHC.Num.Integer
import GHC.Num.Natural
import GHC.Natural
import Data.Maybe

import Loomeric.Group

class ToInteger a where
    toInteger :: a -> Integer

instance ToInteger Natural where
    toInteger = naturalToInteger

instance ToInteger Int where
    toInteger = integerFromInt

instance ToInteger Word where
    toInteger = integerFromWord

class (PeanoSystem bt, OrderedSemiring offt) => Offset bt offt where
    (+?) :: bt -> offt -> Maybe bt
    (+!) :: bt -> offt -> bt

instance (PeanoSystem a, OrderedSemiring a) => Offset a a where
    x +? y = Just $ x + y
    x +! y = x + y

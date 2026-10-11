module Loomeric.Data.Conversions.Integer where
import Prelude (($), id, Int, Word)
import GHC.Num.Integer
import GHC.Num.Natural
import GHC.Natural
import Data.Kind
import GHC.Exts

class ToNatural a where
    toNatural :: a -> Natural

instance ToNatural Word where
    toNatural = naturalFromWord

instance ToNatural Natural where
    toNatural = id

class ToInteger a where
    toInteger :: a -> Integer

instance ToInteger Natural where
    toInteger = naturalToInteger

instance ToInteger Int where
    toInteger = integerFromInt

instance ToInteger Word where
    toInteger = integerFromWord

instance ToInteger Integer where
    toInteger = id

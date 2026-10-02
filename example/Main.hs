module Main where

import LoomericPrelude
import Loomeric.Data.Vector

v1, v2 :: Vector4D Float
v1 = Vector4D 1 1 1 1
v2 = Vector4D 1 1 1 2

main :: IO ()
main = print $ v1 + v2

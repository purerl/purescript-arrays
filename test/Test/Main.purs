module Test.Main where

import Prelude

import Effect (Effect)
import Test.Data.Array (testArray)
import Test.Data.Array.Partial (testArrayPartial)

main :: Effect Unit
main = do
  testArray
  testArrayPartial

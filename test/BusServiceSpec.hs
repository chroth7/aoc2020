module BusServiceSpec where

import           BusService
import           Test.Hspec

main :: IO ()
main = hspec spec

testInput :: String
testInput = "939\n\
\7,13,x,x,59,x,31,19"

spec :: Spec
spec = do
  describe "Build the bags" $ do
    it "parsing" $ do
      let (BusState time busses) = parseDay13 testInput
      time `shouldBe` 939
      busses `shouldBe` [7, 13, 59, 31, 19]
      nextBusMagicNumber (parseDay13 testInput) `shouldBe` 295
  describe "Part2" $ do
    it "core logic" $ do
      busPart2 (BusState2 [(17,0),(13,2),(19,3)]) 1 1 `shouldBe` 3417
      busPart2 (parseDay13p2 testInput) 1 1 `shouldBe` 1068781

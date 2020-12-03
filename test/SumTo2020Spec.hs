module SumTo2020Spec where

import           SumTo2020
import           Test.Hspec

main :: IO ()
main = hspec spec

testInput :: [Int]
testInput = [1721, 979, 366, 299, 675, 1456]

spec :: Spec
spec = do
  describe "2 Factor: sum to 2020" $ do
    it "should return a valid pair" $ do
      sumTo2020 testInput `shouldBe` (299, 1721)
    it "should  NOT return a valid pair" $ do
      sumTo2020 [1,2,5,2,54,21,32,199,1000,1000,1,1] `shouldBe` (0, 0)
  describe "3 Factor: sum to 2020" $ do
    it "should return a valid pair" $ do
      triplet2020 testInput `shouldBe` (979, 366, 675)

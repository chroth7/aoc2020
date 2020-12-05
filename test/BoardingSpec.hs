module BoardingSpec where

import           Boarding
import           Test.Hspec

main :: IO ()
main = hspec spec

testInput :: String
testInput = "FBFBBFFRLR"

spec :: Spec
spec = do
  describe "Get seat" $ do
    it "row" $ do
      getRow testInput `shouldBe` 44
    it "column" $ do
      getColumn testInput `shouldBe` 5
    it "seat" $ do
      getSeatId "BFFFBBFRRR" `shouldBe` 567
      getSeatId "FFFBBBFRRR" `shouldBe` 119
      getSeatId "BBFFBBFRLL" `shouldBe` 820

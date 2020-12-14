module NavigationSpec where

import           Navigation
import           Test.Hspec

main :: IO ()
main = hspec spec

testInts :: [Int]
testInts = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

testInput :: String
testInput = "F10\n\
\N3\n\
\F7\n\
\R90\n\
\F11"

spec :: Spec
spec = do
  describe "Navigation" $ do
    it "Test navigtion" $ do
      let NavigationState coord _ = navigate (parseDay12 testInput)
      coord `shouldBe` (-8, 17)

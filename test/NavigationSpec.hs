module NavigationSpec where

import           Navigation
import           Test.Hspec

main :: IO ()
main = hspec spec

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
    it "Test navigtion with Waypoints" $ do
      let WaypointState coord wp = navigateWP (parseDay12WP testInput)
      wp `shouldBe` (4, -10)
      coord `shouldBe` (214, -72)

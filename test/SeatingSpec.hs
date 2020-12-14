module SeatingSpec where

import           Seating
import           Test.Hspec

main :: IO ()
main = hspec spec

testInput :: String
testInput = "L.LL.LL.LL\n\
\LLLLLLL.LL\n\
\L.L.L..L..\n\
\LLLL.LL.LL\n\
\L.LL.LL.LL\n\
\L.LLLLL.LL\n\
\..L.L.....\n\
\LLLLLLLLLL\n\
\L.LLLLLL.L\n\
\L.LLLLL.LL"

step1 :: String
step1 = "#.##.##.##\n\
\#######.##\n\
\#.#.#..#..\n\
\####.##.##\n\
\#.##.##.##\n\
\#.#####.##\n\
\..#.#.....\n\
\##########\n\
\#.######.#\n\
\#.#####.##"

step2 :: String
step2 = "#.LL.L#.##\n\
\#LLLLLL.L#\n\
\L.L.L..L..\n\
\#LLL.LL.L#\n\
\#.LL.LL.LL\n\
\#.LLLL#.##\n\
\..L.L.....\n\
\#LLLLLLLL#\n\
\#.LLLLLL.L\n\
\#.#LLLL.##"

spec :: Spec
spec = do
  describe "Parse" $ do
    it "works" $ do
      let Layout rows cols seats = parseDay11 testInput
      rows `shouldBe` 10
      cols `shouldBe` 10
      length seats `shouldBe` 100
      head seats `shouldBe` ((0,0), [(0,1), (1,0), (1,1)], Free)
      last seats `shouldBe` ((9,9), [(8,8), (8,9), (9,8)], Free)
  describe "Update layout" $ do
    it "works" $ do
      let layout = parseDay11 testInput
      let expectedStep1 = parseDay11 step1
      let expectedStep2 = parseDay11 step2

      let step1 = applyUpdate 3 layout
      step1 `shouldBe` expectedStep1

      let step2 = applyUpdate 3 $ applyUpdate 3 layout
      step2 `shouldBe` expectedStep2

    describe "Full flow" $ do
      it "does the job" $ do
        (countTotalOccupied $ rinseAndRepeatSeating 3 $ parseDay11 testInput) `shouldBe` 37


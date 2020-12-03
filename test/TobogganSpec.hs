module TobogganSpec where 

import Test.Hspec
import Toboggan

main :: IO ()
main = hspec spec

testInput :: [String]
testInput = [
  "..##.......",
  "#...#...#..",
  ".#....#..#.",
  "..#.#...#.#",
  ".#...##..#.",
  "..#.##.....",
  ".#.#.#....#",
  ".#........#",
  "#.##...#...",
  "#...##....#",
  ".#..#...#.#"]

spec :: Spec
spec = do
  describe "Validate one password" $ do
    it "basic run" $ do
      tobogganRunBasic testInput `shouldBe` 7
    it "spec movement" $ do
      tobogganRun testInput (1, 1) `shouldBe` 2
      tobogganRun testInput (1, 3) `shouldBe` 7
      tobogganRun testInput (1, 5) `shouldBe` 3
      tobogganRun testInput (1, 7) `shouldBe` 4
      tobogganRun testInput (2, 1) `shouldBe` 2

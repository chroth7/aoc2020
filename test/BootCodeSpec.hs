module BootCodeSpec where

import           BootCode
import           Test.Hspec

main :: IO ()
main = hspec spec

testInput :: String
testInput = "nop +0\n\
\acc +1\n\
\jmp +4\n\
\acc +3\n\
\jmp -3\n\
\acc -99\n\
\acc +1\n\
\jmp -4\n\
\acc +6"

spec :: Spec
spec = do
  describe "Parse Instructions" $ do
    it "basic" $ do
      head (parseInstructions testInput) `shouldBe` FullInstruction NOP 0
  describe "Run instructions" $ do
    it "runs" $ do
      runBootSequence (parseInstructions testInput) initBootState `shouldBe` 5

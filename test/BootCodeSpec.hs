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

testInputFixed :: String
testInputFixed = "nop +0\n\
\acc +1\n\
\jmp +4\n\
\acc +3\n\
\jmp -3\n\
\acc -99\n\
\acc +1\n\
\nop -4\n\
\acc +6"

spec :: Spec
spec = do
  describe "Parse Instructions" $ do
    it "basic" $ do
      head (parseInstructions testInput) `shouldBe` FullInstruction NOP 0
  describe "Run instructions and terminate everywhere" $ do
    it "runs" $ do
      runBootSequenceTerminateEverywhere (parseInstructions testInput) initBootState `shouldBe` 5
  describe "Run instructions and terminate only at the end" $ do
    it "runs" $ do
      runBootSequenceTerminateEndOnly (parseInstructions testInput) initBootState `shouldBe` Nothing
      runBootSequenceTerminateEndOnly (parseInstructions testInputFixed) initBootState `shouldBe` Just 8
  describe "Can fix instruction set" $ do
    it "fixes" $ do
      findTermination (parseInstructions testInput) 0 `shouldBe` Just 8


module PortEncodingSpec where

import           PortEncoding
import           Test.Hspec

main :: IO ()
main = hspec spec

testInput :: String
testInput = "35\n\
\20\n\
\15\n\
\25\n\
\47\n\
\40\n\
\62\n\
\55\n\
\65\n\
\95\n\
\102\n\
\117\n\
\150\n\
\182\n\
\127\n\
\219\n\
\299\n\
\277\n\
\309\n\
\576"

spec :: Spec
spec = do
  describe "Check if valid on single combination" $ do
    it "basic" $ do
      isValidCombination 26 [1..25] `shouldBe` True
      isValidCombination 50 [1..25] `shouldBe` False
      isValidCombination 50 [50] `shouldBe` False
      isValidCombination 50 [] `shouldBe` False
  describe "Check if valid" $ do
    it "basic" $ do
      isValidCombination 40 [35, 20, 15, 25, 47] `shouldBe` True
      checkRollingWindow 5 (parseDay9 testInput) `shouldBe` Just 127

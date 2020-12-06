module CustomsSpec where

import           Customs
import           Test.Hspec

main :: IO ()
main = hspec spec

testInput :: String
testInput = "abc\n\
\\n\
\a\n\
\b\n\
\c\n\
\\n\
\ab\n\
\ac\n\
\\n\
\a\n\
\a\n\
\a\n\
\a\n\
\\n\
\b"

spec :: Spec
spec = do
  describe "Count Yes" $ do
    it "Anyone" $ do
      countYesAnyone (readCustomsGroups testInput) `shouldBe` [3, 3, 3, 1, 1]
    it "Everyone" $ do
      countYesEveryone (readCustomsGroups testInput) `shouldBe` [3, 0, 1, 1, 1]

module PasswordsManagementSpec where 

import Test.Hspec
import PasswordsManagement

main :: IO ()
main = hspec spec

testInput :: [String]
testInput = ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]

spec :: Spec
spec = do
  describe "Validate one password" $ do
    it "should parse a rule" $ do
      parseRule (head testInput) `shouldBe` PasswordRule { minCount=1, maxCount=3, letter="a", password="abcde" }

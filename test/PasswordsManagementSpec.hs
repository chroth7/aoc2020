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
    it "validates one using Toboggan Rule" $ do
      (validateRuleToboggan $ parseRule (head testInput)) `shouldBe` True
    it "validates one using Toboggan Rule" $ do
      (validateRuleSanta $ parseRule (head testInput)) `shouldBe` True
    it "validates many" $ do
      map (validatePasswordString validateRuleToboggan) testInput `shouldBe` [True, False, True]
      map (validatePasswordString validateRuleSanta) testInput `shouldBe` [True, False, False]
    it "also counts" $ do
      countValidPasswordsToboggan testInput `shouldBe` 2
      countValidPasswordsSanta testInput `shouldBe` 1

  describe "SafeBangBang" $ do
    it "works as expected" $ do
      safeBangBang 4 [0..10] `shouldBe` Just 4
      safeBangBang 40 [0..10] `shouldBe` Nothing
      safeBangBang 40 ([] :: [Int]) `shouldBe` Nothing

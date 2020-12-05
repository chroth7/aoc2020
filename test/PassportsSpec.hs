module PassportsSpec where

import           Passports
import           Test.Hspec

main :: IO ()
main = hspec spec

testInput :: String
testInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
\byr:1937 iyr:2017 cid:147 hgt:183cm\n\
  \\n\
  \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
  \hcl:#cfa07d byr:1929\n\
  \\n\
  \hcl:#ae17e1 iyr:2013\n\
  \eyr:2024\n\
  \ecl:brn pid:760753108 byr:1931\n\
  \hgt:179cm\n\
  \\n\
  \hcl:#cfa07d eyr:2025 pid:166559648\n\
  \iyr:2011 ecl:brn hgt:59in"

spec :: Spec
spec = do
  describe "Read inputs" $ do
    it "Reads inputs (check length as we are lazy)" $ do
      length (readPassports testInput) `shouldBe` 4
    it "Parses PassportString" $ do
      stringToPassportCandidate "a:123 b:3e21 foo:bar" `shouldBe` [("a", "123"), ("b", "3e21"), ("foo", "bar")]
    it "validates Passports" $ do
      validatePassportEasy [("a", "123"), ("b", "3e21"), ("foo", "bar")] `shouldBe` False
      validatePassportEasy [("byr", "123"),("iyr", "123"), ("eyr", "123"), ("hgt", "123"), ("hcl", "123"), ("ecl", "123"), ("pid", "123")]
        `shouldBe` True
    it "validates multiple passports" $ do
      validatePassportsEasy (readPassportsToCandidates testInput) `shouldBe` [True, False, True, False]

  describe "Validation Rules etc." $ do
    it "Can deal with heights" $ do
      isValidHeight "170cm" `shouldBe` True
      isValidHeight "120cm" `shouldBe` False
      isValidHeight "70in" `shouldBe` True
      isValidHeight "170in" `shouldBe` False

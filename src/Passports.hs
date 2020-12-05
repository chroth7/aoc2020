module Passports
  ( readPassports
  , readPassportsToCandidates
  , stringToPassportCandidate
  , validatePassport
  , validatePassportEasy
  , validatePassports
  , validatePassportsEasy
  , isValidHeight
  ) where

import           Data.Bifunctor
import           Data.Char
import           Data.List
import           Data.Maybe
import           Text.Regex.Posix

readPassports :: String -> [String]
readPassports io = parsePassportLines "" rawLines
  where rawLines = lines io

parsePassportLines :: String -> [String] -> [String]
parsePassportLines s [] = [s]
parsePassportLines acc (x: xs)
  | x == "" = acc: parsePassportLines "" xs
  | otherwise = parsePassportLines (acc ++ space ++ x) xs
  where space = if null acc then "" else " "

readPassportsToCandidates :: String -> [PassportCandidate]
readPassportsToCandidates = map stringToPassportCandidate . readPassports

type KvPair = (String, String)
type PassportCandidate = [KvPair]

stringToPassportCandidate :: String -> PassportCandidate
stringToPassportCandidate input = map kvPair $ words input
  where kvPair = second (drop 1) . span (/= ':')

validatePassportsEasy :: [PassportCandidate] -> [Bool]
validatePassportsEasy = map validatePassportEasy

validatePassports :: [PassportCandidate] -> [Bool]
validatePassports = map validatePassport

-- RULES

validationRulesEasy :: [PassportCandidate -> Bool]
validationRulesEasy = [validatePassportKeys]

validationRules :: [PassportCandidate -> Bool]
validationRules = [validatePassportKeys, validateByr, validateIyr, validateEyr, validateHgt, validateHcl, validateEcl, validatePid]

validatePassportEasy :: PassportCandidate -> Bool
validatePassportEasy candidate = and $ validationRulesEasy <*> pure candidate

validatePassport :: PassportCandidate -> Bool
validatePassport candidate = and $ validationRules <*> pure candidate

expectedKeys :: [String]
expectedKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- cid optional

validatePassportKeys :: PassportCandidate -> Bool
validatePassportKeys candidate = all (`elem` passportKeys) expectedKeys
  where passportKeys = map fst candidate

isYear :: String -> Bool
isYear = (=~ "^[0-9]{4}$")

isHcl :: String -> Bool
isHcl = (=~ "^#[0-9a-f]{6}$")

isPid :: String -> Bool
isPid = (=~ "^[0-9]{9}$")

isEcl :: String -> Bool
isEcl s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isHeight :: String -> Bool
isHeight = (=~ "^[0-9]{2,3}(cm|in)$")

isValidHeight :: String -> Bool
isValidHeight str = let
  value = read $ takeWhile isDigit str
  unit = dropWhile isDigit str
  in
    if unit == "cm"
      then value >= 150 && value <= 193
      else value >= 59 && value <= 76

retrieveValue :: String -> PassportCandidate -> String
retrieveValue key = snd . fromJust . find (\x -> fst x == key)

type ValidationRule = PassportCandidate -> Bool

validateByr :: ValidationRule
validateByr candidate = isYear byr
  && read byr >= 1920
  && read byr <= 2002
  where byr = retrieveValue "byr" candidate

validateIyr :: ValidationRule
validateIyr candidate = isYear iyr
  && read iyr >= 2010
  && read iyr <= 2020
  where iyr = retrieveValue "iyr" candidate

validateEyr :: ValidationRule
validateEyr candidate = isYear eyr
  && read eyr >= 2020
  && read eyr <= 2030
  where eyr = retrieveValue "eyr" candidate

validateHgt :: ValidationRule
validateHgt candidate = isHeight hgt && isValidHeight hgt
  where hgt = retrieveValue "hgt" candidate

validateHcl :: ValidationRule
validateHcl candidate = isHcl hcl
  where hcl = retrieveValue "hcl" candidate

validateEcl :: ValidationRule
validateEcl candidate = isEcl ecl
  where ecl = retrieveValue "ecl" candidate

validatePid :: ValidationRule
validatePid candidate = isPid pid
  where pid = retrieveValue "pid" candidate

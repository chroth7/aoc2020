module Passports
  ( readPassports
  , readPassportsToCandidates
  , stringToPassportCandidate
  , validatePassport
  , validatePassports
  ) where

import           Data.Bifunctor

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

expectedKeys :: [String]
expectedKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- cid optional

validatePassport :: PassportCandidate -> Bool
validatePassport candidate = all (`elem` passportKeys) expectedKeys
  where passportKeys = map fst candidate

validatePassports :: [PassportCandidate] -> [Bool]
validatePassports = map validatePassport



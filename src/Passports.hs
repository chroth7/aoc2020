module Passports
  ( readPassports
  ) where

readPassports :: String -> [String]
readPassports io = parsePassportLines "" rawLines
  where rawLines = lines io

parsePassportLines :: String -> [String] -> [String]
parsePassportLines s [] = [s]
parsePassportLines acc (x: xs)
  | x == "" = (acc: parsePassportLines "" xs)
  | otherwise = parsePassportLines (acc ++ space ++ x) xs
  where space = if length acc == 0 then "" else " "

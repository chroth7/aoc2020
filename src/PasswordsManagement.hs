module PasswordsManagement 
  ( PasswordRule(..)
  , parseRule
  , validateRule
  , validatePasswordString
  , countValidPasswords
  ) where

import qualified Data.Text as T

data PasswordRule = PasswordRule { minCount :: Int
                                 , maxCount :: Int
                                 , letter :: String
                                 , password :: String
                                 } deriving (Show, Eq)

parseToInt :: T.Text -> Int
parseToInt t = (read $ T.unpack t) :: Int

parseRule :: String -> PasswordRule
parseRule input = let packedInput = T.pack input
  in 
    let (min:restMin) = T.splitOn (T.pack "-") packedInput
        (max:restMax) = T.splitOn (T.pack " ") (head restMin)
        [letter, password] = restMax
    in PasswordRule 
      { minCount=parseToInt min
      , maxCount=parseToInt max
      , letter=init $ T.unpack letter
      , password=T.unpack password}

countLettersInString :: Char -> String -> Int
countLettersInString c str = length $ filter (== c) str

validateRule :: PasswordRule -> Bool
validateRule rule = count <= maxCount rule && count >= minCount rule
  where count = countLettersInString (head $ letter rule) $ password rule

validatePasswordString :: String -> Bool
validatePasswordString = validateRule . parseRule

countValidPasswords :: [String] -> Int
countValidPasswords inputs = length $ filter (==True) $ map validatePasswordString inputs

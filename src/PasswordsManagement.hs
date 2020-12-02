module PasswordsManagement 
  ( PasswordRule(..)
  , parseRule
  , validateRuleToboggan
  , validateRuleSanta
  , validatePasswordString
  , countValidPasswordsToboggan
  , countValidPasswordsSanta
  , safeBangBang
  ) where

import qualified Data.Text as T
import Data.Maybe (fromJust)

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

type VerificationRule = PasswordRule -> Bool

validateRuleToboggan :: VerificationRule
validateRuleToboggan rule = count <= maxCount rule && count >= minCount rule
  where count = countLettersInString (head $ letter rule) $ password rule

safeBangBang :: Int -> [a] -> Maybe a
safeBangBang index list = if index > length list - 1 then Nothing else Just (list !! index)

matchCharacter :: Eq a => a -> Int -> [a] -> Bool
-- matchCharacter s index list = isJust bb && (fromJust bb == s)
matchCharacter s index list = bb == Just s
  where bb = safeBangBang (index - 1) list

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

validateRuleSanta :: VerificationRule
validateRuleSanta rule = matchFirst `xor` matchSecond
  where matchFirst = matchCharacter (head $ letter rule) (minCount rule) (password rule)
        matchSecond = matchCharacter (head $ letter rule) (maxCount rule) (password rule)

validatePasswordString :: VerificationRule -> String -> Bool
validatePasswordString rule = rule . parseRule

countValidPasswordsToboggan :: [String] -> Int
countValidPasswordsToboggan inputs = length $ filter (==True) $ map (validatePasswordString validateRuleToboggan) inputs

countValidPasswordsSanta :: [String] -> Int
countValidPasswordsSanta inputs = length $ filter (==True) $ map (validatePasswordString validateRuleSanta) inputs

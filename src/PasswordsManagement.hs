module PasswordsManagement where

import qualified Data.Text as T

data PasswordRule = PasswordRule { minCount :: Int
                                 , maxCount :: Int
                                 , letter :: String
                                 , password :: String
                                 } deriving (Show, Eq)

parseRule :: String -> PasswordRule
parseRule input = let packedInput = T.pack input
  in 
    let (min:restMin) = T.splitOn (T.pack "-") packedInput
      in 
        let (max:restMax) = T.splitOn (T.pack " ") (head restMin)
          in 
            let [letter, password] = restMax
              in PasswordRule 
                { minCount=(read $ T.unpack min) :: Int
                , maxCount=(read $ T.unpack max) :: Int
                , letter=init $ T.unpack letter
                , password=T.unpack password}


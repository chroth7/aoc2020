import AddXY
import Test.QuickCheck

prop_addsNumbers :: Int -> Int -> Bool
prop_addsNumbers x y = addXY x y == x + y

main :: IO ()
main = do
  quickCheck prop_addsNumbers
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_addsNumbers
  putStrLn "DONE"



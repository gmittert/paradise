module Lib.Format where

-- | Create a comma separated list
-- e.g. commaList [1,2,3,4] = "1,2,3,4"
commaList :: Show a => [a] -> String
commaList = delimWith ","

-- | Create a semicolon separated list
-- e.g. semiList [1,2,3,4] = "1;2;3;4"
semiList :: Show a => [a] -> String
semiList = delimWith ";"

delimWith :: Show a => String -> [a] -> String
delimWith _ [] = ""
delimWith s (x:xs) = show x ++ concatMap (\x -> s ++ show x) xs

commaListS :: [String] -> String
commaListS = delimWithS ","

semiListS :: [String] -> String
semiListS = delimWithS ";"

delimWithS :: String -> [String] -> String
delimWithS _ [] = ""
delimWithS s (x:xs) = x ++ concatMap (\x -> s ++ x) xs

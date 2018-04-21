module Lib.Format where

commaList :: Show a => [a] -> String
commaList = delimWith ","

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

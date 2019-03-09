module Helpers
    ( stringToIntList
    ) where

_toInt :: Char -> Int
_toInt x = case x of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9

_stringToIntList :: [Char] -> [Int]
_stringToIntList [] = []
_stringToIntList (x:xs) = (_toInt x):(_stringToIntList xs)

stringToIntList :: [Char] -> Maybe [Int]
stringToIntList li  | (all (`elem` "0123456789") li) = Just (_stringToIntList li)
                    | otherwise = Nothing

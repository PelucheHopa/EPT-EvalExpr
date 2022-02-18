module RightSide where

import Utility
import Parenthesis
import Operation

{- ISOLE LA PARENTHESE A CALCULER -}
saveRightSide :: String -> String -> String
saveRightSide [x] stock = (myConcat stock (deleteExtra (show x :: String)))
saveRightSide (x:xs) stock = saveRightSide xs (myConcat stock (deleteExtra (show x :: String)))

{- ISOLE LA PARENTHESE A CALCULER -}
externRightSideTwo :: String -> Int -> Int -> String
externRightSideTwo [x] _ _
    | x /= ')' = ""
    | otherwise = saveRightSide [x] ""
externRightSideTwo (x:xs) nbr compt
    | x /= ')' = externRightSideTwo xs nbr compt
    | otherwise = saveRightSide (x:xs) ""

{- PREND LA ZONE GAUCHE-}
externRightSide :: String -> Int -> Int -> String
externRightSide [] nbr compt = ""
externRightSide [x] nbr compt = ""
externRightSide (x:xs) nbr compt
    | x /= '(' && compt > 0 = externRightSide xs nbr compt
    | x == '(' && compt > 1 = externRightSide xs nbr (compt - 1)
    | otherwise = externRightSideTwo xs nbr nbr
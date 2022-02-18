module MiddleSide where
    
import Utility
import Parenthesis
import Operation 

{- ISOLE LA PARENTHESE A CALCULER -}
saveMiddleSide :: String -> String -> Int -> Int -> String
saveMiddleSide [x] stock nbr compt
    | x /= ')' = (myConcat stock (deleteExtra (show x :: String)))
    | otherwise = stock
saveMiddleSide (x:xs) _ 0 _ = (x:xs)
saveMiddleSide (x:xs) stock nbr compt
    | x /= ')' = saveMiddleSide xs (myConcat stock (deleteExtra (show x :: String))) nbr compt
    | otherwise = stock

{- PREND LA ZONE GAUCHE-}
externMiddleSide :: String -> Int -> Int -> String
externMiddleSide [] nbr compt = ""
externMiddleSide [x] nbr compt = ""
externMiddleSide (x:xs) 0 compt = saveMiddleSide (x:xs) "" 0 compt
externMiddleSide (x:xs) nbr compt
    | x /= '(' && compt > 0 = externMiddleSide xs nbr compt
    | x == '(' && compt > 1 = externMiddleSide xs nbr (compt - 1)
    | otherwise = saveMiddleSide xs "" nbr nbr
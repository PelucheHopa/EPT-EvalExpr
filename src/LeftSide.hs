module LeftSide where
    
import Utility
import Parenthesis
import Operation 

{- PARCOURS LA STRING EN SUPPRIMANT AU FUR A MESURE JUSQU'A LA ZONE VOULUE -}
externLeftSide :: String -> String -> Int -> Int -> String
externLeftSide [] stock nbr compt = ""
externLeftSide [x] stock nbr compt = ""
externLeftSide (x:xs) stock 0 _ = ""
externLeftSide (x:xs) stock nbr compt
    | x /= '(' && compt > 0 = externLeftSide  xs (myConcat stock (deleteExtra (show x :: String))) nbr compt
    | x == '(' && compt > 1 = externLeftSide  xs (myConcat stock (deleteExtra (show x :: String))) nbr (compt - 1)
    | otherwise = (myConcat stock (deleteExtra (show x :: String)))
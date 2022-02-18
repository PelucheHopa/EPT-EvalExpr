module Parenthesis where
import System.Environment ( getArgs )
import System.IO
import System.Exit
import Utility

checkParenthesisLeft :: String -> Int -> Int
checkParenthesisLeft [x] nbr
    | x == '(' = nbr + 1
    | otherwise = nbr
checkParenthesisLeft  (x:xs) nbr
    | x == '(' = checkParenthesisLeft  xs (nbr + 1)
    | otherwise = checkParenthesisLeft  xs nbr

checkParenthesisRight :: String -> Int -> Int
checkParenthesisRight [x] nbr
    | x == ')' = nbr + 1
    | otherwise = nbr
checkParenthesisRight  (x:xs) nbr
    | x == ')' = checkParenthesisRight  xs (nbr + 1)
    | otherwise = checkParenthesisRight  xs nbr

checkParenthesis :: String -> IO ()
checkParenthesis x
    | checkParenthesisLeft x 0 == checkParenthesisRight x 0 = return()
    | otherwise = exitWith (ExitFailure 84)

{- PREND LA ZONE GAUCHE-}
removeParenthesisLeft :: String -> String -> Int -> Int -> String
removeParenthesisLeft [] stock nbr compt = ""
removeParenthesisLeft [x] stock nbr compt
    | x == '(' && compt > 1 = (myConcat stock [x])
    | x == '(' && compt == 1 = stock
    | x /= '(' && compt > 0 = (myConcat stock [x])
    | otherwise = (myConcat stock [x])
removeParenthesisLeft (x:xs) stock nbr compt
    | x == '(' && compt > 1 = removeParenthesisLeft xs (myConcat stock [x]) nbr (compt - 1)
    | x == '(' && compt == 1 = removeParenthesisLeft xs stock nbr compt
    | x /= '(' && compt > 0 = removeParenthesisLeft xs (myConcat stock [x]) nbr compt
    | otherwise = removeParenthesisLeft xs (myConcat stock [x]) nbr compt

{- PREND LA ZONE GAUCHE-}
removeParenthesisRight :: String -> String -> Int -> Int -> String
removeParenthesisRight [] stock nbr compt = ""
removeParenthesisRight [x] stock nbr compt
    | x == ')' && compt > 1 = (myConcat stock [x])
    | x == ')' && compt == 1 = stock
    | x /= ')' && compt > 0 = (myConcat stock [x])
    | otherwise = (myConcat stock [x])
removeParenthesisRight (x:xs) stock nbr compt
    | x == ')' && compt > 1 = removeParenthesisRight xs (myConcat stock [x]) nbr (compt - 1)
    | x == ')' && compt == 1 = removeParenthesisRight xs stock nbr compt
    | x /= ')' && compt > 0 = removeParenthesisRight xs (myConcat stock [x]) nbr compt
    | otherwise = removeParenthesisRight xs (myConcat stock [x]) nbr compt
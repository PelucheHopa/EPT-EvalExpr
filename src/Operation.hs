module Operation where

import Utility
import Parenthesis

calcul :: Float -> Float -> Int -> Float
calcul nbr1 nbr2 operation
    | nbr1 < 0 && nbr2 < 0 && operation > 2 = calcul (nbr1 - nbr1 - nbr1) (nbr2 - nbr2 - nbr2) operation
    | operation == 4 = nbr1 / nbr2
    | operation == 3 = nbr1 * nbr2
    | operation == 2 = nbr1 - nbr2
    | operation == 1 = nbr1 + nbr2
    | otherwise = 0

{-TROUVE L'OPERATION PRIORITAIRE ET SON TYPE-}
detectOperationPos :: String -> Int -> Int -> Int -> Int -> Int
detectOperationPos [x] oper pos r_op r_pos = r_pos
detectOperationPos (x:xs) oper pos r_op r_pos
    | pos == 1 = detectOperationPos xs oper (pos + 1) r_op r_pos
    | x == '/' && (r_op  < 3) = pos
    | x == '*' && (r_op  < 3) = pos
    | x == '-' && (r_op  == 0) = (detectOperationPos xs oper (pos + 1) 2 pos)
    | x == '+' && (r_op  == 0) = (detectOperationPos xs oper (pos + 1) 1 pos)
    | otherwise = detectOperationPos xs oper (pos + 1) r_op r_pos

{-TROUVE L'OPERATION PRIORITAIRE ET SON TYPE-}
detectOperation :: String -> Int -> Int -> Int -> Int
detectOperation [x] oper pos r_op = r_op
detectOperation (x:xs) oper pos r_op
    | pos == 1 = detectOperation xs oper (pos + 1) r_op
    | x == '/' && (r_op  < 3) = 4
    | x == '*' && (r_op  < 3) = 3
    | x == '-'&& (r_op  == 0) = (detectOperation xs oper (pos + 1) 2)
    | x == '+'&& (r_op  == 0) = (detectOperation xs oper (pos + 1) 1)
    | otherwise = detectOperation xs oper (pos + 1) r_op 

checkOperation :: String -> String -> String -> String
checkOperation [x] stock comp = myConcat (myConcat stock comp) [x]
checkOperation (x:xs) stock [] = checkOperation xs (myConcat stock "") [x]
checkOperation (x:xs) stock [y]
    | x == '-' && y == '-' = checkOperation xs (myConcat stock "+") []
    | x == 't' && y == '\\' = checkOperation xs stock []
    | y == '\t' = checkOperation xs stock [x]
    | y == ' ' = checkOperation xs stock [x]
    | otherwise = checkOperation xs (myConcat stock [y]) [x]
checkOperationStepOne :: String -> String -> String
checkOperationStepOne [x] stock = myConcat stock [x]
checkOperationStepOne (x:xs) stock = checkOperation xs "" [x]
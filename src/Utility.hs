module Utility where 

{-CONCAT EVERYTHING-}
myConcat :: [a] -> [a] -> [a]
myConcat xs ys = foldr (:) ys xs

{-RETOURNE UNE STRING-}
reverseString :: [Char] -> [Char]
reverseString [] = []
reverseString (x:xs) = reverseString xs ++ [x]

{-CONVERT UNE STRING EN INT-}
stringToInt :: String -> Int
stringToInt str = (read str :: Int)

{-CONVERT UN INT EN STRING-}
intToString :: Int -> String
intToString nbr = (show nbr :: String)

{-RENVOIE LA TAILLE DE LA STRING-}
sizeCompt :: String -> Int -> Int
sizeCompt [x] nbr = nbr + 1
sizeCompt (x:xs) nbr = sizeCompt xs (nbr + 1)

{- SUPPRIME LES  ' ' LORS DU CAST CHAR -> STRING -}
deleteExtra :: String -> String
deleteExtra (x:xs:xt) = [xs]

{-PREND LE COTÉ DROITE DE L'OPERATION-}
takeSide :: String -> String -> Int -> Int -> String
takeSide [x] stock nbr nega = (myConcat stock [x])
takeSide (x:xs) stock nbr nega
    | nbr == 0 && x == '-' = takeSide xs (myConcat stock [x]) (nbr - 1) nega
    | nbr <= 0 && x >= '0' && x <= '9' = takeSide xs (myConcat stock [x]) (nbr - 1) nega
    | nbr <= 0 && x == '.' = takeSide xs (myConcat stock [x]) (nbr - 1) nega
    | nbr <= 0 = stock
    | otherwise = takeSide xs stock (nbr - 1) 0

{-PREND LE COTÉ DROITE DE L'OPERATION-}
takeInternalSide :: String -> String -> Int -> Int -> String
takeInternalSide [x] stock nbr nega
    | nbr == 0 && x == '-' = stock
    | nbr <= 0 && x >= '0' && x <= '9' = stock
    | nbr <= 0 && x == '.' = stock
    | nbr <= 0 = stock
    | otherwise = stock
takeInternalSide (x:xs) stock nbr valid
    | nbr == 0 && x == '-' = takeInternalSide xs stock (nbr - 1) valid
    | nbr <= 0 && x >= '0' && x <= '9' = takeInternalSide xs stock (nbr - 1) valid
    | nbr <= 0 && x == '.' = takeInternalSide xs stock (nbr - 1) valid
    | nbr <= 0 = (x:xs)
    | otherwise = takeInternalSide xs stock (nbr - 1) 0

{-PREND LE COTÉ DROITE DE L'OPERATION-}
takeReverseSide :: String -> String -> Int -> Int -> String
takeReverseSide [x] stock nbr nega
    | nbr <= 0 && x == '-' = (myConcat stock [x])
    | nbr <= 0 && x >= '0' && x <= '9' = (myConcat stock [x])
    | nbr <= 0 && x == '.' = (myConcat stock [x])
    | nbr <= 0 = stock
    | otherwise = stock
takeReverseSide (x:xs) stock nbr nega
    | nbr <= 0 && x == '-' = (myConcat stock [x])
    | nbr <= 0 && x >= '0' && x <= '9' = takeReverseSide xs (myConcat stock [x]) (nbr - 1) nega
    | nbr <= 0 && x == '.' = takeReverseSide xs (myConcat stock [x]) (nbr - 1) nega
    | nbr <= 0 = stock
    | otherwise = takeReverseSide xs stock (nbr - 1) nega
    
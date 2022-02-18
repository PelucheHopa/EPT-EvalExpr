module Main where
import System.Environment ( getArgs )
import System.IO
import System.Exit
import Utility
import Parenthesis
import Operation
import LeftSide
import RightSide
import MiddleSide

cut :: String -> String
cut x = (externMiddleSide x (checkParenthesisLeft x 0) (checkParenthesisRight x 0))

checkSyntax :: String -> String -> IO ()
checkSyntax [] _ = exitWith (ExitFailure 84)
checkSyntax (x:xs) [] = checkSyntax xs [x]
checkSyntax [x] [y]
    | x >= '*' && x <= '/' = exitWith (ExitFailure 84)
    | x < ' ' || x >= '{' = exitWith (ExitFailure 84)
    | x == '\\' = exitWith (ExitFailure 84)
    | x >= 'a' && x <= 'z' = exitWith (ExitFailure 84)
    | x >= 'A' && x <= 'Z' = exitWith (ExitFailure 84)
    | x == '*' && y == '*' = exitWith (ExitFailure 84)
    | x == '(' && y == ')' = exitWith (ExitFailure 84)
    | x == ')' && y == '(' = exitWith (ExitFailure 84)
    | x == '/' && y == '/' = exitWith (ExitFailure 84)
    | otherwise = return ()
checkSyntax (x:xs) [y]
    | x < ' ' || x >= '{' = exitWith (ExitFailure 84)
    | x == '\\' = exitWith (ExitFailure 84)
    | x >= 'a' && x <= 'z' = exitWith (ExitFailure 84)
    | x >= 'A' && x <= 'Z' = exitWith (ExitFailure 84)
    | x == '*' && y == '*' = exitWith (ExitFailure 84)
    | x == '/' && y == '*' = exitWith (ExitFailure 84)
    | x == '*' && y == '/' = exitWith (ExitFailure 84)
    | x == '/' && y == '-' = exitWith (ExitFailure 84)
    | x == '(' && y == ')' = exitWith (ExitFailure 84)
    | x == ')' && y == '(' = exitWith (ExitFailure 84)
    | x == '/' && y == '/' = exitWith (ExitFailure 84)
    | otherwise = checkSyntax xs [x]

launch :: String -> String
launch x
    | (detectOperation (cut x) 0 1 0 ) == 0 && (checkParenthesisRight x 0) == 0 = x
    | (detectOperation (cut x) 0 1 0 ) == 0 && (checkParenthesisRight x 0) /= 0 = launch (checkOperationStepOne (removeParenthesisRight (removeParenthesisLeft x "" (checkParenthesisLeft x 0) (checkParenthesisLeft x 0)) "" (checkParenthesisRight x 0) (checkParenthesisRight x 0)) "")
    | otherwise = launch (checkOperationStepOne (myConcat (externLeftSide x "" (checkParenthesisRight x 0) (checkParenthesisRight x 0)) (myConcat (myConcat (reverseString ((takeInternalSide (reverseString (cut x)) "" ((sizeCompt (cut x) 1) - (detectOperationPos (cut x) 0 1 0 0)) 0))) (myConcat (show (calcul (read (reverseString (takeReverseSide (reverseString (cut x)) "" ((sizeCompt (cut x) 1) - (detectOperationPos (cut x) 0 1 0 0)) 0)) :: Float) (read (takeSide (cut x) "" (detectOperationPos (cut x) 0 1 0 0) 0) :: Float) (detectOperation (cut x) 0 1 0 )) :: String) (takeInternalSide (cut x) "" (detectOperationPos (cut x) 0 1 0 0) 0))) (externRightSide x (checkParenthesisRight x 0) (checkParenthesisRight x 0)))) "")

main :: IO ()
main = do
    (x:xs) <- getArgs
    let checking = (checkOperationStepOne x "")
    checkSyntax checking ""
    checkParenthesis checking
    putStrLn (launch checking)
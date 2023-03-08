{-
-- EPITECH PROJECT, 2023
-- main.hs
-- File description:
-- Wolfram
-}
import System.Environment (getArgs)
import System.Exit
import Prelude hiding (lines)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

data Conf = Conf {
    rule :: Int,
    start :: Int,
    lines :: Int,
    wd :: Int,
    move :: Int,
    bR :: [Int]
}

generateFirstLine:: Conf -> String
generateFirstLine c =
    let first = replicate ((wd c `div` 2) + move c) ' '
    in first ++ "*" ++replicate ((wd c `div` 2) - move c) ' '

defaultConf :: Conf
defaultConf = Conf {
    rule = -1,
    start = 0,
    lines = -1,
    wd = 80,
    move = 0,
    bR = [0]
}

toBinary :: Int -> String
toBinary n =
    let binaryVal = showIntAtBase 2 intToDigit n ""
        padding = replicate (8 - length binaryVal) '0'
    in (padding ++ binaryVal)

getOpts :: Conf -> [String] -> Maybe Conf
getOpts c [] = Just c
getOpts c ("--rule":s) =
    case s of
        [] -> Nothing
        _ ->  getOpts (c {rule = read (head s)}) (tail s)
getOpts c ("--start":s) =
    case s of
        [] -> Nothing
        _ -> getOpts (c {start = read (head s)}) (tail s)
getOpts c ("--lines":s) =
    case s of
        [] -> Nothing
        _ -> getOpts (c {lines = read (head s)}) (tail s)
getOpts c ("--window":s) =
    case s of
        [] -> Nothing
        _  -> getOpts (c {wd= read (head s)}) (tail s)
getOpts c ("--move":s) =
    case s of
        [] -> Nothing
        _ -> getOpts (c {move = read (head s)}) (tail s)
getOpts _ _ = Nothing

errorHandling :: Maybe Conf -> IO()
errorHandling Nothing = putStrLn "Error: Args Err" >> exitWith(ExitFailure 84)
errorHandling (Just c) =
    if ((rule c) < 0 || (rule c) > 255) then
        putStrLn "Error: Wrong args" >> exitWith(ExitFailure 84)
    else
        return ()

getLineRule :: Char  -> Char -> Char -> Conf -> Int
getLineRule '*' '*' '*' c =
    (bR c) !! 0
getLineRule '*' '*' ' ' c =
    (bR c) !! 1
getLineRule '*' ' ' '*' c =
    (bR c) !! 2
getLineRule '*' ' ' ' ' c =
    (bR c) !! 3
getLineRule ' ' '*' '*' c =
    (bR c) !! 4
getLineRule ' ' '*' ' ' c =
    (bR c) !! 5
getLineRule ' ' ' ' '*' c =
    (bR c) !! 6
getLineRule ' ' ' ' ' ' c =
    (bR c) !! 7
getLineRule _ _ _ _ =
    0
generate :: Conf -> String -> Char -> String
generate  (Conf _ _ _ _ _ _) [_] _ =  " "
generate (Conf _ _ _ _ _ _) [] _ = ""
generate c [x, y] ch =
    let binaryRule = getLineRule x y ' ' c
    in if binaryRule == 1 then ch : generate c [] '*'
    else ch : generate c []' '
generate c (x : y : z : xs) ch =
    let binaryRule = getLineRule x y z c
    in if binaryRule == 1 then ch : generate c (y : z : xs) '*'
    else ch : generate c (y : z : xs) ' '

printWithoutSpaces :: Int -> String -> Int -> IO()
printWithoutSpaces runs str wind = putStrLn (take wind (drop runs str))

loop :: Conf -> String -> Int -> Int -> IO()
loop _ _ 0 _ = return ()
loop c str i run =
    if (start c) <= 0 then
        printWithoutSpaces run str (wd c) >>
        loop c (" " ++ (generate c str ' ') ++ " ") (i - 1) (run + 1)
    else
        loop (dcS c)  (" " ++ (generate (dcS c) str ' ') ++ " ") (i) (run + 1)

dcS :: Conf -> Conf
dcS c =
    if (start c) <= 0 then
        c
    else
        c {start = (start c) - 1}

addBinaryToConf :: Conf -> Conf

addBinaryToConf c = c {bR = map (read . (:"")) (toBinary (rule c)) :: [Int]}

main :: IO()
main = do
    args <- getArgs
    let confValue = getOpts defaultConf args
    errorHandling confValue
    case confValue of
        Just val ->
                let c = addBinaryToConf val
                in loop c (generateFirstLine c) (lines c) 0
        Nothing -> putStrLn "Error: Args Err" >> exitWith(ExitFailure 84)

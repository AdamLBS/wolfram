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
    move :: Int
}

generateFirstLine:: Conf -> String
generateFirstLine c =
    replicate (wd c `div` 2) ' ' ++ "*" ++ replicate ((wd c `div` 2) - 1) ' '

defaultConf :: Conf
defaultConf = Conf {
    rule = -1,
    start = 0,
    lines = -1,
    wd = 80,
    move = 0
}

toBinary :: Int -> String
toBinary n =
    let binaryVal = showIntAtBase 2 intToDigit n ""
        padding = replicate (8 - length binaryVal) '0'
    in padding ++ binaryVal

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
    if (rule c) == -1 then
        putStrLn "Error: Wrong args" >> exitWith(ExitFailure 84)
    else
        return ()

getLineRule :: Char  -> Char -> Char -> Conf -> Int
getLineRule '*' '*' '*' c =
    let binaryRule = toBinary (rule c)
    in read [binaryRule !! 0]
getLineRule '*' '*' ' ' c =
    let binaryRule = toBinary (rule c)
    in read [binaryRule !! 1]
getLineRule '*' ' ' '*' c =
    let binaryRule = toBinary (rule c)
    in read [binaryRule !! 2]
getLineRule '*' ' ' ' ' c =
    let binaryRule = toBinary (rule c)
    in read [binaryRule !! 3]
getLineRule ' ' '*' '*' c =
    let binaryRule = toBinary (rule c)
    in read [binaryRule !! 4]
getLineRule ' ' '*' ' ' c =
    let binaryRule = toBinary (rule c)
    in read [binaryRule !! 5]
getLineRule ' ' ' ' '*' c =
    let binaryRule = toBinary (rule c)
    in read [binaryRule !! 6]
getLineRule ' ' ' ' ' ' c =
    let binaryRule = toBinary (rule c)
    in read [binaryRule !! 7]
getLineRule _ _ _ _ =
    0
generateNextLine :: Conf -> String -> Char -> String
generateNextLine _ [] _ =  " "
generateNextLine c [x, y] _ =
    let binaryRule = getLineRule x y ' ' c
    in if binaryRule == 1 then "* " else "  "
generateNextLine c (x : y : z : xs) ch =
    let binaryRule = getLineRule x y z c
    in if binaryRule == 1 then ch : generateNextLine c (y : z : xs) '*'
    else ch : generateNextLine c (y : z : xs) ' '

wolframLoop :: Conf -> String -> Int -> IO()
wolframLoop _ _ 0 = return ()
wolframLoop c str i =
    if (start c) == 0 then
            putStrLn str >> wolframLoop c (generateNextLine c str ' ') (i - 1)
    else
        wolframLoop (dcS c) (generateNextLine (dcS c) str ' ') (i)

dcS :: Conf -> Conf
dcS c =
    if (start c) == 0 then
        c
    else
        c {start = (start c) - 1}
doMove :: Conf -> Conf
doMove c =
    if (move c) == 0 then
        c
    else
        c {wd = (wd c) + (move c)}

main :: IO()
main = do
    args <- getArgs
    let confValue = getOpts defaultConf args
    errorHandling confValue
    case confValue of
        Just val ->
                let c = doMove val
                in wolframLoop c (generateFirstLine c) (lines c)
        Nothing -> putStrLn "Error: Args Err" >> exitWith(ExitFailure 84)

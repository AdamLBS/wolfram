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
    rule :: Integer,
    start :: Integer,
    lines :: Maybe Integer,
    window :: Integer,
    move :: Maybe Integer
}

defaultConf :: Conf
defaultConf = Conf {
    rule = -1,
    start = 0,
    lines = Nothing,
    window = 80,
    move = Nothing
}

toBinary :: Integer -> String
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
        _ -> getOpts (c {lines = Just (read (head s))}) (tail s)
getOpts c ("--window":s) =
    case s of
        [] -> Nothing
        _  -> getOpts (c {window = read (head s)}) (tail s)
getOpts c ("--move":s) =
    case s of
        [] -> Nothing
        _ -> getOpts (c {move = Just (read (head s))}) (tail s)
getOpts c [] = Nothing

errorHandling :: Maybe Conf -> IO()
errorHandling Nothing = putStrLn "Error: Args Err" >> exitWith(ExitFailure 84)
errorHandling (Just c) =
    if (rule c) == -1 then
        putStrLn "Error: Wrong args" >> exitWith(ExitFailure 84)
    else
        return ()

main :: IO()
main = do
    args <- getArgs
    let confValue = getOpts defaultConf args
    errorHandling confValue
    case confValue of
        Just val ->
                let binaryRule = toBinary (rule val)
                in putStrLn ("Rule: " ++ show binaryRule)

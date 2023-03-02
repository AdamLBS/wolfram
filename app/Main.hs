{-
-- EPITECH PROJECT, 2023
-- main.hs
-- File description:
-- Wolfram
-}
import System.Environment (getArgs)
import System.Exit
import Prelude hiding (lines)
import Control.Monad

data Conf = Conf {
    rule :: Maybe Integer,
    start :: Integer,
    lines :: Maybe Integer,
    window :: Integer,
    move :: Maybe Integer
}

defaultConf :: Conf
defaultConf = Conf {
    rule = Nothing,
    start = 0,
    lines = Nothing,
    window = 80,
    move = Nothing
}

getOpts :: Conf -> [String] -> Maybe Conf
getOpts c [] = Just c
getOpts c ("--rule":s) =
    case s of
        [] -> Nothing
        _ ->  getOpts (c {rule = Just (read (head s))}) (tail s)
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


main :: IO()
main = do
    args <- getArgs
    let confValue = getOpts defaultConf args
    case confValue of
        Just val ->
            when (rule val == Nothing) $ do
                putStrLn "Error: Wrong args"
                exitWith(ExitFailure 84)
        Nothing -> putStrLn "Error: Wrong args" >> exitWith(ExitFailure 84)
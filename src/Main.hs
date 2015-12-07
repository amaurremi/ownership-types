module Main where

import Data.Map (toList)
import System.Environment
import System.IO

import Eval
import Lexer
import Parser
import TypeCheck

main = do
    args <- getArgs
    code <- readFile $ head args -- todo
    let result = do
        parsedResult <- parsed $ lexer code
        typeCheck parsedResult
    case result of
        Left error -> print error
        Right prog -> do
            putStrLn "Store contents:"
            putStrLn $ unlines $ map show $ toList $ eval prog
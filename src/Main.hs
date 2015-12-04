module Main where

import System.Environment
import System.IO

import Lexer
import Parser
import TypeCheck

main = do
    args <- getArgs
    code <- readFile $ head args -- todo
    case parsed (lexer code) of
        Left error -> do
                        putStrLn "type-check error:\n"
                        print error
        Right prog -> print $ typeCheck prog
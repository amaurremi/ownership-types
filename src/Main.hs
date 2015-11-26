module Main where

import System.Environment
import System.IO

import Lexer

main = do
    args <- getArgs
    code <- readFile $ head args -- todo
    print $ lexer code
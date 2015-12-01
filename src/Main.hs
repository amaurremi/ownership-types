module Main where

import System.Environment
import System.IO

import Parser

main = do
    args <- getArgs
    code <- readFile $ head args -- todo
    print $ parsed code
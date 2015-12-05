module Main where

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
        Right prog -> print prog
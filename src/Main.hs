module Main where

import System.Environment
import System.IO

import Lexer
import Parser
import TypeCheck

import Test.Simple
import Test.Car

main = do
    args <- getArgs
    code <- readFile $ head args -- todo
    case parsed (lexer code) of
        Left error   -> print error
        Right prog -> print $ typeCheck prog
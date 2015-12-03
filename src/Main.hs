module Main where

import System.Environment
import System.IO

import TypeCheck

import Test.Simple
import Test.Car

main = print $ typeCheck car
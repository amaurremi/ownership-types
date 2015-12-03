module Main where

import System.Environment
import System.IO

import TypeCheck

import Test.Simple

main = print $ typeCheck simple
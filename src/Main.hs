module Main where

import System.Environment
import System.IO

import TypeCheck

import Test.Car

main = let checked = typeCheck car
       in print "checked"
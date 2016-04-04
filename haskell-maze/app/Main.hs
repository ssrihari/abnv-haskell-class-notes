module Main where

import System.Random (newStdGen)

import Lib

main :: IO ()
main = do
  gen <- newStdGen
  putStr . renderMaze $ createMaze gen 10 10

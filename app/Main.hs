module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  filename:_ <- getArgs
  board <- getBoard filename
  putStrLn "Orginal board"
  print board
  putStrLn "\nsolving ...\n"
  print (sudoku board)

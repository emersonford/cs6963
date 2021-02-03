module Main where

import Sudoku (Slot, Board, newBoard, insertNums, solve)

main = interact main'

main' inp =
  let lns   = filter (not . null) $ map words $ lines inp
      mn    = map read $ head lns :: [Int]
      board = newBoard (head mn) (mn !! 1)
      nums  = tail lns
  in  case solve $ insertNums nums board of
        Just b  -> show b ++ "\n"
        Nothing -> "Unsolvable board!\n"

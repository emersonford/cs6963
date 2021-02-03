module Main where

import Sudoku (Slot, Board, newBoard, insertNums, solve, slots, possibleNums)
import Data.Set (empty)
import System.Random (mkStdGen, genWord32)
import System.Random.Shuffle (shuffle')

main = interact main'

main' inp =
  let lns       = filter (not . null) $ map words $ lines inp
      ln        = map read $ head lns :: [Int]
      m         = head ln
      n         = (ln !! 1)
      numRemove = (ln !! 2)
      seed      = (ln !! 3)
      board     = newBoard m n
      bsl       = slots board
      randGen   = mkStdGen seed
  in  case
        solve $ insertNums
          [map show $ shuffle' [1 .. m * n] (m * n) randGen]
          board { slots = shuffle' bsl (length bsl) randGen }
      of
        Just b ->
          map
              (\x -> if x == 'X' then '_' else x)
              ( show
                ( b
                  { slots = map (\x -> x { possibleNums = Left empty })
                                (take numRemove (slots b))
                    ++ drop numRemove (slots b)
                  }
                )
              )
            ++ "\n\nSolution:\n"
            ++ show b
            ++ "\n"

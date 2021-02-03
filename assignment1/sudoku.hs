module Sudoku (Slot, Board, slots, newBoard, insertNums, solve, possibleNums) where

import Data.Set (Set, delete, fromList, size, toList, empty)
import Data.List (sortOn)
import Data.Either (isLeft, isRight, fromLeft)
import Control.Applicative ((<|>))
import Debug.Trace (trace)

data Slot = Slot
    { row :: Int
    , col :: Int
    -- Left means slot has a number set, Right means a slot that has yet
    -- to be set.
    , possibleNums :: Either (Set Int) Int
    }

instance Show Slot where
  show (Slot r c pn) =
    case pn of
      Left l -> if size l == 0 then "X" else show (toList l)
      Right r -> show r

data Board = Board
    { m :: Int
    , n :: Int
    , slots :: [Slot]
    }

instance Show Board where
  show (Board m n sl) = concatMap (boardSlotToString m n) (sortOn row $ sortOn col sl)

boardSlotToString m n sl | c == 0 && r == 0         = show sl
                         | c == 0 && r `mod` n == 0 = "\n\n" ++ show sl
                         | c == 0                   = "\n" ++ show sl
                         | c `mod` m == 0           = "  " ++ show sl
                         | otherwise                = " " ++ show sl
 where
  r = row sl
  c = col sl


-- Functions for taking stdin and inserting the set numbers to the board.
iterSlot :: (Int, Int, Board) -> String -> (Int, Int, Board)
iterSlot (r, c, b) num =
  ( r
  , c + 1
  , if num == "_" || num == "." then b else setNumInBoard b r c (read num)
  )

iterOverRow :: (Int, Board) -> [String] -> (Int, Board)
iterOverRow (r, b) row = case foldl iterSlot (r, 0, b) row of
  (r, _, b) -> (r + 1, b)

insertNums :: [[String]] -> Board -> Board
insertNums rows b = case foldl iterOverRow (0, b) rows of
  (_, b) -> b

-- Location functions
-- Is a in the same square as b?
inSquare m n a brow bcol =
  let colStart = bcol - (bcol `mod` m)
      rowStart = brow - (brow `mod` n)
  in  (colStart <= col a)
      && (col a < colStart + m)
      && (rowStart <= row a)
      && (row a < rowStart + n)

inRow a brow = row a == brow
inCol a bcol = col a == bcol

-- Slot functions
removeNum n sl =
  let pn = possibleNums sl
  in  case pn of
        Left  l -> sl { possibleNums = Left (delete n l) }
        -- Left empty can occur if we're passed in an unsolvable board.
        Right r -> sl { possibleNums = if r == n then Left empty else Right r }

-- setNum should never be called on a set slot, else its a bug, so leave it undefined.
setNum n sl =
  either (const sl { possibleNums = Right n }) undefined (possibleNums sl)

-- Board functions
newBoard m n =
  let nums = [1 .. m * n]
  in  Board
        { m     = m
        , n     = n
        , slots = [ Slot
                      { row          = x - 1
                      , col          = y - 1
                      , possibleNums = Left (fromList nums)
                      }
                  | x <- nums
                  , y <- nums
                  ]
        }

-- Set a number to a slot in the board.
setNumInBoard :: Board -> Int -> Int -> Int -> Board
setNumInBoard b srow scol num = b
  { slots = map
    ( \x -> if row x == srow && col x == scol
      then setNum num x
      else if inSquare (m b) (n b) x srow scol || inRow x srow || inCol x scol
        then removeNum num x
        else x
    )
    (slots b)
  }

-- Get the slot with size 0 (impossible board)
-- OR get the slot with the smallest possibleNums Left (board still needs solving)
-- OR get a slot with a possibleNums Right (board has been solved)
getNextSlot :: [Slot] -> Slot
getNextSlot (x:xs) = hgetNextSlot x (filter (isLeft . possibleNums) xs)

hgetNextSlot :: Slot -> [Slot] -> Slot
hgetNextSlot x [] = x
hgetNextSlot x (xx:xs) | isRight pnx  = hgetNextSlot xx xs
                       | pnxs == 0    = x
                       | pnxxs < pnxs = hgetNextSlot xx xs
                       | otherwise    = hgetNextSlot x xs
 where
  pnx   = possibleNums x
  pnxs  = size (fromLeft undefined pnx)
  pnxx  = possibleNums xx
  pnxxs = size (fromLeft undefined pnxx)

-- Steps of solve:
-- 1. Get a slot from the board.
-- 2. If the slot is filled, return Just the board as it is solved.
--    If the slot has no possible numbers, return Nothing as this board is unsolvable.
-- 3. If the slot has possible numbers, generate a new board for every possible number of that board.
-- 4. Recurse on each new board created.
-- 5. Return using MonadPlus on the returned Maybes (analogous to OR short circuiting).
solve :: Board -> Maybe Board
-- solve b | trace ("solve: \n" ++ show b) False = undefined
solve b
  | isRight (possibleNums nextSlot) = Just b
  | nsSize == 0 = Nothing
  | otherwise = foldr
    ((<|>) . solve . setNumInBoard b (row nextSlot) (col nextSlot))
    Nothing
    (toList nsPn)
 where
  nextSlot = getNextSlot (slots b)
  nsPn     = fromLeft undefined (possibleNums nextSlot)
  nsSize   = size nsPn

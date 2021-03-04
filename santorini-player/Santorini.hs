{-# LANGUAGE DeriveGeneric #-}
module Santorini (GameState
  , Move
  , Tokens
  , mValidGameState
  , mValidTokens
  , pickStartingLoc
  , getPossibleMoves
  , pickNextMove
  , fromC
  , toC
  , buildC
  , validTokens
  , tokensUnique
  , occupied
  , canBuild
  , canMove) where

import GHC.Generics
import Data.List (nubBy, sortOn)
import Debug.Trace (trace)

import Data.Aeson (FromJSON, ToJSON)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V ((++), fromList, toList, empty, uniq)

{- Data Structures & Validation -}
type Coord = (Int, Int)
type Tokens = (Coord, Coord)

validCoord :: Coord -> Bool
validCoord (x, y) = 1 <= min x y && max x y <= 5

inTokens :: Coord -> Tokens -> Bool
inTokens c (x, x') = c == x || c == x'

occupied :: Coord -> (Tokens, Tokens) -> Bool
occupied c (tokens1, tokens2) = inTokens c tokens1 || inTokens c tokens2

tokensEq :: Tokens -> Tokens -> Bool
tokensEq (x, x') (y, y') = (x == y && x' == y') || (x == y' && x' == y)

tokensUnique :: Tokens -> Tokens -> Bool
tokensUnique (x, y) (x', y') = x /= x' && x /= y' && y /= x' && y /= y'

validTokens :: Tokens -> Bool
validTokens (c1, c2) =
    -- Ensure coordinates are valid.
  validCoord c1
    && validCoord c2
    -- Ensure coordinates are unique.
    && (c1 /= c2)

mValidTokens []  = Just []
mValidTokens [l] = if validTokens l then Just [l] else Nothing

emptySpaces = V.fromList [ V.fromList [ 0 | _ <- [1 .. 5] ] | _ <- [1 .. 5] ]

data GameState = GameState
  { turn    :: Int
  , spaces  :: Vector (Vector Int)
  , players :: (Tokens, Tokens)
  }
  deriving (Generic, Show)

instance ToJSON GameState
instance FromJSON GameState

spaceIdx :: Vector (Vector Int) -> Coord -> Int
spaceIdx sp (coordX, coordY) = sp ! (coordX - 1) ! (coordY - 1)

mapToSpace
  :: (Int -> Int) -> Vector (Vector Int) -> Coord -> Vector (Vector Int)
mapToSpace f sp (fstC, sndC) =
  let newVal = f $ spaceIdx sp (fstC, sndC)
      row    = fstC - 1
      col    = sndC - 1
  in  sp // [(row, sp ! row // [(col, newVal)])]

flipPlayers :: (Tokens, Tokens) -> (Tokens, Tokens)
flipPlayers (f, s) = (s, f)

validateGameState :: GameState -> Bool
validateGameState (GameState t sp (p1, p2)) =
    -- Ensure each player spaces is valid.
  validTokens p1
    && validTokens p2
    -- Ensure each token coordinate is unique.
    && tokensUnique p1 p2
    -- Ensure there are 5 rows of spaces.
    && (length sp == 5)
    -- Ensure there are 5 columns per row of spaces.
    && all ((5 ==) . length) sp
    -- Ensure each board slot value is in bounds.
    && (all $ all (\x -> 0 <= x && x <= 4)) sp
    -- Ensure turns is a valid number.
    && (t >= 0)

mValidGameState gs = if validateGameState gs then Just gs else Nothing

data Move = Move
  { fromC  :: Coord
  , toC    :: Coord
  , buildC :: Coord
  }
  deriving Show

{- Game State Functions -}
genCoords :: (Coord -> Bool) -> [Coord]
genCoords filterF = [ (x, y) | x <- [1 .. 5], y <- [1 .. 5], filterF (x, y) ]

genTokens :: (Tokens -> Bool) -> [Tokens]
genTokens filterF = nubBy
  tokensEq
  [ ((x, y), (x', y'))
  | x  <- [1 .. 5]
  , y  <- [1 .. 5]
  , x' <- [1 .. 5]
  , y' <- [1 .. 5]
  , filterF ((x, y), (x', y'))
  ]


inSurrounding :: Coord -> Coord -> Bool
inSurrounding (fromX, fromY) (toX, toY) =
  let xDiff = abs (fromX - toX)
      yDiff = abs (fromY - toY)
  in  ((xDiff == 1) || (yDiff == 1)) && (xDiff <= 1) && (yDiff <= 1)

canBuild :: Coord -> Coord -> GameState -> Bool
canBuild (fromX, fromY) (toX, toY) (GameState t sp pl) =
  (spaceIdx sp (toX, toY) < 4) && not (occupied (toX, toY) pl) && inSurrounding
    (fromX, fromY)
    (toX  , toY)

canMove :: Coord -> Coord -> GameState -> Bool
canMove (fromX, fromY) (toX, toY) (GameState t sp pl) =
  let fromLevel = spaceIdx sp (fromX, fromY)
      toLevel   = spaceIdx sp (toX, toY)
  in  fromLevel >= toLevel - 1 && canBuild (fromX, fromY)
                                           (toX  , toY)
                                           (GameState t sp pl)

isWinningMove :: Coord -> GameState -> Bool
isWinningMove (toX, toY) (GameState _ sp _) = spaceIdx sp (toX, toY) == 3

getPossibleMoves :: GameState -> [Move]
getPossibleMoves (GameState t sp ((fstT, sndT), p2)) =
  [ Move { fromC = fromC, toC = (toX, toY), buildC = (buildX, buildY) }
  | let pl = ((fstT, sndT), p2)
-- For each of our tokens.
  , (fromC, otherC) <- [(fstT, sndT), (sndT, fstT)]
-- Get a list of possible moves for each token.
  , (toX  , toY   ) <- genCoords (\x -> canMove fromC x (GameState t sp pl))
-- Create a new GameState with each move.
  , let newGs = GameState t sp (((toX, toY), otherC), p2)
-- Get a list of possible builds from each move, or (0, 0) to indicate a winning move.
  , (buildX, buildY) <- if isWinningMove (toX, toY) (GameState t sp pl)
    then [(0, 0)]
    else genCoords (\x -> canBuild (toX, toY) x newGs)
  ]

-- Apply without swapping players and incrementing turns.
applyMove' :: GameState -> Move -> GameState
applyMove' (GameState t sp ((fstT, sndT), p2)) (Move fC tC bC) =
  let newTs | fC == fstT = ((tC, sndT), p2)
            | fC == sndT = ((fstT, tC), p2)
      newSp = if bC == (0, 0) then sp else mapToSpace (+ 1) sp bC
  in  GameState t newSp newTs

applyMove :: GameState -> Move -> GameState
applyMove gs m =
  let (GameState t sp pl) = applyMove' gs m
  in  GameState (t + 1) sp (flipPlayers pl)

{- Heuristics -}
numberOfMoves gs = length $ getPossibleMoves gs

sumTokenHeight (GameState t sp ((fstT, sndT), _)) =
  spaceIdx sp fstT + spaceIdx sp sndT

scoreMove gs m
  | isWinningMove (toC m) gs
  = 2
  | otherwise
  = let newGs        = applyMove' gs m
        numMoves     = numberOfMoves newGs
        numMovesNorm = fromIntegral numMoves / 91
        tokH         = sumTokenHeight newGs
        tokHNorm     = fromIntegral tokH / 4
        score        = (0.5 * numMovesNorm) + (0.5 * tokHNorm)
    in  score
    -- in  trace
    --       (  "numMoves: "
    --       ++ show numMoves
    --       ++ "\tnorm: "
    --       ++ show numMovesNorm
    --       ++ "\ttokH: "
    --       ++ show tokH
    --       ++ "\ttokHNorm: "
    --       ++ show tokHNorm
    --       ++ "\tscore: "
    --       ++ show score
    --       )
    --       score

{- Selection Functions -}
pickStartingLoc :: [Tokens] -> Maybe [Tokens]
pickStartingLoc []         = Just [((2, 2), (4, 3))]
pickStartingLoc [p2Tokens] = Just
  [ p2Tokens
  , last $ sortOn
    (\x -> numberOfMoves (GameState 0 emptySpaces (x, p2Tokens)))
    (genTokens (\x -> validTokens x && tokensUnique x p2Tokens))
  ]

pickNextMove :: GameState -> Maybe GameState
pickNextMove gs =
  Just (applyMove gs (last $ sortOn (scoreMove gs) (getPossibleMoves gs)))

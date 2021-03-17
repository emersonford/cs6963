{-# LANGUAGE DeriveGeneric #-}

module Santorini
  ( GameState,
    Tokens,
    mValidGameState,
    mValidTokens,
    validTokens,
    tokensUnique,
    occupied,
    canBuild,
    canMove,
    pickNextMove,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int8)
import Data.List (nubBy, sortOn)
import Data.Tuple (swap)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V (empty, fromList, toList, uniq, (++))
import Debug.Trace (trace)
import GHC.Generics

{- Data Structures & Validation -}
type Coord = (Int8, Int8)

type Tokens = (Coord, Coord)

validCoord :: Coord -> Bool
validCoord (x, y) = 1 <= min x y && max x y <= 5

inTokens :: Coord -> Tokens -> Bool
inTokens c (x, x') = c == x || c == x'

occupied :: Coord -> (Tokens, Tokens) -> Bool
occupied c (tokens1, tokens2) = inTokens c tokens1 || inTokens c tokens2

tokensPartialEq :: Tokens -> Tokens -> Bool
tokensPartialEq (x, x') (y, y') = (x == y && x' == y') || (x == y' && x' == y)

tokensUnique :: Tokens -> Tokens -> Bool
tokensUnique (x, y) (x', y') = x /= x' && x /= y' && y /= x' && y /= y'

validTokens :: Tokens -> Bool
validTokens (c1, c2) =
  -- Ensure coordinates are valid.
  validCoord c1
    && validCoord c2
    -- Ensure coordinates are unique.
    && (c1 /= c2)

mValidTokens [] = Just []
mValidTokens [l] = if validTokens l then Just [l] else Nothing

data Card = Apollo | Artemis | Atlas | Demeter | Hephastus | Minotaur | Pan | Prometheus
  deriving (Generic, Show, Eq)

instance ToJSON Card

instance FromJSON Card

data Player = Player
  { card :: Card,
    tokens :: Tokens
  }
  deriving (Generic, Show)

instance ToJSON Player

instance FromJSON Player

data GameState = GameState
  { turn :: Integer,
    spaces :: Vector (Vector Int8),
    players :: (Player, Player)
  }
  deriving (Generic, Show)

instance ToJSON GameState

instance FromJSON GameState

emptySpaces = V.fromList [V.fromList [0 | _ <- [1 .. 5]] | _ <- [1 .. 5]]

getTokenPair :: GameState -> (Tokens, Tokens)
getTokenPair (GameState _ _ (Player _ p1, Player _ p2)) = (p1, p2)

spaceIdx :: Vector (Vector Int8) -> Coord -> Int8
spaceIdx sp (coordX, coordY) = sp ! fromIntegral (coordX - 1) ! fromIntegral (coordY - 1)

mapToSpace ::
  (Int8 -> Int8) -> Vector (Vector Int8) -> Coord -> Vector (Vector Int8)
mapToSpace f sp (fstC, sndC) =
  let newVal = f $ spaceIdx sp (fstC, sndC)
      row = fromIntegral (fstC - 1)
      col = fromIntegral (sndC - 1)
   in sp // [(row, sp ! row // [(col, newVal)])]

validateGameState :: GameState -> Bool
validateGameState (GameState t sp (Player c1 tkns1, Player c2 tkns2)) =
  -- Ensure each player spaces is valid.
  validTokens tkns1
    && validTokens tkns2
    -- Ensure each token coordinate is unique.
    && tokensUnique tkns1 tkns2
    -- Ensure each player has a unique card.
    && (c1 /= c2)
    -- Ensure there are 5 rows of spaces.
    && (length sp == 5)
    -- Ensure there are 5 columns per row of spaces.
    && all ((5 ==) . length) sp
    -- Ensure each board slot value is in bounds.
    && (all $ all (\x -> 0 <= x && x <= 4)) sp
    -- Ensure turns is a valid number.
    && (t >= 0)

mValidGameState gs = if validateGameState gs then Just gs else Nothing

{- Game State Functions -}
genCoords :: (Coord -> Bool) -> [Coord]
genCoords filterF = [(x, y) | x <- [1 .. 5], y <- [1 .. 5], filterF (x, y)]

genTokens :: (Tokens -> Bool) -> [Tokens]
genTokens filterF =
  nubBy
    tokensPartialEq
    [ ((x, y), (x', y'))
      | x <- [1 .. 5],
        y <- [1 .. 5],
        x' <- [1 .. 5],
        y' <- [1 .. 5],
        filterF ((x, y), (x', y'))
    ]

inSurrounding :: Coord -> Coord -> Bool
inSurrounding (fromX, fromY) (toX, toY) =
  let xDiff = abs (fromX - toX)
      yDiff = abs (fromY - toY)
   in ((xDiff == 1) || (yDiff == 1)) && (xDiff <= 1) && (yDiff <= 1)

canBuild' :: Coord -> Coord -> GameState -> Bool -> Bool
canBuild' (fromX, fromY) (toX, toY) gs checkOccupied =
  let (GameState _ sp pl) = gs
   in (spaceIdx sp (toX, toY) < 4)
        && ( not checkOccupied
               || not (occupied (toX, toY) (getTokenPair gs))
           )
        && inSurrounding (fromX, fromY) (toX, toY)

canBuild :: Coord -> Coord -> GameState -> Bool
canBuild fromC toC gs = canBuild' fromC toC gs True

canMove' :: Coord -> Coord -> GameState -> Bool -> Bool
canMove' (fromX, fromY) (toX, toY) gs checkOccupied =
  let (GameState _ sp _) = gs
      fromLevel = spaceIdx sp (fromX, fromY)
      toLevel = spaceIdx sp (toX, toY)
   in fromLevel >= toLevel - 1 && canBuild' (fromX, fromY) (toX, toY) gs checkOccupied

canMove :: Coord -> Coord -> GameState -> Bool
canMove fromC toC gs = canMove' fromC toC gs True

-- Return the GameState as is if the first player's first token is in a space
-- with height 3. Else, apply the function to the GameState.
wonOrElse :: GameState -> [(GameState, Bool)] -> [(GameState, Bool)]
wonOrElse gs els =
  let (GameState _ sp (Player _ (fstT, _), _)) = gs
   in if spaceIdx sp fstT == 3 then [(gs, True)] else els

getBuildMoves' :: Coord -> GameState -> (Int8 -> Int8) -> [(GameState, Bool)]
getBuildMoves' fromC gs buildF = genCoords (\x -> canBuild fromC x gs) >>= (\x -> [(gs {spaces = mapToSpace buildF (spaces gs) x}, False)])

getBuildMoves :: Coord -> GameState -> [(GameState, Bool)]
getBuildMoves fromC gs = getBuildMoves' fromC gs (+ 1)

getMoves :: Coord -> Coord -> GameState -> [(Coord, GameState)]
getMoves fromC otherC gs =
  let (GameState _ _ (p1, p2)) = gs
   in [ (toC, gsMove)
        | toC <- genCoords (\x -> canMove fromC x gs),
          let gsMove = gs {players = (p1 {tokens = (toC, otherC)}, p2)}
      ]

pushBackToken (fromX, fromY) (toX, toY) = (toX + (toX - fromX), toY + (toY - fromY))

getPossibleGameStates' :: Card -> GameState -> Coord -> Coord -> [(GameState, Bool)]
getPossibleGameStates' Apollo gs fromC otherC =
  let (GameState _ _ (p1, p2)) = gs
      (p2t1, p2t2) = tokens p2
   in do
        -- Generate a list of first moves, including those that swap with an _opponent_ piece.
        toC <- genCoords (\x -> canMove' fromC x gs False && x /= otherC)
        let p1Player = p1 {tokens = (toC, otherC)}
        let gsMove
              | toC == p2t1 = gs {players = (p1Player, p2 {tokens = (fromC, p2t2)})}
              | toC == p2t2 = gs {players = (p1Player, p2 {tokens = (p2t1, fromC)})}
              | otherwise = gs {players = (p1Player, p2)}

        wonOrElse
          gsMove
          -- Generate a list of builds after the first move.
          (getBuildMoves toC gsMove)
getPossibleGameStates' Artemis gs fromC otherC =
  let (GameState _ _ (p1, p2)) = gs
   in do
        -- Generate a list of first moves.
        (toC, gsMove1) <- getMoves fromC otherC gs

        wonOrElse
          gsMove1
          -- Generate a list of builds after the first move.
          ( getBuildMoves toC gsMove1
              -- Generate a list of optional second moves.
              ++ ( do
                     toC2 <- genCoords (\x -> canMove toC x gsMove1 && (x /= fromC))
                     let gsMove2 = gsMove1 {players = (p1 {tokens = (toC2, otherC)}, p2)}

                     wonOrElse
                       gsMove2
                       -- Generate a list of builds after the second move.
                       (getBuildMoves toC2 gsMove2)
                 )
          )
getPossibleGameStates' Atlas gs fromC otherC =
  do
    -- Generate a list of first moves.
    (toC, gsMove) <- getMoves fromC otherC gs

    wonOrElse
      gsMove
      -- Generate a list of builds after the first move.
      ( getBuildMoves toC gsMove
          -- Generate a list of builds that create a dome.
          ++ getBuildMoves' toC gsMove (const 4)
      )
getPossibleGameStates' Demeter gs fromC otherC =
  do
    -- Generate a list of first moves.
    (toC, gsMove) <- getMoves fromC otherC gs

    wonOrElse
      gsMove
      -- Generate a list of builds after the first move.
      ( do
          firstBuildC <- genCoords (\x -> canBuild toC x gsMove)
          let gsBuild1 = gsMove {spaces = mapToSpace (+ 1) (spaces gsMove) firstBuildC}

          -- Generate a list of optional second builds after the first build.
          (gsBuild1, False) :
            [ (gsBuild1 {spaces = mapToSpace (+ 1) (spaces gsBuild1) secondBuildC}, False)
              | secondBuildC <- genCoords (\x -> canBuild toC x gsBuild1 && x /= firstBuildC)
            ]
      )
getPossibleGameStates' Hephastus gs fromC otherC =
  do
    -- Generate a list of first moves.
    (toC, gsMove) <- getMoves fromC otherC gs

    wonOrElse
      gsMove
      -- Generate a list of builds after the first move.
      ( do
          firstBuildC <- genCoords (\x -> canBuild toC x gsMove)
          let gsBuild1 = gsMove {spaces = mapToSpace (+ 1) (spaces gsMove) firstBuildC}

          -- If the space we just built on is less than 3, add an additonal
          -- move to build once more on that space.
          if spaceIdx (spaces gsBuild1) firstBuildC < 3
            then [(gsBuild1, False), (gsBuild1 {spaces = mapToSpace (+ 1) (spaces gsBuild1) firstBuildC}, False)]
            else [(gsBuild1, False)]
      )
getPossibleGameStates' Minotaur gs fromC otherC =
  let (GameState _ _ (p1, p2)) = gs
      (p2t1, p2t2) = tokens p2
   in do
        -- Generate a list of first moves, including those that swap with an _opponent_ piece.
        toC <- genCoords (\x -> canMove' fromC x gs False && x /= otherC)
        let p1Player = p1 {tokens = (toC, otherC)}

        let pushback = pushBackToken fromC toC
        let p2Player
              | (toC == p2t1) && validCoord pushback && canBuild p2t1 pushback gs = Just p2 {tokens = (pushback, p2t2)}
              | (toC == p2t2) && validCoord pushback && canBuild p2t2 pushback gs = Just p2 {tokens = (p2t1, pushback)}
              | (toC /= p2t1) && (toC /= p2t2) = Just p2
              | otherwise = Nothing

        let gsMove = p2Player >>= (\x -> Just gs {players = (p1Player, x)})

        maybe
          []
          ( \x ->
              wonOrElse
                x
                -- Generate a list of builds after the first move.
                (getBuildMoves toC x)
          )
          gsMove
getPossibleGameStates' Pan gs fromC otherC =
  let (GameState _ sp _) = gs
   in do
        -- Generate a list of first moves.
        (toC, gsMove) <- getMoves fromC otherC gs

        if spaceIdx sp toC == 3 || (spaceIdx sp fromC - spaceIdx sp toC >= 2)
          then [(gsMove, True)]
          else -- Generate a list of builds after the first move.
            getBuildMoves toC gsMove
getPossibleGameStates' Prometheus gs fromC otherC =
  let (GameState _ sp (p1, p2)) = gs
   in ( do
          -- Generate a list of first builds.
          (gsBuild1, _) <- getBuildMoves fromC gs

          -- Generate a list of moves restricted to the level we're currently at.
          let level = spaceIdx sp fromC
          toC <- genCoords (\x -> canMove fromC x gs && spaceIdx sp x <= level)
          let gsMove = gsBuild1 {players = (p1 {tokens = (toC, otherC)}, p2)}

          -- Generate a list of second builds if we didn't win on our move.
          wonOrElse gsMove (getBuildMoves toC gsMove)
      )
        -- Generate a list of moves without the first build.
        ++ ( do
               -- Generate a list of first moves.
               (toC, gsMove) <- getMoves fromC otherC gs

               wonOrElse
                 gsMove
                 -- Generate a list of builds after the first move.
                 (getBuildMoves toC gsMove)
           )

getPossibleGameStates :: GameState -> [(GameState, Bool)]
getPossibleGameStates gs = let (GameState _ _ (Player c (fstT, sndT), p2)) = gs in [(fstT, sndT), (sndT, fstT)] >>= uncurry (getPossibleGameStates' c gs)

incrementTurn (GameState t sp pls) = GameState (t + 1) sp (swap pls)

{- Heuristics -}
sumTokenHeight (GameState _ sp (p1, _)) =
  let (fstT, sndT) = tokens p1 in spaceIdx sp fstT + spaceIdx sp sndT

scoreGameState :: (GameState, Bool) -> Double
scoreGameState (gs, gsWon)
  | gsWon = 2
  | otherwise =
    let numMoves = length $ getPossibleGameStates gs
        numMovesNorm = fromIntegral numMoves / 91
        tokH = sumTokenHeight gs
        tokHNorm = fromIntegral tokH / 4
        score = (0.5 * numMovesNorm) + (0.5 * tokHNorm)
     in trace
          ( "numMoves: "
              ++ show numMoves
              ++ "\tnorm: "
              ++ show numMovesNorm
              ++ "\ttokH: "
              ++ show tokH
              ++ "\ttokHNorm: "
              ++ show tokHNorm
              ++ "\tscore: "
              ++ show score
          )
          score

-- {- Selection Functions -}
-- pickStartingLoc :: [Tokens] -> Maybe [Tokens]
-- pickStartingLoc [] = Just [((2, 2), (4, 3))]
-- pickStartingLoc [p2Tokens] =
--   Just
--     [ p2Tokens,
--       last $
--         sortOn
--           (\x -> numberOfMoves (GameState 0 emptySpaces (x, p2Tokens)))
--           (genTokens (\x -> validTokens x && tokensUnique x p2Tokens))
--     ]

pickNextMove :: GameState -> Maybe GameState
pickNextMove gs =
  Just (fst $ last $ sortOn scoreGameState (getPossibleGameStates gs))

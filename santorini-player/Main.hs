module Main where

import Data.Aeson (decode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8 (putStrLn)
import System.Exit (exitFailure, exitSuccess)
import System.IO (isEOF, hFlush, stdout)

import Santorini (GameState, Tokens, mValidGameState, mValidTokens, pickStartingLoc, getPossibleMoves, pickNextMove)

-- {"players":[[[2,5],[3,5]],[[3,4],[4,4]]], "spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]], "turn":19}

main :: IO ()
main = do
  inp <- B.getLine
  case
      (decode (LB.fromStrict inp) :: Maybe [Tokens])
      >>= mValidTokens
      >>= pickStartingLoc
    of
      Just ts -> do
        LB8.putStrLn (encode ts)
        hFlush stdout
        gameLoop
      _ -> do
        putStrLn "Received invalid starting positions, exiting..."
        exitFailure



gameLoop :: IO ()
gameLoop = do
  inp <- B.getLine
  case
      (decode (LB.fromStrict inp) :: Maybe GameState)
      >>= mValidGameState
      >>= pickNextMove
    of
      Just gs -> do
        LB8.putStrLn (encode gs)
        hFlush stdout
        gameLoop
      Nothing -> do
        putStrLn "Received invalid game state, exiting..."
        exitFailure

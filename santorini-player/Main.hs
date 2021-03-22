module Main where

import Data.Aeson (decode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8 (putStrLn)
import Santorini (Card, GameState, Player (Player), PrePlayer (PrePlayer), Tokens, mValidGameState, pickNextMove, pickStartingLocFirst, pickStartingLocSecond, validTokens)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, isEOF, stdout)

main :: IO ()
main = do
  inp <- B.getLine
  case (decode (LB.fromStrict inp) :: Maybe (PrePlayer, Player))
    >>= ( \(PrePlayer c1, Player c2 t) ->
            if c1 /= c2 then Just (PrePlayer c1, Player c2 t) else Nothing
        )
    >>= (\(pp1, p2) -> let p1 = pickStartingLocSecond (pp1, p2) in Just (p2, p1)) of
    Just ts -> do
      LB8.putStrLn (encode ts)
      hFlush stdout
      gameLoop
    Nothing -> case (decode (LB.fromStrict inp) :: Maybe (PrePlayer, PrePlayer))
      >>= ( \(PrePlayer c1, PrePlayer c2) ->
              if c1 /= c2 then Just (PrePlayer c1, PrePlayer c2) else Nothing
          )
      >>= (\(PrePlayer c, pp2) -> let p1 = pickStartingLocFirst c in Just (pp2, p1)) of
      Just ts -> do
        LB8.putStrLn (encode ts)
        hFlush stdout
        gameLoop
      Nothing -> do
        putStrLn "Received invalid starting positions, exiting..."
        exitFailure

gameLoop :: IO ()
gameLoop = do
  inp <- B.getLine
  case (decode (LB.fromStrict inp) :: Maybe GameState)
    >>= mValidGameState
    >>= (Just . pickNextMove) of
    Just gs -> do
      LB8.putStrLn (encode gs)
      hFlush stdout
      gameLoop
    Nothing -> do
      putStrLn "Received invalid game state, exiting..."
      exitFailure

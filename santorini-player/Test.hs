{-# LANGUAGE QuasiQuotes #-}
module Main where

import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ(r)
import Control.Exception (evaluate)
import Control.Parallel.Strategies (using, rdeepseq, parList, parListChunk)
import Debug.Trace (trace)

import Data.Maybe (isNothing, isJust, fromJust)
import Data.Aeson (decode)
import Data.Vector (update, fromList)
import qualified Data.ByteString.Lazy.Char8 as LB8

import Santorini (GameState
  , Move
  , Tokens
  , mValidGameState
  , mValidTokens
  , pickStartingLoc
  , getPossibleMoves
  , fromC
  , toC
  , buildC
  , validTokens
  , tokensUnique)

main :: IO ()
main = hspec $ do
  it "check invalid starting tokens" $ do
    all
        ( isNothing
        . (\x ->
            (decode (LB8.pack x) :: Maybe [Tokens])
              >>= mValidTokens
              >>= pickStartingLoc
          )
        )
        [ "[[]]"
        , ""
        , "[[1, 1]]"
        , "[[[1, 1], [1, 1]]]"
        , "[[[1, 1], [1, 6]]]"
        , "[[[1, 2], [3, 4]], [[2, 2], [4, 4]]]"
        ]
      `shouldBe` True

  it "check valid tokens" $ do
    all
        ( isJust
        . (\x ->
            (decode (LB8.pack x) :: Maybe [Tokens])
              >>= mValidTokens
              >>= pickStartingLoc
          )
        )
        ["[]", "[[[1, 1], [1, 2]]]"]
      `shouldBe` True

  it "check invalid gameState" $ do
    all
        ( isNothing
        . (\x -> (decode (LB8.pack x) :: Maybe GameState) >>= mValidGameState)
        )
        [ [r|{"players":[], "spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]], "turn":19}|]
        , [r|{"players":[[[2,5],[3,5]]], "spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]], "turn":19}|]
        , [r|{"players":[[[2,5],[3,5]], [[]]], "spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]], "turn":19}|]
        , [r|{"players":[[[2,5],[3,5]], [[2,5],[4,4]]], "spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]], "turn":19}|]
        , [r|{"players":[[[2,4],[3,5]], [[2,5],[3,5]]], "spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]], "turn":19}|]
        , [r|{"players":[[[2,4],[3,5]], [[2,5],[2,5]]], "spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]], "turn":19}|]
        , [r|{"players":[[[2,4],[2,4]], [[2,5],[2,3]]], "spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]], "turn":19}|]
        , [r|{"players":[[[2,4],[3,5]], [[2,5],[4,5]]], "spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,5]], "turn":19}|]
        , [r|{"players":[[[2,4],[3,5]], [[2,5],[4,5]]], "spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0]], "turn":19}|]
        , [r|{"players":[[[2,4],[3,5]], [[2,5],[4,5]]], "spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,1]], "turn":-1}|]
        ]
      `shouldBe` True

  it "check we don't move and build to the same place in genPossibleMoves" $ do
    and
        (       [ all
                    (\x -> toC x /= buildC x)
                    (getPossibleMoves
                      (fromJust
                        (decode
                          (LB8.pack
                            ([r|{"players":|]
                            ++ show [[[x1, x2], [x3, x4]], [[y1, y2], [y3, y4]]]
                            ++ [r|, "spaces":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]], "turn":1}|]
                            )
                          ) :: Maybe GameState
                        )
                      )
                    )
                | x1 <- [1 .. 5]
                , x2 <- [1 .. 5]
                , x3 <- [1 .. 5]
                , x4 <- [1 .. 5]
                , y1 <- [1 .. 5]
                , y2 <- [1 .. 5]
                , y3 <- [1 .. 5]
                , y4 <- [1 .. 5]
                , let coordList = [(x1, x2), (x3, x4), (y1, y2), (y3, y4)]
                , let xTokens   = (head coordList, coordList !! 1)
                , let yTokens   = (coordList !! 2, coordList !! 3)
                , validTokens xTokens
                , validTokens yTokens
                , tokensUnique xTokens yTokens
                ]
        `using` parListChunk 2048 rdeepseq
        )
      `shouldBe` True

  it "check we don't move or build into a 4 tall space" $ do
    and
        (       [ all
                    (\x -> toC x /= fourC && buildC x /= fourC)
                    (getPossibleMoves
                      (fromJust
                        (decode
                          (LB8.pack
                            (  [r|{"players":|]
                            ++ show [[[x1, x2], [x3, x4]], [[y1, y2], [y3, y4]]]
                            ++ [r|, "spaces":|]
                            ++ show spaces
                            ++ [r|, "turn":1}|]
                            )
                          ) :: Maybe GameState
                        )
                      )
                    )
                | x1    <- [1 .. 5]
                , x2    <- [1 .. 5]
                , x3    <- [1 .. 5]
                , x4    <- [1 .. 5]
                , y1    <- [1 .. 5]
                , y2    <- [1 .. 5]
                , y3    <- [1 .. 5]
                , y4    <- [1 .. 5]
                , spIdx <- [0 .. 24]
                , let spIdxX = spIdx `div` 5
                , let spIdxY = spIdx `mod` 5
                , let fourC = (spIdxX + 1, spIdxY + 1)
                , let
                  spaces = update
                    (fromList $ map
                      fromList
                      [ [0, 0, 0, 0, 0]
                      , [0, 0, 0, 0, 0]
                      , [0, 0, 0, 0, 0]
                      , [0, 0, 0, 0, 0]
                      , [0, 0, 0, 0, 0]
                      ]
                    )
                    (fromList
                      [ ( spIdxX
                        , update (fromList [0, 0, 0, 0, 0]) (fromList [(spIdxY, 4)])
                        )
                      ]
                    )
                , let coordList = [(x1, x2), (x3, x4), (y1, y2), (y3, y4)]
                , let xTokens   = (head coordList, coordList !! 1)
                , let yTokens   = (coordList !! 2, coordList !! 3)
                , validTokens xTokens
                , validTokens yTokens
                , tokensUnique xTokens yTokens
                ]
        `using` parListChunk 2048 rdeepseq
        )
      `shouldBe` True

  it "check that we don't move / build into another token" $ do
    and
        (       [ all
                    (\x ->
                      (toC x `notElem` coordList)
                        && ((buildC x == fromC x) || (buildC x `notElem` coordList))
                    )
                    (getPossibleMoves
                      (fromJust
                        (decode
                          (LB8.pack
                            ([r|{"players":|]
                            ++ show [[[x1, x2], [x3, x4]], [[y1, y2], [y3, y4]]]
                            ++ [r|, "spaces":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]], "turn":1}|]
                            )
                          ) :: Maybe GameState
                        )
                      )
                    )
                | x1 <- [1 .. 5]
                , x2 <- [1 .. 5]
                , x3 <- [1 .. 5]
                , x4 <- [1 .. 5]
                , y1 <- [1 .. 5]
                , y2 <- [1 .. 5]
                , y3 <- [1 .. 5]
                , y4 <- [1 .. 5]
                , let coordList = [(x1, x2), (x3, x4), (y1, y2), (y3, y4)]
                , let xTokens   = (head coordList, coordList !! 1)
                , let yTokens   = (coordList !! 2, coordList !! 3)
                , validTokens xTokens
                , validTokens yTokens
                , tokensUnique xTokens yTokens
                ]
        `using` parListChunk 2048 rdeepseq
        )
      `shouldBe` True

  it "check for no possible moves on block out" $ do
    length
        (getPossibleMoves
          (fromJust
            (decode
              (LB8.pack
                "{\"turn\": 0, \"players\": [[[1,1],[3,3]],[[5,5],[5,1]]], \"spaces\": [[2,4,0,0,0],[4,4,4,4,0],[0,4,0,4,0],[0,4,4,4,0],[0,0,0,0,0]]}"
              ) :: Maybe GameState
            )
          )
        )
      `shouldBe` 0

  it "check for two possible moves on block out" $ do
    all
        (\x ->
          fromC x
            == (1, 1)
            && toC x
            == (1, 2)
            && (buildC x == (1, 1) || buildC x == (1, 3))
        )
        (getPossibleMoves
          (fromJust
            (decode
              (LB8.pack
                "{\"turn\": 0, \"players\": [[[1,1],[3,3]],[[5,5],[5,1]]], \"spaces\": [[2,2,0,0,0],[4,4,4,4,0],[0,4,0,4,0],[0,4,4,4,0],[0,0,0,0,0]]}"
              ) :: Maybe GameState
            )
          )
        )
      `shouldBe` True

  it "check for one possible moves on win" $ do
    all
        (\x -> fromC x == (1, 1) && toC x == (1, 2) && (buildC x == (0, 0)))
        (getPossibleMoves
          (fromJust
            (decode
              (LB8.pack
                "{\"turn\": 0, \"players\": [[[1,1],[3,3]],[[5,5],[5,1]]], \"spaces\": [[2,3,0,0,0],[4,4,4,4,0],[0,4,0,4,0],[0,4,4,4,0],[0,0,0,0,0]]}"
              ) :: Maybe GameState
            )
          )
        )
      `shouldBe` True

module Main where

import Control.Monad (unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Network.Socket.ByteString (recv, sendAll)
import TCPServer (runTCPServer)

main :: IO ()
main = runTCPServer Nothing "3001" talk
  where
    talk s = do
      msg <- recv s 1024
      unless (S.null msg) $ do
        S8.putStr (S.append (S8.pack "Got: \n") msg)
        sendAll s (S.append (S8.pack "Echo: ") msg)
        talk s

module Main where

import qualified Control.Exception as E
import Control.Monad (mzero, unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import Debug.Trace (trace)
import qualified MatchAction as MA (ActionResult (..), combineMA)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Protocol (TSTPMsg, decodeMsg, encodeMsg)
import qualified Protocol as TSTP (TSTPHVal (..), TSTPOp (..), headers, op, path, payload, version)
import System.IO
import TCPServer (runTCPServer)

main :: IO ()
main = runTCPServer Nothing "3000" forwardMessages

forwardMessages s =
  withSocketsDo $ do
    addr <- resolve

    hdl <- socketToHandle s ReadWriteMode
    hSetBuffering hdl NoBuffering

    E.bracket (open addr) (\s -> close s >> hClose hdl) (forward hdl)
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just "localhost") (Just "3000")
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock
    forward hdl ss = do
      msg <- (decodeMsg hdl :: IO (Maybe TSTPMsg))
      if isNothing msg
        then do
          hPutStr hdl "HTTP/1.0 400 Bad Request\r\n\r\n"
        else do
          let m = fromJust msg
          S8.putStr (S.append (S8.pack "received msg: \n") (encodeMsg m))

          case maChain m of
            MA.Error -> do
              hPutStr hdl "HTTP/1.0 400 Bad Request\r\n\r\n"
            (MA.Continue x) -> do
              sendAll ss (encodeMsg x)
              resp <- recv ss 4096

              S8.putStr (S.append (S8.pack "received resp: \n") resp)
              S.hPut hdl resp

              forward hdl ss
            (MA.Forward x) -> do
              sendAll ss (encodeMsg x)
              resp <- recv ss 4096

              S8.putStr (S.append (S8.pack "received resp: \n") resp)
              S.hPut hdl resp

              forward hdl ss
      where
        maChain = MA.combineMA rules

rules :: [(TSTPMsg -> Bool, TSTPMsg -> MA.ActionResult TSTPMsg)]
rules =
  [ ( Map.member (T.pack "Flag-A") . TSTP.headers,
      const MA.Error
    ),
    ( \x -> T.pack "/foo" == TSTP.path x,
      \x -> MA.Continue x {TSTP.path = T.pack "/foobar"}
    ),
    ( \x -> T.pack "/foobar" == TSTP.path x,
      MA.Forward
    ),
    ( Map.member (T.pack "Flag-B") . TSTP.headers,
      \x ->
        MA.Continue
          x
            { TSTP.headers = let tmp = Map.adjust (\(TSTP.TSTPHText t) -> TSTP.TSTPHText (T.append t (T.pack "bar"))) (T.pack "Flag-B") (TSTP.headers x) in trace (show tmp) tmp
            }
    )
  ]

{-# LANGUAGE DeriveGeneric #-}

module Protocol (Protocol, encodeMsg, decodeMsg, TSTPMsg, op, path, version, headers, payload, TSTPHVal (..), TSTPOp (..)) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Attoparsec.Text (decimal, maybeResult, parse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (hGetLine)
import GHC.Generics
import Network.Socket
import Network.Socket.ByteString (recv)
import System.IO (Handle)
import Text.Read (readMaybe)

class Protocol a where
  decodeMsg :: Handle -> IO (Maybe a)
  encodeMsg :: a -> BS.ByteString

-- TODO: Learn how to use the State monad for this.
getLineFromSocket :: Socket -> BS.ByteString -> MaybeT IO (BS.ByteString, BS.ByteString)
getLineFromSocket s buf = do
  d <- liftIO $ recv s 1024
  let dat = BS.append buf d

  -- ord('\n') == 10
  let (b1, b2) = BS.break (== 10) dat
  if BS.length b2 == 0
    then getLineFromSocket s b1
    else return (b1, BS.tail b2)

-- Use a primitive form of HTTP as a test protocol for writing this proxy as
-- HTCondor CE's protocol appears to be very complicated and will take a lot of
-- work to understand.
-- TSTP = Test Protocol

-- Invariants:
-- 1. Duplicate headers are not present.

-- EXAMPLE:
-- GET / HTTP/1.1
-- Content-Length: 20
-- Host: localhost
-- Flag-A: 1
-- Flag-B: foobar
--
-- { a: 12, b: "hello"}

data TSTPOp = GET | HEAD | POST | PUT
  deriving (Read, Show)

data TSTPHVal
  = TSTPHInt Int
  | TSTPHText T.Text
  deriving (Eq, Show)

toTSTPHVal :: T.Text -> T.Text -> Maybe TSTPHVal
toTSTPHVal h v
  | h == T.pack "Content-Length" = maybeResult (parse decimal v) >>= (Just . TSTPHInt)
  | otherwise = Just (TSTPHText v)

data TSTPMsg = TSTPMsg
  { op :: TSTPOp,
    path :: T.Text,
    version :: T.Text,
    headers :: Map.Map T.Text TSTPHVal,
    payload :: Maybe BS.ByteString
  }

getTSTPHeaders :: Handle -> Map.Map T.Text TSTPHVal -> MaybeT IO (Map.Map T.Text TSTPHVal)
getTSTPHeaders h headersMap = do
  line <- liftIO $ hGetLine h

  -- Check for empty line to signify end of headers.
  if line == T.singleton '\r'
    then return headersMap
    else do
      let (hname, hval) = sndMap (T.drop 2) (T.breakOn (T.pack ": ") (T.dropWhileEnd (== '\r') line))
      guard (Map.notMember hname headersMap) -- ensure no duplicate headers
      guard (not $ T.null hval) -- ensure we got a header value
      let val = toTSTPHVal hname hval
      guard (isJust val) -- ensure header value is of the right type
      getTSTPHeaders h (Map.insert hname (fromJust val) headersMap)
  where
    sndMap f (fstE, sndE) = (fstE, f sndE)

instance Protocol TSTPMsg where
  decodeMsg h = runMaybeT $ do
    line <- liftIO $ hGetLine h

    let spltLine = T.splitOn (T.singleton ' ') line
    guard (length spltLine == 3)
    let [o, p, v] = spltLine

    let decodedOp = (readMaybe (T.unpack o) :: Maybe TSTPOp)
    guard (isJust decodedOp)

    let vers = T.stripPrefix (T.pack "HTTP/") v
    guard (isJust vers)

    hdrs <- getTSTPHeaders h Map.empty
    let tmpMsg = TSTPMsg {op = fromJust decodedOp, path = p, version = fromJust vers, headers = hdrs, payload = Nothing}

    case Map.lookup (T.pack "Content-Length") hdrs of
      Just (TSTPHInt l) -> do
        pl <- liftIO $ BS.hGet h l
        if BS.length pl /= l then mzero else return tmpMsg {payload = Just pl}
      _ -> return tmpMsg

  encodeMsg (TSTPMsg o p v hdrs pl) =
    BS.concat
      ( [BS8.pack (show o), BS8.singleton ' ', encodeUtf8 p, BS8.singleton ' ', BS8.pack "HTTP/", encodeUtf8 v, BS8.pack "\r\n"]
          ++ Map.foldrWithKey (\k v ks -> BS.concat [encodeUtf8 k, BS8.pack ": ", hvalToBs v, BS8.pack "\r\n"] : ks) [] hdrs
          ++ [BS8.pack "\r\n"]
          ++ ( case pl of
                 (Just d) -> [d]
                 _ -> []
             )
      )
    where
      hvalToBs v = case v of
        (TSTPHInt i) -> BS8.pack (show i)
        (TSTPHText t) -> encodeUtf8 t

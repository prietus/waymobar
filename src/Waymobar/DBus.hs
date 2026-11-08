-- | A small pure-Haskell D-Bus client: enough to be a StatusNotifierItem
-- watcher/host and to drive dbusmenu. Little-endian marshalling, no fd
-- passing, SASL EXTERNAL authentication.
module Waymobar.DBus
  ( -- * Values
    Type (..)
  , Value (..)
  , typeOf
  , signature
  , parseSignature
    -- * Messages
  , MsgType (..)
  , Message (..)
  , emptyMessage
    -- * Connection
  , Bus
  , busName
  , connectSession
  , call
  , callNoReply
  , emitSignal
  , reply
  , replyError
  , setHandler
  , addMatch
  , requestName
    -- * Helpers
  , fromVariant
  , dictLookup
  , valueString
  , valueBool
  , valueInt
  ) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.Binary.Get
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Numeric (showHex)
import System.Environment (lookupEnv)
import System.Posix.User (getRealUserID)
import System.Timeout (timeout)

------------------------------------------------------------------------------
-- Values

data Type
  = TByte | TBool | TInt16 | TUInt16 | TInt32 | TUInt32 | TInt64 | TUInt64
  | TDouble | TString | TObjectPath | TSignature | TVariant | TUnixFd
  | TArray Type
  | TStruct [Type]
  | TDictEntry Type Type
  deriving (Eq, Show)

data Value
  = VByte Word8
  | VBool Bool
  | VInt16 Int16
  | VUInt16 Word16
  | VInt32 Int32
  | VUInt32 Word32
  | VInt64 Int64
  | VUInt64 Word64
  | VDouble Double
  | VString String
  | VObjectPath String
  | VSignature [Type]
  | VVariant Value
  | VUnixFd Word32
  | VArray Type [Value]      -- ^ element type, elements
  | VStruct [Value]
  | VDictEntry Value Value
  | VBytes B.ByteString      -- ^ an @ay@, kept packed
  deriving (Eq, Show)

typeOf :: Value -> Type
typeOf = \case
  VByte _ -> TByte
  VBool _ -> TBool
  VInt16 _ -> TInt16
  VUInt16 _ -> TUInt16
  VInt32 _ -> TInt32
  VUInt32 _ -> TUInt32
  VInt64 _ -> TInt64
  VUInt64 _ -> TUInt64
  VDouble _ -> TDouble
  VString _ -> TString
  VObjectPath _ -> TObjectPath
  VSignature _ -> TSignature
  VVariant _ -> TVariant
  VUnixFd _ -> TUnixFd
  VArray t _ -> TArray t
  VStruct vs -> TStruct (map typeOf vs)
  VDictEntry k v -> TDictEntry (typeOf k) (typeOf v)
  VBytes _ -> TArray TByte

signature :: [Type] -> String
signature = concatMap sig1
  where
    sig1 = \case
      TByte -> "y"; TBool -> "b"; TInt16 -> "n"; TUInt16 -> "q"
      TInt32 -> "i"; TUInt32 -> "u"; TInt64 -> "x"; TUInt64 -> "t"
      TDouble -> "d"; TString -> "s"; TObjectPath -> "o"; TSignature -> "g"
      TVariant -> "v"; TUnixFd -> "h"
      TArray t -> 'a' : sig1 t
      TStruct ts -> "(" ++ concatMap sig1 ts ++ ")"
      TDictEntry k v -> "{" ++ sig1 k ++ sig1 v ++ "}"

parseSignature :: String -> Maybe [Type]
parseSignature s = case many1 s of
  Just (ts, "") -> Just ts
  _ -> Nothing
  where
    many1 str = go str []
      where
        go "" acc = Just (reverse acc, "")
        go r@(')' : _) acc = Just (reverse acc, r)
        go r@('}' : _) acc = Just (reverse acc, r)
        go r acc = one r >>= \(t, r') -> go r' (t : acc)
    one = \case
      'y' : r -> Just (TByte, r); 'b' : r -> Just (TBool, r)
      'n' : r -> Just (TInt16, r); 'q' : r -> Just (TUInt16, r)
      'i' : r -> Just (TInt32, r); 'u' : r -> Just (TUInt32, r)
      'x' : r -> Just (TInt64, r); 't' : r -> Just (TUInt64, r)
      'd' : r -> Just (TDouble, r); 's' : r -> Just (TString, r)
      'o' : r -> Just (TObjectPath, r); 'g' : r -> Just (TSignature, r)
      'v' : r -> Just (TVariant, r); 'h' : r -> Just (TUnixFd, r)
      'a' : r -> one r >>= \(t, r') -> Just (TArray t, r')
      '(' : r -> case many1 r of
        Just (ts, ')' : r') -> Just (TStruct ts, r')
        _ -> Nothing
      '{' : r -> case many1 r of
        Just ([k, v], '}' : r') -> Just (TDictEntry k v, r')
        _ -> Nothing
      _ -> Nothing

alignment :: Type -> Int
alignment = \case
  TByte -> 1; TSignature -> 1; TVariant -> 1
  TInt16 -> 2; TUInt16 -> 2
  TInt64 -> 8; TUInt64 -> 8; TDouble -> 8
  TStruct _ -> 8; TDictEntry _ _ -> 8
  _ -> 4

------------------------------------------------------------------------------
-- Marshalling

padTo :: Int -> Int -> Int
padTo a off = (a - off `mod` a) `mod` a

-- | Encode a value at a given offset; returns the bytes and the new offset.
encode :: Int -> Value -> (BB.Builder, Int)
encode off v =
  let a = alignment (typeOf v)
      p = padTo a off
      (b, n) = body (off + p) v
  in (zeros p <> b, off + p + n)
  where
    zeros k = mconcat (replicate k (BB.word8 0))
    body o = \case
      VByte x -> (BB.word8 x, 1)
      VBool x -> (BB.word32LE (if x then 1 else 0), 4)
      VInt16 x -> (BB.int16LE x, 2)
      VUInt16 x -> (BB.word16LE x, 2)
      VInt32 x -> (BB.int32LE x, 4)
      VUInt32 x -> (BB.word32LE x, 4)
      VInt64 x -> (BB.int64LE x, 8)
      VUInt64 x -> (BB.word64LE x, 8)
      VDouble x -> (BB.doubleLE x, 8)
      VUnixFd x -> (BB.word32LE x, 4)
      VString s -> str s
      VObjectPath s -> str s
      VSignature ts ->
        let bs = utf8 (signature ts)
        in (BB.word8 (fromIntegral (B.length bs)) <> BB.byteString bs <> BB.word8 0, B.length bs + 2)
      VVariant x ->
        let sig = utf8 (signature [typeOf x])
            sigLen = B.length sig + 2
            (b, o') = encode (o + sigLen) x
        in (BB.word8 (fromIntegral (B.length sig)) <> BB.byteString sig <> BB.word8 0 <> b, o' - o)
      VStruct xs -> seqEnc o xs
      VDictEntry k x -> seqEnc o [k, x]
      VBytes bs -> (BB.word32LE (fromIntegral (B.length bs)) <> BB.byteString bs, 4 + B.length bs)
      VArray t xs ->
        let start = o + 4
            p = padTo (alignment t) start
            (b, end) = foldl (\(acc, cur) x -> let (bx, cur') = encode cur x in (acc <> bx, cur')) (mempty, start + p) xs
            len = end - (start + p)
        in (BB.word32LE (fromIntegral len) <> zeros p <> b, end - o)
    str s =
      let bs = utf8 s
      in (BB.word32LE (fromIntegral (B.length bs)) <> BB.byteString bs <> BB.word8 0, B.length bs + 5)
    seqEnc o xs =
      let (b, end) = foldl (\(acc, cur) x -> let (bx, cur') = encode cur x in (acc <> bx, cur')) (mempty, o) xs
      in (b, end - o)

encodeMany :: Int -> [Value] -> (BB.Builder, Int)
encodeMany off = foldl (\(acc, cur) x -> let (b, cur') = encode cur x in (acc <> b, cur')) (mempty, off)

alignGet :: Int -> Get ()
alignGet a = do
  pos <- fromIntegral <$> bytesRead
  skip (padTo a pos)

decode :: Type -> Get Value
decode t = do
  alignGet (alignment t)
  case t of
    TByte -> VByte <$> getWord8
    TBool -> VBool . (/= 0) <$> getWord32le
    TInt16 -> VInt16 <$> getInt16le
    TUInt16 -> VUInt16 <$> getWord16le
    TInt32 -> VInt32 <$> getInt32le
    TUInt32 -> VUInt32 <$> getWord32le
    TInt64 -> VInt64 <$> getInt64le
    TUInt64 -> VUInt64 <$> getWord64le
    TDouble -> VDouble <$> getDoublele
    TUnixFd -> VUnixFd <$> getWord32le
    TString -> VString <$> getStr
    TObjectPath -> VObjectPath <$> getStr
    TSignature -> VSignature <$> getSig
    TVariant -> do
      ts <- getSig
      case ts of
        [vt] -> VVariant <$> decode vt
        _ -> fail "bad variant signature"
    TStruct ts -> VStruct <$> mapM decode ts
    TDictEntry k v -> VDictEntry <$> decode k <*> decode v
    TArray TByte -> do
      len <- fromIntegral <$> getWord32le
      VBytes <$> getByteString len
    TArray et -> do
      len <- (fromIntegral :: Word32 -> Int) <$> getWord32le
      alignGet (alignment et)
      start <- (fromIntegral :: Int64 -> Int) <$> bytesRead
      let loop acc = do
            pos <- (fromIntegral :: Int64 -> Int) <$> bytesRead
            if pos - start >= len then pure (reverse acc) else decode et >>= \x -> loop (x : acc)
      VArray et <$> loop []
  where
    getStr = do
      len <- fromIntegral <$> getWord32le
      s <- getByteString len
      skip 1
      pure (fromUtf8 s)
    getSig = do
      len <- fromIntegral <$> getWord8
      s <- getByteString len
      skip 1
      maybe (fail "bad signature") pure (parseSignature (BC.unpack s))

------------------------------------------------------------------------------
-- Messages

data MsgType = MethodCall | MethodReturn | ErrorMsg | Signal | OtherType Word8
  deriving (Eq, Show)

data Message = Message
  { msgType        :: MsgType
  , msgFlags       :: Word8
  , msgSerial      :: Word32
  , msgPath        :: Maybe String
  , msgInterface   :: Maybe String
  , msgMember      :: Maybe String
  , msgErrorName   :: Maybe String
  , msgReplySerial :: Maybe Word32
  , msgDestination :: Maybe String
  , msgSender      :: Maybe String
  , msgBody        :: [Value]
  } deriving (Show)

emptyMessage :: MsgType -> Message
emptyMessage t = Message t 0 0 Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

renderMessage :: Message -> B.ByteString
renderMessage m =
  let (bodyB, bodyLen) = encodeMany 0 (msgBody m)
      field code v = VStruct [VByte code, VVariant v]
      fields = VArray (TStruct [TByte, TVariant]) $ concat
        [ [ field 1 (VObjectPath p) | Just p <- [msgPath m] ]
        , [ field 2 (VString i) | Just i <- [msgInterface m] ]
        , [ field 3 (VString x) | Just x <- [msgMember m] ]
        , [ field 4 (VString e) | Just e <- [msgErrorName m] ]
        , [ field 5 (VUInt32 r) | Just r <- [msgReplySerial m] ]
        , [ field 6 (VString d) | Just d <- [msgDestination m] ]
        , [ field 8 (VSignature (map typeOf (msgBody m)))  | not (null (msgBody m)) ] ]
      tcode = case msgType m of
        MethodCall -> 1; MethodReturn -> 2; ErrorMsg -> 3; Signal -> 4; OtherType x -> x
      fixed = BB.word8 (fromIntegral (fromEnum 'l')) <> BB.word8 tcode <> BB.word8 (msgFlags m) <> BB.word8 1
           <> BB.word32LE (fromIntegral bodyLen) <> BB.word32LE (msgSerial m)
      (fieldsB, end) = encode 12 fields
      pad = padTo 8 end
  in BL.toStrict (BB.toLazyByteString (fixed <> fieldsB <> mconcat (replicate pad (BB.word8 0)) <> bodyB))

-- | Total length of the message starting the buffer, once 16 bytes are in.
messageLength :: B.ByteString -> Maybe Int
messageLength bs
  | B.length bs < 16 = Nothing
  | otherwise =
      let w32 i = foldr (\k acc -> acc `shiftL` 8 .|. fromIntegral (B.index bs (i + k))) 0 [0 .. 3] :: Int
          bodyLen = w32 4
          fieldsLen = w32 12
          hdr = 16 + fieldsLen
      in Just (hdr + padTo 8 hdr + bodyLen)

parseMessage :: B.ByteString -> Either String Message
parseMessage bs
  | B.null bs || B.head bs /= fromIntegral (fromEnum 'l') = Left "only little-endian messages are supported"
  | otherwise = case runGetOrFail getMsg (BL.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, m) -> Right m
  where
    getMsg = do
      _ <- getWord8
      t <- getWord8
      flags <- getWord8
      _ <- getWord8
      bodyLen <- getWord32le
      serial <- getWord32le
      VArray _ fs <- decode (TArray (TStruct [TByte, TVariant]))
      alignGet 8
      let hf = M.fromList [ (c, v) | VStruct [VByte c, VVariant v] <- fs ]
          str c = case M.lookup c hf of
            Just (VString s) -> Just s
            Just (VObjectPath s) -> Just s
            _ -> Nothing
          sig = case M.lookup 8 hf of
            Just (VSignature ts) -> ts
            _ -> []
      body <- if bodyLen == 0 then pure [] else mapM decode sig
      pure Message
        { msgType = case t of 1 -> MethodCall; 2 -> MethodReturn; 3 -> ErrorMsg; 4 -> Signal; x -> OtherType x
        , msgFlags = flags
        , msgSerial = serial
        , msgPath = str 1
        , msgInterface = str 2
        , msgMember = str 3
        , msgErrorName = str 4
        , msgReplySerial = case M.lookup 5 hf of Just (VUInt32 r) -> Just r; _ -> Nothing
        , msgDestination = str 6
        , msgSender = str 7
        , msgBody = body
        }

------------------------------------------------------------------------------
-- Connection

data Bus = Bus
  { busSock    :: NS.Socket
  , busSerial  :: IORef Word32
  , busPending :: MVar (M.Map Word32 (MVar Message))
  , busHandler :: IORef (Message -> IO ())
  , busWrite   :: MVar ()
  , busUnique  :: IORef String
  }

busName :: Bus -> IO String
busName = readIORef . busUnique

-- | Connect to the session bus and say Hello.
connectSession :: IO Bus
connectSession = do
  addr <- lookupEnv "DBUS_SESSION_BUS_ADDRESS" >>= \case
    Just a | not (null a) -> pure a
    _ -> maybe "unix:path=/run/user/0/bus" (\d -> "unix:path=" ++ d ++ "/bus") <$> lookupEnv "XDG_RUNTIME_DIR"
  sockAddr <- case parseAddress addr of
    Just sa -> pure sa
    Nothing -> E.throwIO (userError ("unsupported D-Bus address: " ++ addr))
  sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
  NS.withFdSocket sock NS.setCloseOnExecIfNeeded
  NS.connect sock sockAddr
  uid <- getRealUserID
  let hexUid = concatMap (\c -> showHex (fromEnum c) "") (show uid)
  NSB.sendAll sock (BC.pack ("\0AUTH EXTERNAL " ++ hexUid ++ "\r\n"))
  line <- recvLine sock B.empty
  unless (BC.pack "OK" `B.isPrefixOf` line) $ E.throwIO (userError ("D-Bus authentication failed: " ++ BC.unpack line))
  NSB.sendAll sock (BC.pack "BEGIN\r\n")
  bus <- Bus sock <$> newIORef 1 <*> newMVar M.empty <*> newIORef (const (pure ())) <*> newMVar () <*> newIORef ""
  inbox <- newChan
  void $ forkIO (readerLoop bus inbox)
  void $ forkIO $ forever $ do
    m <- readChan inbox
    h <- readIORef (busHandler bus)
    E.handle (\(_ :: E.SomeException) -> pure ()) (h m)
  r <- call bus "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus" "Hello" []
  case r of
    Right [VString n] -> writeIORef (busUnique bus) n
    _ -> E.throwIO (userError "D-Bus Hello failed")
  pure bus

parseAddress :: String -> Maybe NS.SockAddr
parseAddress full = listToMaybe (mapMaybe one (splitOn ';' full))
  where
    one a = case break (== ':') a of
      ("unix", ':' : kvs) ->
        let kv = [ (k, drop 1 v) | p <- splitOn ',' kvs, let (k, v) = break (== '=') p ]
        in case (lookup "path" kv, lookup "abstract" kv) of
             (Just p, _) -> Just (NS.SockAddrUnix (unescape p))
             (_, Just p) -> Just (NS.SockAddrUnix ('\0' : unescape p))
             _ -> Nothing
      _ -> Nothing
    unescape ('%' : a : b : r) = toEnum (read ("0x" ++ [a, b])) : unescape r
    unescape (x : r) = x : unescape r
    unescape [] = []

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : r) -> a : splitOn c r
  (a, []) -> [a]

recvLine :: NS.Socket -> B.ByteString -> IO B.ByteString
recvLine s acc = case B.breakSubstring (BC.pack "\r\n") acc of
  (l, r) | not (B.null r) -> pure l
  _ -> do
    chunk <- NSB.recv s 1
    when (B.null chunk) $ E.throwIO (userError "D-Bus connection closed during authentication")
    recvLine s (acc <> chunk)

readerLoop :: Bus -> Chan Message -> IO ()
readerLoop bus inbox = go B.empty
  where
    go buf = case messageLength buf of
      Just n | B.length buf >= n -> do
        let (m, rest) = B.splitAt n buf
        case parseMessage m of
          Right msg -> dispatch msg
          Left _ -> pure ()
        go rest
      _ -> do
        chunk <- NSB.recv (busSock bus) 65536
        if B.null chunk
          then failAll
          else go (buf <> chunk)
    dispatch msg = case (msgType msg, msgReplySerial msg) of
      (t, Just rs) | t == MethodReturn || t == ErrorMsg -> do
        mv <- modifyMVar (busPending bus) (\m -> pure (M.delete rs m, M.lookup rs m))
        mapM_ (`putMVar` msg) mv
      _ -> writeChan inbox msg
    failAll = do
      pend <- swapMVar (busPending bus) M.empty
      forM_ (M.elems pend) $ \mv -> putMVar mv (emptyMessage ErrorMsg) { msgErrorName = Just "waymobar.Disconnected" }

send :: Bus -> Message -> IO Word32
send bus m = withMVar (busWrite bus) $ \_ -> do
  serial <- atomicModifyIORef' (busSerial bus) (\s -> (s + 1, s))
  NSB.sendAll (busSock bus) (renderMessage m { msgSerial = serial })
  pure serial

-- | Call a method and wait (at most 5 seconds) for the reply.
call :: Bus -> String -> String -> String -> String -> [Value] -> IO (Either String [Value])
call bus dest path iface member args = do
  mv <- newEmptyMVar
  -- register before sending so a fast reply is not lost
  serial <- modifyMVar (busPending bus) $ \pend -> withMVar (busWrite bus) $ \_ -> do
    s <- atomicModifyIORef' (busSerial bus) (\x -> (x + 1, x))
    let m = (emptyMessage MethodCall)
          { msgSerial = s, msgDestination = Just dest, msgPath = Just path
          , msgInterface = Just iface, msgMember = Just member, msgBody = args }
    NSB.sendAll (busSock bus) (renderMessage m)
    pure (M.insert s mv pend, s)
  r <- timeout 5000000 (takeMVar mv)
  case r of
    Nothing -> do
      modifyMVar_ (busPending bus) (pure . M.delete serial)
      pure (Left "timeout")
    Just m | msgType m == ErrorMsg -> pure (Left (fromMaybe "error" (msgErrorName m) ++ errText m))
           | otherwise -> pure (Right (msgBody m))
  where
    errText m = case msgBody m of
      (VString s : _) -> ": " ++ s
      _ -> ""

callNoReply :: Bus -> String -> String -> String -> String -> [Value] -> IO ()
callNoReply bus dest path iface member args = void $ send bus (emptyMessage MethodCall)
  { msgFlags = 1, msgDestination = Just dest, msgPath = Just path
  , msgInterface = Just iface, msgMember = Just member, msgBody = args }

emitSignal :: Bus -> String -> String -> String -> [Value] -> IO ()
emitSignal bus path iface member args = void $ send bus (emptyMessage Signal)
  { msgPath = Just path, msgInterface = Just iface, msgMember = Just member, msgBody = args }

reply :: Bus -> Message -> [Value] -> IO ()
reply bus m args = unless (msgFlags m `mod` 2 == 1) $ void $ send bus (emptyMessage MethodReturn)
  { msgReplySerial = Just (msgSerial m), msgDestination = msgSender m, msgBody = args }

replyError :: Bus -> Message -> String -> String -> IO ()
replyError bus m name text = void $ send bus (emptyMessage ErrorMsg)
  { msgReplySerial = Just (msgSerial m), msgDestination = msgSender m
  , msgErrorName = Just name, msgBody = [VString text] }

-- | Receive method calls and signals (run on one dispatcher thread; it may
-- make calls).
setHandler :: Bus -> (Message -> IO ()) -> IO ()
setHandler bus = writeIORef (busHandler bus)

addMatch :: Bus -> String -> IO ()
addMatch bus rule = void $ call bus "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus" "AddMatch" [VString rule]

-- | Request a well-known name; True if we are now its primary owner.
requestName :: Bus -> String -> Word32 -> IO Bool
requestName bus name flags = do
  r <- call bus "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus" "RequestName" [VString name, VUInt32 flags]
  pure $ case r of
    Right [VUInt32 1] -> True
    Right [VUInt32 4] -> True
    _ -> False

------------------------------------------------------------------------------
-- Helpers

fromVariant :: Value -> Value
fromVariant (VVariant v) = fromVariant v
fromVariant v = v

-- | Look a key up in an @a{sv}@.
dictLookup :: String -> Value -> Maybe Value
dictLookup k (VArray _ es) = listToMaybe [ fromVariant v | VDictEntry (VString k') v <- es, k' == k ]
dictLookup _ _ = Nothing

valueString :: Value -> Maybe String
valueString v = case fromVariant v of
  VString s -> Just s
  VObjectPath s -> Just s
  _ -> Nothing

valueBool :: Value -> Maybe Bool
valueBool v = case fromVariant v of
  VBool b -> Just b
  _ -> Nothing

valueInt :: Value -> Maybe Int
valueInt v = case fromVariant v of
  VInt32 i -> Just (fromIntegral i)
  VUInt32 i -> Just (fromIntegral i)
  VInt16 i -> Just (fromIntegral i)
  VUInt16 i -> Just (fromIntegral i)
  VInt64 i -> Just (fromIntegral i)
  VUInt64 i -> Just (fromIntegral i)
  VByte i -> Just (fromIntegral i)
  _ -> Nothing

utf8 :: String -> B.ByteString
utf8 = TE.encodeUtf8 . T.pack

fromUtf8 :: B.ByteString -> String
fromUtf8 = T.unpack . TE.decodeUtf8Lenient

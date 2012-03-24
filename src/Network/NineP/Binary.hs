{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------

-- Module      : Network.NineP.Binary
-- Copyright   : Evgeny I. E. Omelchenko, Tim Newsham
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Evgeny I. E. Omelchenko
-- Stability   : experimental
--
-- Module providing Binary serialization of 9P messages to and from lazy
-- ByteStrings.
-- 
-- This library does not currently provide any networking support or
-- wrappers for easy to write clients or servers, though that may come
-- with time as we decide the best way to implement these.
-- 
-- 9P2000 messages are sent in little endian byte order rather than 
-- network byte order (big endian)
-----------------------------------------------------------------------------

module Network.NineP.Binary
    ( Bin(..)
    , Qid(..)
    , Stat(..)
    , Msg(..)
    , MsgBody(..)
    , TMsgBody(..)
    , RMsgBody(..)
    ) where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (replicateM)
import Data.Char (chr, ord)
import Data.Word (Word8, Word16, Word32, Word64)

import Data.Binary.Get
    ( Get, runGet, isEmpty, remaining, skip
    , getWord8, getWord16le, getWord32le, getWord64le, getLazyByteString
    , getRemainingLazyByteString
    )
import Data.Binary.Put
    ( Put, runPut
    , putWord8, putWord16le, putWord32le, putWord64le, putLazyByteString
    )

import qualified Data.ByteString.Lazy as L

class Bin a where
    get :: Get a
    put :: a -> Put

class MsgBody a where
    type MsgType a
    getMsgBody  :: MsgType a -> Get a
    putMsgBody  :: a -> Put
    msgBodyType :: a -> MsgType a

data Qid = Qid
    { qid_typ  :: Word8
    , qid_vers :: Word32
    , qid_path :: Word64 
    } deriving (Show, Eq)

data Msg a 
    = Msg { msg_tag  :: Word16
          , msg_body :: a
          } deriving (Show, Eq)

data Stat = Stat
    { st_typ    :: Word16
    , st_dev    :: Word32
    , st_qid    :: Qid
    , st_mode   :: Word32
    , st_atime  :: Word32
    , st_mtime  :: Word32
    , st_length :: Word64
    , st_name   :: String
    , st_uid    :: String
    , st_gid    :: String
    , st_muid   :: String 
    } deriving (Show, Eq)

data TMsgBody
    = Tversion
        { tv_msize   :: Word32
        , tv_version :: String 
        }
    | Tauth
        { tau_afid  :: Word32
        , tau_uname :: String
        , tau_aname :: String
        }
    | Tflush
        { tf_oldtag :: Word16 }
    | Tattach 
        { tat_fid :: Word32
        , tat_afid :: Word32
        , tat_uname :: String
        , tat_aname :: String
        }
    | Twalk
        { tw_fid :: Word32
        , tw_newfid :: Word32
        , tw_wnames :: [String]
        }
    | Topen
        { to_fid :: Word32
        , to_mode :: Word8
        }
    | Tcreate
        { tcr_fid :: Word32
        , tcr_name :: String
        , tcr_perm :: Word32
        , tcr_mode :: Word8
        }
    | Tread
        { trd_fid :: Word32
        , trd_offset :: Word64
        , trd_count :: Word32
        }
    | Twstat
        { tws_fid :: Word32
        , tws_stat :: [Stat]
        }
    | Twrite
        { twr_fid :: Word32
        , twr_offset :: Word64
        , twr_dat :: L.ByteString 
        }
    | Tclunk
        { tcl_fid :: Word32 }
    | Tstat
        { ts_fid :: Word32 }
    | Tremove
        { trm_fid :: Word32 }
    deriving (Show, Eq)

data RMsgBody
    = Rversion
        { rv_msize :: Word32
        , rv_version :: String
        }
    | Rauth { ra_aqid :: Qid }
    | Rerror { re_ename :: String }
    | Rflush 
    | Rattach { rat_qid :: Qid }
    | Rwalk { rw_wqid :: [Qid] }
    | Ropen
        { ro_qid :: Qid
        , ro_iounit :: Word32
        }
    | Rcreate
        { rcr_qid :: Qid
        , rcr_iounit :: Word32
        }
    | Rread { rrd_dat :: L.ByteString }
    | Rwrite { rw_count :: Word32 }
    | Rclunk
    | Rremove
    | Rstat { rs_stat :: [Stat] }
    | Rwstat
    deriving (Show, Eq)

data TMsgType
    = TTversion | TTauth | TTattach | XXX_TTerror | TTflush | TTwalk
    | TTopen | TTcreate | TTread | TTwrite | TTclunk | TTremove 
    | TTstat | TTwstat
    deriving (Show, Eq, Ord, Enum)

data RMsgType
    = TRversion | TRauth | TRattach | TRerror | TRflush | TRwalk
    | TRopen | TRcreate | TRread | TRwrite | TRclunk | TRremove 
    | TRstat | TRwstat
    deriving (Show, Eq, Ord, Enum)

instance Bin Word8 where
    get = getWord8
    put = putWord8

instance Bin Word16 where
    get = getWord16le
    put = putWord16le

instance Bin Word32 where
    get = getWord32le
    put = putWord32le

instance Bin Word64 where
    get = getWord64le
    put = putWord64le

instance Bin Char where
    get = chr . fromIntegral <$> getWord8
    put = putWord8 . fromIntegral . ord

instance Bin String where
    get = getWord16le >>= \n -> replicateM (fromIntegral n) get
    put xs = putWord16le (fromIntegral $ length xs) >> mapM_ put xs

instance Bin Qid where
    get = Qid <$> get <*> get <*> get
    put (Qid t v p) = put t >> put v >> put p

instance Bin Stat where
    get = do
        n <- getWord16le
        getNest n g
      where
        g = Stat <$> get <*> get <*> get <*> get <*> get <*> get <*> get 
                 <*> get <*> get <*> get <*> get

    put (Stat a b c d e f g h i j k) = do
        let buf = runPut p
        putWord16le $ fromIntegral $ L.length buf
        putLazyByteString buf
      where 
        p  = put a >> put b >> put c >> put d >> put e >> put f >> put g 
                   >> put h >> put i >> put j >> put k

instance Bin TMsgType where
    get =
      do
        n <- getWord8
        -- 106 == _tTerror
        return $ if n >= 100 && (n `mod` 2 == 0) && n < 128 && n /= 106 
                    then toEnum . fromEnum . (`div` 2) . subtract 100 $ n
                    else error $ "invalid tag: " ++ (show n)
    put = putWord8 . toEnum . (+ 100) . (* 2) . fromEnum

instance Bin RMsgType where
    get =
      do
        n <- getWord8
        return $ if n >= 100 && (n `mod` 2 == 1) && n < 128
                    then toEnum . fromEnum . (`div` 2) . subtract 101 $ n
                    else error $ "invalid tag: " ++ (show n)
    put = putWord8 . toEnum . (+ 101) . (* 2) . fromEnum

instance (MsgBody a, Bin (MsgType a)) => Bin (Msg a) where
    get =
      do
        sz <- getWord32le
        if sz < 4 || sz > maxSize
          then return $ error $ "Invalid size: " ++ show sz
          else getNest (sz - 4) $ do
            typ <- get
            tag <- getWord16le
            body <- getMsgBody typ
            return $ Msg tag body

    put (Msg tag body) =
      do
        let typ = msgBodyType body
            buf = runPut (put typ >> put tag >> putMsgBody body)
        putWord32le $ fromIntegral $ L.length buf + 4
        putLazyByteString buf

instance MsgBody TMsgBody where
    type MsgType TMsgBody = TMsgType

    getMsgBody TTversion = Tversion <$> get <*> get
    getMsgBody TTauth = Tauth <$> get <*> get <*> get
    getMsgBody XXX_TTerror = error "there is no Terror"
    getMsgBody TTflush = Tflush <$> get
    getMsgBody TTattach = Tattach <$> get <*> get <*> get <*> get
    getMsgBody TTwalk = Twalk <$> get <*> get <*> getList16
    getMsgBody TTopen = Topen <$> get <*> get
    getMsgBody TTcreate = Tcreate <$> get <*> get <*> get <*> get
    getMsgBody TTread = Tread <$> get <*> get <*> get
    getMsgBody TTwrite = Twrite <$> get <*> get <*> getBytes32
    getMsgBody TTclunk = Tclunk <$> get
    getMsgBody TTremove = Tremove <$> get
    getMsgBody TTstat = Tstat <$> get
    getMsgBody TTwstat = Twstat <$> get <*> getNestList16

    putMsgBody (Tversion a b) = put a >> put b
    putMsgBody (Tauth a b c) = put a >> put b >> put c
    putMsgBody (Tflush a) = put a
    putMsgBody (Tattach a b c d) = put a >> put b >> put c >> put d
    putMsgBody (Twalk a b c) = put a >> put b >> putList16 c
    putMsgBody (Topen a b) = put a >> put b
    putMsgBody (Tcreate a b c d) = put a >> put b >> put c >> put d
    putMsgBody (Tread a b c) = put a >> put b >> put c
    putMsgBody (Twrite a b c) = put a >> put b >> putBytes32 c
    putMsgBody (Tclunk a) = put a
    putMsgBody (Tremove a) = put a
    putMsgBody (Tstat a) = put a
    putMsgBody (Twstat a b) = put a >> putNestList16 b

    msgBodyType (Tversion _ _) = TTversion
    msgBodyType (Tauth _ _ _) = TTauth
    msgBodyType (Tflush _) = TTflush
    msgBodyType (Tattach _ _ _ _) = TTattach
    msgBodyType (Twalk _ _ _) = TTwalk
    msgBodyType (Topen _ _) = TTopen
    msgBodyType (Tcreate _ _ _ _) = TTcreate
    msgBodyType (Tread _ _ _) = TTread
    msgBodyType (Twrite _ _ _) = TTwrite
    msgBodyType (Tclunk _) = TTclunk
    msgBodyType (Tremove _) = TTremove
    msgBodyType (Tstat _) = TTstat
    msgBodyType (Twstat _ _) = TTwstat

instance MsgBody RMsgBody where
    type MsgType RMsgBody = RMsgType

    getMsgBody TRversion = Rversion <$> get <*> get
    getMsgBody TRauth = Rauth <$> get
    getMsgBody TRerror = Rerror <$> get
    getMsgBody TRflush = return Rflush
    getMsgBody TRattach = Rattach <$> get
    getMsgBody TRwalk = Rwalk <$> getList16
    getMsgBody TRopen = Ropen <$> get <*> get
    getMsgBody TRcreate = Rcreate <$> get <*> get
    getMsgBody TRread = Rread <$> getBytes32
    getMsgBody TRwrite = Rwrite <$> get
    getMsgBody TRclunk = return Rclunk
    getMsgBody TRremove = return Rremove
    getMsgBody TRstat = Rstat <$> getNestList16
    getMsgBody TRwstat = return Rwstat

    msgBodyType (Rversion _ _) = TRversion
    msgBodyType (Rauth _) = TRauth
    msgBodyType (Rflush) = TRflush
    msgBodyType (Rattach _) = TRattach
    msgBodyType (Rerror _) = TRerror
    msgBodyType (Rwalk _) = TRwalk
    msgBodyType (Ropen _ _) = TRopen
    msgBodyType (Rcreate _ _) = TRcreate
    msgBodyType (Rread _) = TRread
    msgBodyType (Rwrite _) = TRwrite
    msgBodyType (Rclunk) = TRclunk
    msgBodyType (Rremove) = TRremove
    msgBodyType (Rstat _) = TRstat
    msgBodyType (Rwstat) = TRwstat

    putMsgBody (Rversion a b) = put a >> put b
    putMsgBody (Rauth a) = put a
    putMsgBody (Rerror a) = put a
    putMsgBody (Rflush) = return ()
    putMsgBody (Rattach a) = put a
    putMsgBody (Rwalk a) = putList16 a
    putMsgBody (Ropen a b) = put a >> put b
    putMsgBody (Rcreate a b) = put a >> put b
    putMsgBody (Rread a) = putBytes32 a
    putMsgBody (Rwrite a) = put a
    putMsgBody (Rclunk) = return ()
    putMsgBody (Rremove) = return ()
    putMsgBody (Rstat a) = putNestList16 a
    putMsgBody (Rwstat) = return ()

getNest' :: Get a -> Get a
getNest' g =
  do
    x <- g
    e <- isEmpty
    if e
      then return x
      else do
        n <- remaining
        error $ show n ++ " extra bytes in nested structure"

getNest :: Integral n => n -> Get a -> Get a
getNest sz g =
  do
    b' <- getRemainingLazyByteString
    let result = flip runGet (L.take (fromIntegral sz) b') $ getNest' g
    skip $ fromIntegral sz
    return result

getListAll :: (Bin a) => Get [a]
getListAll =
  do
    e <- isEmpty
    if e 
      then return [] 
      else (:) <$> get <*> getListAll

putListAll :: (Bin a) => [a] -> Put
putListAll = mapM_ put

getNestList16 :: (Bin a) => Get [a]
getNestList16 =
  do
    n <- getWord16le
    getNest n getListAll

putNestList16 :: Bin a => [a] -> Put
putNestList16 xs =
  do
    let buf = runPut (putListAll xs)
    putWord16le $ fromIntegral $ L.length buf
    putLazyByteString buf

getList16 :: Bin a => Get [a]
getList16 = getWord16le >>= \n -> replicateM (fromIntegral n) get

putList16 :: Bin a => [a] -> Put
putList16 xs = putWord16le (fromIntegral $ length xs) >> mapM_ put xs

getBytes32 :: Get L.ByteString
getBytes32 = getWord32le >>= getLazyByteString . fromIntegral

putBytes32 :: L.ByteString -> Put
putBytes32 xs = putWord32le (fromIntegral $ L.length xs) 
              >> putLazyByteString xs

maxSize :: Word32
maxSize = 1024 * 1024 -- XXX arbitrary, configured?


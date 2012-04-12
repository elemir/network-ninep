{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, MultiParamTypeClasses #-}
module Network.NineP.Server.Utils
    ( getQid, fromStat, toWStat, toMode, walk, sendRMsg, convertPerm
    ) where

import Prelude hiding (lookup)

import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO(..))
import Data.Bits (shiftL, complement, (.&.), (.|.))
import Data.Word (Word8, Word16, Word32, Word64)
import System.IO (Handle, hFlush)

import qualified Data.Map as M

import Data.Accessor(Accessor, (^=))
import Data.Binary.Put (runPut)
import Data.Time(UTCTime)
import Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

import qualified Data.ByteString.Lazy as L

import Data.NineP.Mode
import Data.NineP.Stat
import Network.NineP.Server.File.Internal

import qualified Network.NineP.Binary as B

class Convertable a b where
    convert :: a -> b

(^>=) :: Convertable a b => Accessor r b -> a -> r -> r
(^>=) t a = t ^= convert a

instance Convertable Word32 (Maybe Permission) where
    convert w | w == maxBound = Nothing
              | True = Just w

instance Convertable Word32 (Maybe UTCTime) where
    convert w | w == maxBound = Nothing
              | True = Just $ posixSecondsToUTCTime . fromIntegral $ w

instance Convertable Word64 (Maybe Word64) where
    convert w | w == maxBound = Nothing
              | True = Just w

instance Convertable String (Maybe String) where
    convert s | s == "" = Nothing
              | True = Just s

sendRMsg :: MonadIO m => Handle -> Word16 -> B.RMsgBody -> m ()
sendRMsg h tag msgBody =
    liftIO $ L.hPutStr h (runPut $ B.put $ B.Msg tag msgBody) >> hFlush h

getQid :: File a s => a -> NineP s B.Qid
getQid file =
  do
    path <- qidPath file
    ver <- qidVersion file
    stat' <- stat file
    return $ B.Qid (fromIntegral $ (st_perm stat') `shiftL` 24) ver path

convertPerm :: File a s => a -> Word32 -> NineP s Permission
convertPerm dir perm =
  do
    let perm' = fromIntegral perm
    let mask = if (perm' .&. permDir /= 0) then 0o666 else 0o777
    let comask = complement mask
    stat' <- stat dir
    return $ perm' .&. (comask .|. (st_perm stat' .&. mask))

toWStat :: B.Stat -> WStat
toWStat (B.Stat _ _ _ mode atime mtime size name uid gid muid) =
    wst_perm ^>= mode $ wst_size ^>= size
    $ wst_atime ^>= atime $ wst_mtime ^>= mtime
    $ wst_name ^>= name $ wst_uid ^>= uid 
    $ wst_gid ^>= gid $ wst_muid ^>= muid
    $ emptyWStat

toMode :: Word8 -> Mode
toMode mode =
    let m = fromIntegral mode 
     in fromIntegral $ (1 `shiftL` (m .&. 0xF)) .|. (m .&. (complement 0xF))

fromStat :: File' s -> NineP s B.Stat
fromStat (File' file _) =
  do
    Stat mode atime mtime size name uid guid muid <- stat file
    qid <- getQid file
    return $ B.Stat 0 0 qid mode
                    (liftM round utcTimeToPOSIXSeconds atime)
                    (liftM round utcTimeToPOSIXSeconds mtime)
                    size name uid guid muid

walk :: File a s => a -> [String] -> NineP s (File' s, [B.Qid])
walk file (wname : wnames) = do
  mp <- lookup file
  case M.lookup wname mp of
    Just (File' f _) -> do
      (rf, qids) <- walk f wnames
      qid <- getQid f
      return (rf, qid : qids)
    Nothing -> return (file' file, [])
walk file [] = return (file' file, [])


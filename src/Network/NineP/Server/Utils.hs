{-# LANGUAGE NoImplicitPrelude #-}
module Network.NineP.Server.Utils
    ( getQid, binStat, fromBinStat, fromBinOMode, walk, sendRMsg
    ) where

import Prelude hiding (lookup)

import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO(..))
import Data.Bits (shiftL, complement, (.&.), (.|.))
import Data.Word (Word8, Word16)
import System.IO (Handle, hFlush)

import qualified Data.Map as M

import Data.Binary.Put (runPut)
import Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

import qualified Data.ByteString.Lazy as L

import Network.NineP.Server.File.Internal

import qualified Network.NineP.Binary as B

sendRMsg :: MonadIO m => Handle -> Word16 -> B.RMsgBody -> m ()
sendRMsg h tag msgBody =
    liftIO $ L.hPutStr h (runPut $ B.put $ B.Msg tag msgBody) >> hFlush h

getQid :: File a s => a -> NineP s B.Qid
getQid file =
  do
    path <- qidPath file
    ver <- qidVersion file
    stat' <- stat file
    return $ B.Qid (fromIntegral $ (st_mode stat') `shiftL` 24) ver path

fromBinStat :: B.Stat -> Stat
fromBinStat (B.Stat _ _ _ mode atime mtime size name uid gid muid) =
    Stat mode
         (posixSecondsToUTCTime . fromIntegral $ atime)
         (posixSecondsToUTCTime . fromIntegral $ mtime)
         size name uid gid muid 

fromBinOMode :: Word8 -> OMode
fromBinOMode mode =
    let m = fromIntegral mode 
     in fromIntegral $ (1 `shiftL` (m .&. 0xF)) .|. (m .&. (complement 0xF))

binStat :: File' s -> NineP s B.Stat
binStat (File' file _) = 
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


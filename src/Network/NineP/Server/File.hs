{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.NineP.Server.File
    ( Directory(..), directory'
    , StreamReaderFile(..), srfile'
    , StreamWriterFile(..), swfile'
    , module Network.NineP.Server.File.Internal
    ) where

import Prelude hiding (read, lookup)

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Monad (liftM)
import Control.Monad.Error (throwError)
import Control.Monad.State (get, put)
import Control.Monad.Trans (liftIO)
import Data.Word(Word64)
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.Map as M

import Control.Concurrent.MState (forkM, waitM)
import Data.Bits ((.|.), (.&.))
import Data.Binary.Put (runPut)

import qualified Data.ByteString.Lazy as L

import Network.NineP.Server.Error
import Network.NineP.Server.File.Internal hiding (lookup, create)
import Network.NineP.Server.Utils

import qualified Network.NineP.Binary as B
import qualified Network.NineP.Server.File.Internal

class CommonFile a s => Directory a s where
    lookup :: a -> NineP s (M.Map String (File' s))
    create  :: a -> String -> DMode -> NineP s (File' s)

class CommonFile a s => StreamReaderFile a s where
    content :: a -> OMode -> NineP s L.ByteString

class CommonFile a s => StreamWriterFile a s where
    hook :: a -> OMode -> NineP s (L.ByteString -> NineP s ())

newtype SRFWrap a = SRFWrap a

newtype SWFWrap a = SWFWrap a

newtype DirWrap a = DirWrap a

instance CommonFile a s => CommonFile (SRFWrap a) s where
    qidPath (SRFWrap f)    = qidPath f
    qidVersion (SRFWrap f) = qidVersion f 
    stat (SRFWrap f)       = stat f
    remove (SRFWrap f)     = remove f
    wstat (SRFWrap f)      = wstat f
    parent (SRFWrap f)     = parent f

instance CommonFile a s => CommonFile (SWFWrap a) s where
    qidPath (SWFWrap f)    = qidPath f
    qidVersion (SWFWrap f) = qidVersion f 
    stat (SWFWrap f)       = stat f
    remove (SWFWrap f)     = remove f
    wstat (SWFWrap f)      = wstat f
    parent (SWFWrap f)     = parent f

instance CommonFile a s => CommonFile (DirWrap a) s where
    qidPath (DirWrap f)    = qidPath f
    qidVersion (DirWrap f) = qidVersion f 
    stat (DirWrap f)       = stat f
    remove (DirWrap f)     = remove f
    wstat (DirWrap f)      = wstat f
    parent (DirWrap f)     = parent f

instance StreamReaderFile a s => File (SRFWrap a) s where
    type FileData (SRFWrap a) s = L.ByteString

    open (SRFWrap file) mode =
      if (mode .&. (oWrite .|. oReadWrite .|. oTruncate) /= 0)
        then throwError ErrReadOnly
        else content file mode

    read _ offset size =
      do
        buf <- get
        return $ L.take (fromIntegral size) $ L.drop (fromIntegral offset) buf

    write _ _ _ = throwError ErrReadOnly

    clunk _ _ = return ()

instance StreamWriterFile a s => File (SWFWrap a) s where
    type FileData (SWFWrap a) s = (ThreadId, Chan (Maybe L.ByteString), Word64)

    open (SWFWrap file) mode =
      if (mode .&. (oRead .|. oReadWrite .|. oExec) /= 0)
        then throwError ErrOpPerm
        else
          let getMaybeChanContents chan =
                unsafeInterleaveIO $ do
                  xm <- readChan chan
                  case xm of
                    Nothing -> return []
                    Just x -> do 
                      xs <- getMaybeChanContents chan
                      return (x:xs)
              writerThread chan = do
                chanContents <- liftIO $ getMaybeChanContents chan
                f <- hook file mode
                f $ L.concat chanContents
           in do
             chan <- liftIO newChan
             tid <- forkM $ writerThread chan
             return (tid, chan, 0)

    read _ _ _ = throwError ErrOpPerm

    write _ seek buf =
      do
        (tid, chan, offset) <- get
        let len = fromIntegral $ L.length buf
        put (tid, chan, offset + fromIntegral len)
        if (seek /= offset)
          then throwError ErrIllSeek 
          else do
            liftIO $ writeChan chan $ Just buf
            return len

    clunk _ Nothing = return ()
    clunk _ (Just (tid, chan, _)) = 
      (liftIO $ writeChan chan Nothing) >> waitM tid
                                        >> (liftIO $ killThread tid)

instance Directory a s => File (DirWrap a) s where
    type FileData (DirWrap a) s = (L.ByteString, Word64)

    open (DirWrap dir) mode =
      if (mode .&. (oWrite .|. oReadWrite .|. oTruncate) /= 0)
        then throwError ErrReadOnly
        else do
          mp <- lookup dir
          par' <- parent dir 
          let modifyName name st = st { B.st_name = name }
          let dot = (".", directory' dir)
          let dotdot = ("..", par')
          stat' <- mapM (\(name, file) -> liftM (modifyName name) 
                                              (binStat file)) 
                                              $ dot : dotdot : M.toList mp
          return (runPut . (mapM_ B.put) $ stat', 0)

    read _ seek size = do
      (buf, offset) <- get
      if (seek /= offset)
          then throwError ErrDirOffset
          else do
            let buf' = L.take (fromIntegral size)
                         $ L.drop (fromIntegral offset) buf
            let len = fromIntegral . L.length $ buf'
            put (buf, seek + len)
            return buf'

    write _ _ _ = throwError ErrReadOnly

    clunk _ _ = return ()

    lookup (DirWrap f) = lookup f

    create (DirWrap f) = create f

srfile' :: StreamReaderFile a s => a -> File' s
srfile' = file' . SRFWrap

swfile' :: StreamWriterFile a s => a -> File' s
swfile' = file' . SWFWrap

directory' :: Directory a s => a -> File' s
directory'  = file' . DirWrap


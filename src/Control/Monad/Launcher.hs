{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, Rank2Types #-}
module Control.Monad.Launcher
    ( Launcher
    , setVersion, checkVersion
    , withThreadIdDo, putThreadId, clunkTag
    , withFileDo, withoutFileDo, putFile, clunkFile
    ) where
import Control.Concurrent (ThreadId)
import Control.Monad.Except(throwError)
import Control.Monad.State(get, put, modify)
import Data.Word (Word16, Word32)
import System.IO (Handle)

import qualified Data.Map as M

import Data.Accessor
import Data.Accessor.Tuple

import Control.Concurrent.MState (MState)

import Network.NineP.Server.File.Internal
import Network.NineP.Server.Error
import Network.NineP.Server.Utils

import qualified Network.NineP.Binary as B

type LauncherState s
  = (Maybe String, M.Map Word32 (File' s), M.Map Word16 ThreadId)

type Launcher s a
  = MState (LauncherState s) (NineP s) a

setVersion :: Handle -> Word16 -> Word32 
                    -> Launcher s (Maybe String) -> Launcher s ()
setVersion h tag sz f =
  do
    mver <- f
    case mver of
      Nothing ->
        sendRMsg h tag $ B.Rversion sz "unknown"
      Just ver -> do
        put (mver, M.empty, M.empty)
        sendRMsg h tag $ B.Rversion sz ver

checkVersion :: Launcher s () -> Launcher s ()
checkVersion f =
  do
    (ver, _, _) <- get
    if (ver == Nothing)
      then throwError ErrProto
      else f

withThreadIdDo :: Word16 -> (ThreadId -> Launcher s ()) -> Launcher s ()
withThreadIdDo tag f = 
  do
    (_, _, mp) <- get
    case M.lookup tag mp of
      Just tid -> f tid
      Nothing -> throwError ErrProto

putThreadId :: Word16 -> ThreadId -> Launcher s ()
putThreadId tag tid =
  modify $ third3 ^: M.insert tag tid

clunkTag :: Word16 -> Launcher s ()
clunkTag tag =
  modify $ third3 ^: M.delete tag

withFileDo :: Word32 -> (File' s -> Launcher s ()) -> Launcher s ()
withFileDo fid f =
  do
    (_, mp, _) <- get
    case M.lookup fid mp of
      Just file -> f file
      Nothing -> throwError ErrFidUnknown

withoutFileDo :: Word32 -> (Launcher s ()) -> Launcher s ()
withoutFileDo fid f = 
  do
    (_, mp, _) <- get
    case M.lookup fid mp of
      Just _  -> throwError ErrFidInUse 
      Nothing -> f

putFile :: Word32 -> File' s -> Launcher s ()
putFile fid file = 
  modify $ second3 ^: M.insert fid file

clunkFile :: Word32 -> Launcher s ()
clunkFile fid =
  modify $ second3 ^: M.delete fid



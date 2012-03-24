{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}
module Network.NineP.Server.File.Internal
    ( NineP, NinePST
    , DMode, dmDir, dmAppend, dmExcl, dmAuth, dmTmp
    , OMode, oRead, oWrite, oReadWrite, oExec, oTruncate, oRClose
    , Stat(..)
    , CommonFile(..), File(..)
    , File'(..), file'
    ) where

import Prelude hiding (read)

import Control.Monad.State(StateT(..))
import Control.Monad.Error(ErrorT(..))
import Data.Bits(shiftL)
import Data.Word(Word64, Word32, Word8)

import qualified Data.Map as M

import Data.Time(UTCTime)
import Control.Concurrent.MState (MState)

import qualified Data.ByteString.Lazy as L

import Network.NineP.Server.Error

type NineP t = MState t (ErrorT NinePError IO)
type NinePST s t = StateT s (MState t (ErrorT NinePError IO))

type DMode = Word32

dmDir    :: DMode
dmAppend :: DMode
dmExcl   :: DMode
dmAuth   :: DMode
dmTmp    :: DMode

dmDir    = shiftL 1 31
dmAppend = shiftL 1 30
dmExcl   = shiftL 1 29
dmAuth   = shiftL 1 27
dmTmp    = shiftL 1 26

type OMode = Word8

oRead      :: OMode
oWrite     :: OMode
oReadWrite :: OMode
oExec      :: OMode
oTruncate  :: OMode
oRClose    :: OMode

oRead      = 1
oWrite     = 2
oReadWrite = 4
oExec      = 8
oTruncate  = 0x10
oRClose    = 0x40

data Stat = Stat
    { st_mode    :: !DMode
    , st_atime   :: !UTCTime
    , st_mtime   :: !UTCTime
    , st_size    :: !Word64
    , st_name    :: !String
    , st_uid     :: !String
    , st_gid     :: !String
    , st_muid    :: !String
    } deriving (Show, Eq)

class CommonFile a s where
    qidPath     :: a -> NineP s Word64
    qidVersion  :: a -> NineP s Word32
    stat        :: a -> NineP s Stat
    remove      :: a -> NineP s ()
    parent      :: a -> NineP s (File' s)
    create      :: a -> String -> DMode -> OMode -> NineP s (File' s)
    wstat       :: a -> Stat -> NineP s ()

class CommonFile a s => File a s where
    type FileData a s
    lookup :: a -> NineP s (M.Map String (File' s))
    lookup _ = return M.empty
    open    :: a -> OMode -> NineP s (FileData a s)
    read    :: a -> Word64 -> Word32 -> NinePST (FileData a s) s L.ByteString
    write   :: a -> Word64 -> L.ByteString -> NinePST (FileData a s) s Word32
    clunk   :: a -> Maybe (FileData a s) -> NineP s ()

data File' s = forall a.(File a s) => File' a (Maybe (FileData a s))

file' :: File a s => a -> File' s
file' file = File' file Nothing


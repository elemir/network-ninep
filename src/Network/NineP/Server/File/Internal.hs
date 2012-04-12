{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}
module Network.NineP.Server.File.Internal
    ( NineP, NinePST
    , CommonFile(..), File(..)
    , File'(..), file'
    ) where

import Prelude hiding (read)

import Control.Monad.State(StateT(..))
import Control.Monad.Error(ErrorT, throwError)
import Data.Word(Word64, Word32)

import qualified Data.Map as M

import Control.Concurrent.MState (MState)

import qualified Data.ByteString.Lazy as L

import Data.NineP.Mode
import Data.NineP.Stat
import Network.NineP.Server.Error

type NineP t = MState t (ErrorT NinePError IO)
type NinePST s t = StateT s (MState t (ErrorT NinePError IO))

class CommonFile a s where
    qidPath     :: a -> NineP s Word64
    qidVersion  :: a -> NineP s Word32
    stat        :: a -> NineP s Stat
    remove      :: a -> NineP s ()
    parent      :: a -> NineP s (File' s)
    wstat       :: a -> WStat -> NineP s ()

class CommonFile a s => File a s where
    type FileData a s
    lookup :: a -> NineP s (M.Map String (File' s))
    lookup _ = return M.empty
    create  :: a -> String -> Permission -> NineP s (File' s)
    create _ _ _ = throwError ErrFidBadUse
    open    :: a -> Mode -> NineP s (FileData a s)
    read    :: a -> Word64 -> Word32 -> NinePST (FileData a s) s L.ByteString
    write   :: a -> Word64 -> L.ByteString -> NinePST (FileData a s) s Word32
    clunk   :: a -> Maybe (FileData a s) -> NineP s ()

data File' s = forall a.(File a s) => File' a (Maybe (FileData a s))

file' :: File a s => a -> File' s
file' file = File' file Nothing


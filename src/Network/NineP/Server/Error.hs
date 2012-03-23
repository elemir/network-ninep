module Network.NineP.Server.Error
    ( NinePError(..), toString
    ) where

import Control.Monad.Error(Error(..))

data NinePError = ErrProto | ErrRootRemove | ErrWstatProhibited | ErrNoSuchFile
                | ErrFidInUse | ErrFidUnknown | ErrReadOnly | ErrOpPerm 
                | ErrIllSeek | ErrDirOffset | ErrInvalArg
                | NoAuthRequired | StrError String

instance Error NinePError where
    noMsg = undefined
    strMsg = StrError

toString :: NinePError -> String
toString ErrProto = "Protocol error"
toString ErrNoSuchFile = "No such file or directory"
toString ErrFidInUse = "fid already in use"
toString ErrFidUnknown = "fid unknown or out of range"
toString ErrWstatProhibited = "wstat prohibited"
toString ErrRootRemove = "cannot remove root"
toString ErrReadOnly = "file is read only"
toString ErrOpPerm = "Operation not permitted"
toString ErrIllSeek = "Illegal seek"
toString ErrDirOffset = "bad offset in directory read"
toString ErrInvalArg = "Invalid argument"
toString NoAuthRequired = "u9fs authnone: no authentication required"
toString (StrError str) = str

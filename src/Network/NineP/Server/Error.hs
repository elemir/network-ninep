module Network.NineP.Server.Error
    (NinePError(..)) where

data NinePError = ErrProto | ErrRootRemove | ErrWstatProhibited | ErrNoSuchFile
                | ErrFidInUse | ErrFidUnknown | ErrReadOnly | ErrOpPerm 
                | ErrIllSeek | ErrDirOffset | ErrInvalArg | ErrFidBadUse
                | ErrExists | NoAuthRequired | StrError String

instance Show NinePError where
    show ErrProto = "Protocol error"
    show ErrNoSuchFile = "No such file or directory"
    show ErrFidInUse = "fid already in use"
    show ErrFidUnknown = "fid unknown or out of range"
    show ErrWstatProhibited = "wstat prohibited"
    show ErrRootRemove = "cannot remove root"
    show ErrReadOnly = "file is read only"
    show ErrOpPerm = "Operation not permitted"
    show ErrExists = "File exists"
    show ErrIllSeek = "Illegal seek"
    show ErrDirOffset = "bad offset in directory read"
    show ErrFidBadUse = "bad use of fid"
    show ErrInvalArg = "Invalid argument"
    show NoAuthRequired = "u9fs authnone: no authentication required"
    show (StrError str) = str

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module Data.NineP.Stat
    ( Permission, permDir, permAppend, permExcl, permAuth, permTmp
    , Stat(..)
    , WStat(WStat), emptyWStat, wst_perm, wst_atime, wst_mtime, wst_size 
    , wst_name, wst_uid, wst_gid, wst_muid
    , (^?=)
    ) where
import Data.Word (Word32, Word64)
import Data.Bits(shiftL)

import Data.Accessor(Accessor, (^=))
import Data.Accessor.Template(deriveAccessors)
import Data.Time(UTCTime)

type Permission = Word32

permDir    :: Permission
permDir    = shiftL 1 31

permAppend :: Permission
permAppend = shiftL 1 30

permExcl   :: Permission
permExcl   = shiftL 1 29

permAuth   :: Permission
permAuth   = shiftL 1 27

permTmp    :: Permission
permTmp    = shiftL 1 26

data Stat = Stat
    { st_perm     :: !Permission
    , st_atime_   :: !UTCTime
    , st_mtime_   :: !UTCTime
    , st_size_    :: !Word64
    , st_name     :: !String
    , st_uid_     :: !String
    , st_gid_     :: !String
    , st_muid_    :: !String
    } deriving (Show, Eq)

data WStat = WStat
    { wst_perm_    :: !(Maybe Permission)
    , wst_atime_   :: !(Maybe UTCTime)
    , wst_mtime_   :: !(Maybe UTCTime)
    , wst_size_    :: !(Maybe Word64)
    , wst_name_    :: !(Maybe String)
    , wst_uid_     :: !(Maybe String)
    , wst_gid_     :: !(Maybe String)
    , wst_muid_    :: !(Maybe String)
    } deriving (Show, Eq)

$( deriveAccessors ''WStat )

emptyWStat :: WStat
emptyWStat = WStat Nothing Nothing Nothing Nothing
                   Nothing Nothing Nothing Nothing

(^?=) :: Accessor r (Maybe a) -> a -> r -> r
(^?=) t a = t ^= Just a


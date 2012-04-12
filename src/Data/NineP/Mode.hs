module Data.NineP.Mode
    ( Mode, mRead, mWrite, mReadWrite, mExec, mTruncate, mRClose
    ) where

import Data.Word(Word8)

type Mode = Word8

mRead      :: Mode
mRead      = 1

mWrite     :: Mode
mWrite     = 2

mReadWrite :: Mode
mReadWrite = 4

mExec      :: Mode
mExec      = 8

mTruncate  :: Mode
mTruncate  = 0x10

mRClose    :: Mode
mRClose    = 0x40


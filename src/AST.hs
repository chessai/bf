{-# language
    DerivingStrategies
#-}

module AST
  ( BF(..)
  , Buffer
  , Index
  , Program
  ) where

import Data.Primitive.ByteArray (MutableByteArray)
import Data.Word (Word16)
import GHC.Exts (RealWorld)

data BF
  = MovePtr !Int
    -- ^ increment or decrement the pointer
  | MoveVal !Int
    -- ^ increment or decrement the value at the current index
  | Dot
    -- ^ print out the value at the current index
  | Comma
    -- ^ read a byte from stdin and write it to the current index
  | Loop [BF]
    -- ^ loop over the instructions
  | Whitespace
    -- ^ An ignored character
  | Clear
    -- ^ An optimisation of [-] (setting the value at the current index to 0)
  | Copy [Int]
    -- ^ Optimisation for copying the contents of one cell some others.
    --   The 'Int' is the index of the target relative to the current index.
    --   One or more copies will always be followed by a 'Clear'.
  deriving stock (Eq, Show)

type Buffer = MutableByteArray RealWorld
type Index  = Word16
type Program = [BF]



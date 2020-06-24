{-# language
    DeriveDataTypeable
  , DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , DerivingStrategies
#-}

module AST
  ( BF(..)
  , Instruction(..)
  , Buffer
  , Pointer
  , Program
  ) where

import Data.Data (Data)
import Data.Primitive.PrimArray (MutablePrimArray)
import Data.Word (Word8, Word16)
import GHC.Exts (RealWorld)

data BF a
  = Instr a (BF a)
    -- ^ An instruction, followed by the remainder
    --   of the program.
  | Loop (BF a) (BF a)
    -- ^ A loop. Instructions in the loop, and after.
  | If (BF a) (BF a)
    -- ^ An if-then-else. Contains then- and else- branches.
  | Halt
    -- ^ Halt the program.
  deriving stock (Eq, Show)
  deriving stock (Data)
  deriving stock (Functor, Foldable, Traversable)

data Instruction
  = Jump !Int
    -- ^ (n)
    --
    --   Jump forward or backward by n.
    --
    --   /p += n/
  | Update !Int !Int
    -- ^ (offset, n)
    --
    --   Add n the value at the given offset
    --
    --   /m[p + offset] += n/
  | Set !Int !Int
    -- ^ (offset, n)
    --
    --   Set the value at the given offset by n
    --
    --   /m[p + offset] = n/
  | MulUpdate !Int !Int !Int
    -- ^ (source, dest, n)
    --
    --   Multiply n by the value at source (relative to the
    --   current pointer), then add it to the value at dest
    --   (relative to the current pointer).
    --
    --   /m[p + dest] += m[p + source] * n/
  | MulSet !Int !Int !Int
    -- ^ (source, dest, n)
    --
    --   Multiply n by the value at source (relative to the
    --   current pointer), then set the value at dest
    --   (relative to the current pointer) to it.
    --
    --   /m[p + dest] = m[p + source] * n/
  | Input !Int
    -- ^ (offset)
    --
    --   Receive a byte from stdin and store it at the offset
    --   (relative to the current pointer).
    --
    --   /*(p + offset) = getchar()/
  | Output !Int
    -- ^ (offset)
    --
    --   Print the byte at the offset (relative to the current
    --   pointer).
    --
    --   /putchar(*(p + offset))/
  deriving stock (Eq, Show)
  deriving stock (Data)

-- ^ Byte Buffer (Tape)
--
--   /Note/: Size must not exceed 2^16
type Buffer  = MutablePrimArray RealWorld Word8
-- ^ Pointer into the Byte Buffer
type Pointer = Word16
-- ^ The type of BrainFuck programs
type Program = BF Instruction

instance Semigroup (BF a) where
  Halt <> x = x
  x <> Halt = x
  Instr a r <> r' = Instr a (r <> r')
  Loop x r <> r' = Loop x (r <> r')
  If x r <> r' = If x (r <> r')

instance Monoid (BF a) where
  mempty = Halt

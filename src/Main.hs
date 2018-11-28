{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -O2 #-}

module Main (main) where

import Control.Applicative (pure)
import Control.Monad (mapM_, forM_)
import Data.Bool (otherwise)
import Data.Char (Char, chr)
import Data.Eq ((==))
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (intercalate)
import Data.Maybe (Maybe(..))
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, writePrimArray)
import Data.Semigroup ((<>))
import Data.Word (Word8, Word16)
import GHC.Err (error)
import GHC.IO (IO)
import GHC.Prim (RealWorld)
import GHC.Real (fromIntegral)
import GHC.Show (Show)
import GHC.Types (Int)
import System.IO (hGetChar, stdin, putChar, print)
import Text.Read (readMaybe)
import qualified GHC.Num

main :: IO ()
main = do
  !mem <- newArray
  !ptr <- newPtr
  execute mem ptr 

execute :: Array -> Index -> IO ()
execute mem ptr = forM_ bfComps (\f -> f mem ptr)
  where
    parsedProg = parse prog
    bfComps = bfToComp <$> parsedProg

newArray :: IO Array
newArray = do
  !arr <- newPrimArray 30000
  pure (Array arr)

newPtr :: IO Index
newPtr = Index <$> newIORef 0

newtype Array = Array (MutablePrimArray RealWorld Word8)
newtype Index = Index (IORef Word16)

asciiChar :: Word8 -> Char
asciiChar = chr . fromIntegral

w2i :: Word16 -> Int
w2i !w = fromIntegral w

incPtr :: Index -> IO ()
incPtr (Index index) = modifyIORef' index (\x -> x GHC.Num.+ 1)

decPtr :: Index -> IO ()
decPtr (Index index) = modifyIORef' index (\x -> x GHC.Num.- 1)

incVal :: Array -> Index -> IO ()
incVal (Array arr) (Index index) = do
  !ix <- w2i <$> readIORef index
  !val <- readPrimArray arr ix
  writePrimArray arr ix (val GHC.Num.+ 1) 

decVal :: Array -> Index -> IO ()
decVal (Array arr) (Index index) = do
  !ix <- w2i <$> readIORef index
  !val <- readPrimArray arr ix
  writePrimArray arr ix (val GHC.Num.- 1) 

dot :: Array -> Index -> IO ()
dot (Array arr) (Index index) = do
  !ix <- w2i <$> readIORef index
  !val <- readPrimArray arr ix
  print val 
--  putChar (asciiChar val)
--  putChar '\n'

comma :: Array -> Index -> IO ()
comma (Array arr) (Index index) = do
  byte <- hGetChar stdin
  val <- case readMaybe [byte] of { Just w -> pure w; Nothing -> pure 0 }
  !ix <- w2i <$> readIORef index
  writePrimArray arr ix val

brackets :: Array -> Index -> [Array -> Index -> IO ()] -> IO ()
brackets a@(Array arr) i@(Index index) = \case
  [] -> pure ()
  acts -> do
    mapM_ (\f -> f a i) acts
    !ix <- w2i <$> readIORef index
    val <- readPrimArray arr ix
    if val == 0
      then pure ()
      else brackets a i acts

bfToComp :: BF -> (Array -> Index -> IO ())
bfToComp = \case
  IncPtr       -> \_   ix -> incPtr ix
  DecPtr       -> \_   ix -> decPtr ix
  IncVal       -> \arr ix -> incVal arr ix
  DecVal       -> \arr ix -> decVal arr ix
  Dot          -> \arr ix -> dot arr ix
  Comma        -> \arr ix -> comma arr ix
  BracketLeft  -> \_ _    -> pure ()
  BracketRight -> \_ _    -> pure ()
  Whitespace   -> \_ _    -> pure ()
  Bracket bfs  -> \arr ix -> let comps = bfToComp <$> bfs in brackets arr ix comps

data BF
  = IncPtr
  | DecPtr
  | IncVal
  | DecVal
  | Dot
  | Comma
  | BracketLeft
  | BracketRight
  | Bracket [BF]
  | Whitespace
  deriving (Show)

-- >>> parse prog
-- [ IncVal, IncVal -- Cell c0 = 2
-- , IncPtr
-- , IncVal, IncVal, IncVal, IncVal, IncVal -- Cell c1 = 5
-- , Bracket -- start the loop at c1. this loop will add the contents of c1 to c0.
--    [ DecPtr -- move to c0
--    , IncVal -- add 1 to c0
--    , IncPtr -- move to c1
--    , DecVal -- subtract 1 from c1
--    ]
--   -- at this point our program has added 5 to 2, leaving 7 in c0 and 0 in c1.
-- , IncVal, IncVal, IncVal, IncVal, IncVal, IncVal, IncVal, IncVal -- add 8 to c1
-- , Bracket -- start the loop at c1. this loop will add 48 to c0 (it adds 6, 8 times)
--    [ DecPtr
--    , IncVal, IncVal, IncVal, IncVal, IncVal, IncVal
--    , IncPtr
--    , DecVal
--    ]
-- , DecPtr -- move to c0
-- , Dot -- print c0, ascii 55 -> '7'
-- ]

prog :: [Char]
prog = intercalate newline
  [ "++       Cell c0 = 2"
  , "> +++++  Cell c1 = 5"
  , ""
  , "[        Start your loops with your cell pointer on the loop counter (c1 in our case)"
  , "< +      Add 1 to c0"
  , "> -      Subtract 1 from c1"
  , "]        End your loops with the cell pointer on the loop counter"
  , ""
  , "At this point our program has added 5 to 2 leaving 7 in c0 and 0 in c1"
  , "but we cannot output this value to the terminal since it is not ASCII encoded!"
  , ""
  , "To display the ASCII character \"7\" we must add 48 to the value 7"
  , "48 = 6 * 8 so let's use another loop to help us!"
  , ""
  , "++++ ++++  c1 = 8 and this will be our loop counter again"
  , "["
  , "< +++ +++  Add 6 to c0"
  , "> -        Subtract 1 from c1"
  , "]"
  , "< .        Print out c0 which has the value 55 which translates to \"7\"!"
  ]

charToBF :: Char -> BF
charToBF x
  | x == '>'  = IncPtr
  | x == '<'  = DecPtr
  | x == '+'  = IncVal
  | x == '-'  = DecVal
  | x == '.'  = Dot
  | x == ','  = Comma
  | x == '['  = BracketLeft
  | x == ']'  = BracketRight
  | otherwise = Whitespace

-- | FIXME: Better error handling
goBracket :: [Char] -> ([BF], [Char])
goBracket [] = error "Parse error! Failed parse of a BF loop."
goBracket (x:xs) = case charToBF x of
  BracketRight -> ([], xs)
  BracketLeft  -> goBracket xs
  Whitespace   -> case goBracket xs of { (xs', ys') -> (xs', ys') }
  c            -> case goBracket xs of { (xs', ys') -> (c : xs', ys') }

-- | FIXME: Better error handling
parse :: [Char] -> [BF]
parse [] = []
parse (x:xs) = case charToBF x of
  BracketLeft -> case goBracket xs of { (bfs, chrs) -> [Bracket bfs] <> parse chrs }
  BracketRight -> error "Parse error! Failed parse of a BF loop."
  Whitespace -> parse xs
  c          -> c : parse xs

newline :: [Char]
newline = "\n"
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (modify, get)
import Control.Monad.State.Strict (StateT, runStateT, evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import Data.Char (chr)
import Data.Primitive.ByteArray
import Data.Primitive.ByteArray.Atomic
import Data.Void
import Data.Word (Word8, Word16)
import GHC.Exts (RealWorld)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char hiding (asciiChar)
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = do
  test "test.bf"
  test "hello.bf"

test :: FilePath -> IO ()
test progFile = do
  prog <- doParse progFile
  print $ optimise prog
  putStrLn $ pretty $ optimise prog
  interpret $ optimise prog

type Buffer = MutableByteArray RealWorld
type Index  = Word16

type Program = [BF]
type Interpret = StateT Index (ReaderT Buffer IO)

type Parser = ParsecT Void String Identity

doParse :: FilePath -> IO Program
doParse progFile = do
  progCode <- readFile progFile
  either fail pure (first errorBundlePretty (runParser parser progFile progCode))

parser :: Parser Program
parser = many $ choice
  [ MoveVal 1    <$  char '+'
  , MoveVal (-1) <$  char '-'
  , MovePtr 1    <$  char '>'
  , MovePtr (-1) <$  char '<'
  , Dot          <$  char '.'
  , Comma        <$  char ','
  , Loop         <$> between (char '[') (char ']') parser
  , Whitespace   <$  noneOf "+-<>.,[]"
  ]

pretty :: [BF] -> String
pretty = concatMap go
  where
    go = \case
      MoveVal n -> if n < 0
        then replicate (abs n) '-'
        else replicate n '+'
      MovePtr n -> if n < 0
        then replicate (abs n) '<'
        else replicate n '>'
      Dot -> "."
      Comma -> ","
      Loop loop -> "[" ++ concatMap go loop ++ "]"
      Whitespace -> " "

optimise :: Program -> Program
optimise = id
  . collapseMoves
  . stripWhitespace

collapseMoves :: Program -> Program
collapseMoves = \case
  [] -> []
  MovePtr m : MovePtr n : prog -> optimise (MovePtr (m + n) : prog)
  MoveVal m : MoveVal n : prog -> optimise (MoveVal (m + n) : prog)
  Loop bfs : prog -> Loop (optimise bfs) : optimise prog
  x : prog -> x : optimise prog

stripWhitespace :: Program -> Program
stripWhitespace = concatMap go
  where
    go :: BF -> [BF]
    go = \case
      Loop loop -> [Loop (filter (/= Whitespace) loop)]
      Whitespace -> []
      x -> [x]

data BF
  = MovePtr !Int
    -- ^ increment or decrement the pointer
  | MoveVal !Int
    -- ^ increment or decrement the value at the current index
  | Dot
  | Comma
  | Loop [BF]
  | Whitespace
  deriving stock (Eq, Show)

asciiChar :: Word8 -> Char
asciiChar = chr . fromIntegral

w2i :: Word16 -> Int
w2i !w = fromIntegral w

interpret :: Program -> IO ()
interpret program = do
  buffer <- newPinnedByteArray 30_000
  let ptr = 0
  flip runReaderT buffer $ flip evalStateT ptr $ interpreter program

logState :: Interpret ()
logState = do
  index <- get
  buffer <- ask
  atIndex <- readByteArray @Word8 buffer (fromIntegral index)
  liftIO $ putStrLn $ "c" ++ showSubscript index ++ " = " ++ show atIndex

logDebug :: String -> Interpret ()
logDebug = liftIO . putStrLn

showSubscript :: (Integral a, Show a) => a -> String
showSubscript = map toSubscript . show
  where
    toSubscript :: Char -> Char
    toSubscript = \case
      '0'   -> '₀'
      '1'   -> '₁'
      '2'   -> '₂'
      '3'   -> '₃'
      '4'   -> '₄'
      '5'   -> '₅'
      '6'   -> '₆'
      '7'   -> '₇'
      '8'   -> '₈'
      '9'   -> '₉'
      other -> other

interpreter :: Program -> Interpret ()
interpreter = mapM_ interpreter'
  where
    interpreter' = \case
      MovePtr i -> do
        modify (+ (fromIntegral i))
      MoveVal i -> do
        index <- get
        buffer <- ask
        old <- readByteArray @Word8 buffer (fromIntegral index)
        writeByteArray buffer (fromIntegral index) (old + (fromIntegral i))
      Loop bfs -> do
        buffer <- ask
        indexBeforeLoop <- get
        byteBeforeLoop <- readByteArray @Word8 buffer (fromIntegral indexBeforeLoop)
        when (byteBeforeLoop /= 0) $ do
          mapM_ interpreter' bfs
          indexAfterLoop <- get
          byteAfterLoop <- readByteArray @Word8 buffer (fromIntegral indexAfterLoop)
          when (byteAfterLoop /= 0) (interpreter' (Loop bfs))
      Dot -> do
        index <- get
        buffer <- ask
        val <- readByteArray @Word8 buffer (fromIntegral index)
        liftIO $ putStrLn [asciiChar val]
      Comma -> do
        byte <- liftIO $ readLn @Word8
        index <- get
        buffer <- ask
        writeByteArray buffer (fromIntegral index) byte
      Whitespace -> do
        pure ()

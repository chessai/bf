{-# language
    BangPatterns
  , LambdaCase
  , MagicHash
  , NumericUnderscores
  , TypeApplications
#-}

module Interpreter
  ( interpret
  , interpreter
  ) where

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (modify, get)
import Control.Monad.State.Strict (StateT, evalStateT)
import Data.Char (chr, ord)
import Data.Primitive.ByteArray (MutableByteArray(..), newPinnedByteArray)
import Data.Primitive.PrimArray
import Data.Word (Word8)
import System.IO (hFlush, stdout)
import System.Exit (exitFailure)

import AST

type Interpret = StateT Pointer (ReaderT Buffer IO)

interpret :: Program -> IO ()
interpret program = do
  MutableByteArray buffer# <- newPinnedByteArray 30_000
  let buffer = MutablePrimArray buffer#
  let ptr = 0
  runInterpret buffer ptr $ interpreter program

runInterpret :: ()
  => Buffer
  -> Pointer
  -> Interpret a
  -> IO a
runInterpret buffer ptr i =
  flip runReaderT buffer
  $ flip evalStateT ptr
  $ i

interpreter :: Program -> Interpret ()
interpreter = mapM_ interpreter'
  where
    interpreter' = \case
      MovePtr i -> do
        modify (+ (fromIntegral i))
      MoveVal i -> do
        index <- get
        buffer <- ask
        old <- readPrimArray buffer (fromIntegral index)
        writePrimArray buffer (fromIntegral index) (old + (fromIntegral i))
      Loop bfs -> do
        buffer <- ask
        indexBeforeLoop <- get
        byteBeforeLoop <- readPrimArray buffer (fromIntegral indexBeforeLoop)
        when (byteBeforeLoop /= 0) $ do
          mapM_ interpreter' bfs
          indexAfterLoop <- get
          byteAfterLoop <- readPrimArray buffer (fromIntegral indexAfterLoop)
          when (byteAfterLoop /= 0) (interpreter' (Loop bfs))
      Dot -> do
        index <- get
        buffer <- ask
        val <- readPrimArray buffer (fromIntegral index)
        liftIO $ do
          putChar (asciiChar val)
          hFlush stdout
      Comma -> do
        byte <- liftIO (charToByte <$> getChar)
        index <- get
        buffer <- ask
        writePrimArray buffer (fromIntegral index) byte
      Whitespace -> do
        pure ()
      Clear -> do
        index <- get
        buffer <- ask
        writePrimArray buffer (fromIntegral index) 0
      Copy is -> do
        index <- get
        buffer <- ask
        byte <- readPrimArray buffer (fromIntegral index)
        forM_ is $ \i -> do
          let ix = fromIntegral index + i
          old <- readPrimArray buffer ix
          writePrimArray buffer ix (old + byte)
        interpreter' Clear
      Halt -> do
        liftIO $ exitFailure

charToByte :: Char -> Word8
charToByte = fromIntegral . ord

asciiChar :: Word8 -> Char
asciiChar = chr . fromIntegral

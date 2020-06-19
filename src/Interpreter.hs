{-# language
    BangPatterns
  , LambdaCase
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
import Data.Char (chr)
import Data.Primitive.ByteArray
import Data.Word (Word8, Word16)
import GHC.Exts (RealWorld)

import AST

type Interpret = StateT Index (ReaderT Buffer IO)

interpret :: Program -> IO ()
interpret program = do
  buffer <- newPinnedByteArray 30_000
  let ptr = 0
  runInterpret buffer ptr $ interpreter program

runInterpret :: ()
  => MutableByteArray RealWorld
  -> Word16
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
      Clear -> do
        index <- get
        buffer <- ask
        writeByteArray @Word8 buffer (fromIntegral index) 0
      Copy is -> do
        index <- get
        buffer <- ask
        byte <- readByteArray @Word8 buffer (fromIntegral index)
        forM_ is $ \i -> do
          let ix = fromIntegral index + i
          old <- readByteArray @Word8 buffer ix
          writeByteArray @Word8 buffer ix (old + byte)
        interpreter' Clear

asciiChar :: Word8 -> Char
asciiChar = chr . fromIntegral

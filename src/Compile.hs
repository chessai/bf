{-# language
    AllowAmbiguousTypes
  , DataKinds
  , KindSignatures
  , GADTs
  , LambdaCase
  , ScopedTypeVariables
  , TypeApplications
#-}

module Compile
  ( driver

  , CompileTarget(..)
  , STarget(..)
  , Phase(..), SPhase(..)
  ) where

import System.Process.Typed
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Data.Kind (Type)
import Data.Maybe
import System.Directory
import System.FilePath
import System.Environment (lookupEnv)
import System.IO.Temp

import AST
import UI (Target(..), Options(..))
import Pretty

data STarget :: Target -> Type where
  STargetC99       :: STarget 'TargetC99
  --STargetASM       :: STarget 'TargetASM
  --STargetHaskell98 :: STarget 'TargetHaskell98

data Phase
  = Source
  | Compile
  | Assemble
  | Link

data SPhase :: Phase -> Type where
  SSource   :: FilePath -> SPhase 'Source
  SCompile  :: FilePath -> SPhase 'Compile
  SAssemble :: FilePath -> SPhase 'Assemble
  SLink     :: FilePath -> SPhase 'Link

type CompileT = ReaderT Options
--type Compile  = CompileT Identity

type CompileM = CompileT IO

class CompileTarget (target :: Target) where
  compileToTarget :: STarget target -> Program -> CompileM (SPhase 'Source)

  compile :: STarget target -> SPhase 'Source -> CompileM (SPhase 'Compile)

  assemble :: STarget target -> SPhase 'Compile -> CompileM (SPhase 'Assemble)

  link :: STarget target -> SPhase 'Assemble -> CompileM (SPhase 'Link)

{-
instance CompileTarget 'TargetHaskell98 where
  compileToTarget _ program = do
    bufSize <- asks bufferSize
    out <- asks output
    let source = out ++ ".hs"
    let hsSourceCode = header bufSize ++ compiled ++ footer
    liftIO $ writeFile source hsSourceCode
    pure $ SSource source
    where
      header bufSize = unlines
        [ "{-# LANGUAGE TypeApplications #-}"
        , ""
        , "module Main (main) where"
        , ""
        , "import Data.Primitive.PrimArray"
        , "import Control.Monad (when, forM_)"
        , "import Control.Monad.IO.Class (liftIO)"
        , "import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)"
        , "import Control.Monad.Trans.State.Strict (StateT, evalStateT, modify, get)"
        , "import Data.Char (chr, ord)"
        , "import Data.Word (Word8, Word16)"
        , "import GHC.Exts (RealWorld)"
        , "import System.IO (hFlush, stdout)"
        , ""
        , "main :: IO ()"
        , "main = do"
        , "  buf <- mallocArray @Word8 " ++ show bufSize
        , "  ptr <- newIORef @Word16 0"
        ]
      compiled = concatMap (indent 2 . go) program
      footer = ""

      go = \case
        MovePtr n -> if n < 0
          then "modifyIORef' ptr (subtract " ++ show (abs n) ++ ")"
          else "modifyIORef' ptr (+ " ++ show n ++ ")"
-}

instance CompileTarget 'TargetC99 where
  compileToTarget _ program = do
    bufSize <- asks bufferSize
    out <- asks output
    let source = out ++ ".c"
    let cSourceCode = header bufSize ++ compiled ++ footer
    liftIO $ writeFile source cSourceCode
    pure $ SSource source
    where
      header bufSize = unlines
        [ "#include <stdio.h>"
        , "#include <stdint.h>"
        , "#include <stdlib.h>"
        , ""
        , "int main() {"
        , "  uint8_t buffer[" ++ show bufSize ++ "]={0};"
        , "  uint8_t * p = buffer;"
        ]
      compiled = concatMap (indent 2 . go) program
      footer = "}\n"

      go = \case
        MovePtr 1    -> "p++;"
        MovePtr (-1) -> "p--;"
        MovePtr n    -> if n < 0
          then "p = p - " ++ show (abs n) ++ ";"
          else "p = p + " ++ show n ++ ";"

        MoveVal 1    -> "(*p)++;"
        MoveVal (-1) -> "(*p)--;"
        MoveVal n    -> if n < 0
          then "*p = *p - " ++ show (abs n) ++ ";"
          else "*p = *p + " ++ show n ++ ";"

        Loop bfs -> concat
          [ "while (*p) {\n"
          , concatMap (indent 2 . go) bfs
          , "}"
          ]

        Dot -> "putchar(*p);"

        Comma -> "getchar(*p);"

        Whitespace -> ""

        Clear -> "*p = 0;"

        Copy is -> concat
          [ concatMap (\i -> indent 2 "*p = *p + " ++ show i) is
          , indent 2 "*p = 0;"
          ]

        Halt -> "exit(1);"

  compile _ (SSource source) = do
    out <- asks output
    cc <- getExe "CC"
    liftIO $ whichExplain cc
    let csource = out ++ ".s"
    liftIO $ do
      exe cc ["-S", source, "-o", csource]
    pure (SCompile csource)

  assemble _ (SCompile csource) = do
    out <- asks output
    as <- getExe "AS"
    liftIO $ whichExplain as
    let osource = out ++ ".o"
    liftIO $ do
      exe as [csource, "-o", osource]
    pure (SAssemble osource)

  link _ (SAssemble osource) = do
    out <- asks output
    ld <- getExe "LD"
    liftIO $ whichExplain ld
    ldLibPath <- getExe "LD_LIBRARY_PATH"
    liftIO $ do
      putStrLn $ "LD_LIBRARY_PATH is: " ++ ldLibPath
      exe ld
        [ "-o", out
        , "-dynamic-linker"
        , ldLibPath </> "ld-linux-x86-64.so.2"
        , ldLibPath </> "crt1.o"
        , ldLibPath </> "crti.o"
        , "-lc", osource
        , ldLibPath </> "crtn.o"
        ]
    pure (SLink out)

compileDriver :: CompileTarget target
  => STarget target
  -> Program
  -> Options
  -> IO ()
compileDriver s program options = do
  outputFile <- (</> output options) <$> getCurrentDirectory
  withSystemTempDirectory "brainfreeze" $ \tempDir -> do
    putStrLn tempDir
    setCurrentDirectory tempDir
    let
        pipeline = (compileToTarget s)
          >=> (compile s)
          >=> (assemble s)
          >=> (link s)
    SLink out <- flip runReaderT options (pipeline program)
    copyFile out outputFile

getExe :: MonadIO m => String -> m FilePath
getExe exename = liftIO $ do
  fromMaybe (fail (exename ++ " not found")) <$> lookupEnv exename

whichExplain :: FilePath -> IO ()
whichExplain exeName = do
  putStrLn $ "Using " ++ exeName ++ " found on system at: "
  withProcessWait_ (proc "which" [exeName]) $ \_ -> do
    pure ()

exe :: FilePath -> [String] -> IO ()
exe exeName args = do
  withProcessWait_ (proc exeName args) $ \_ -> do
    pure ()

driver :: ()
  => Program -- ^ program
  -> Options
  -> IO ()
driver program options = case target options of
  TargetC99 -> do
    compileDriver STargetC99 program options
  TargetASM -> do
    --compileDriver STargetASM program options
    fail "ASM not supported yet"
  TargetHaskell98 -> do
    fail "Haskell 98 not supported yet"

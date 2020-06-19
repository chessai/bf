{-# language LambdaCase #-}

module UI
  ( ui
  , Options(..)
  , Target(..)
  ) where

import Data.Char (toLower)

import qualified Options.Applicative as O

ui :: IO Options
ui = O.execParser opts
  where
    opts = O.info (parseOptions O.<**> O.helper)
      (  O.fullDesc
      <> O.progDesc desc
      )
    desc = "bf - an optimising brainfuck compiler and interpreter."

data Target
  = TargetC99
  | TargetASM

data Options = Options
  { input :: FilePath
    -- ^ the input program
  , output :: FilePath
    -- ^ output executable name (optional -o, defaults to program name)
  , interpretOnly :: Bool
    -- ^ interpret only, don't compile
  , bufferSize :: Int
    -- ^ cell size of the tape buffer
  , target :: Target
    -- ^ compilation target
  }

programInput :: O.Parser FilePath
programInput = fileInput
  -- (InputFile <$> fileInput) <|> (InputStdin <$> stdinInput)
  where
    fileInput = O.strOption
      ( O.long "input"
      <> O.short 'f'
      <> O.metavar "FILEPATH"
      <> O.help "filepath to input program"
      )
{-
    stdinInput = O.strOption
      ( O.long "expr"
      <> O.short 'e'
      <> O.metavar "STRING"
      <> O.help "program on standard input"
      )
-}

programOutput :: O.Parser FilePath
programOutput = O.strOption
  ( O.long "output"
  <> O.short 'o'
  <> O.metavar "FILEPATH"
  <> O.value "brainfreeze.out"
  <> O.showDefault
  <> O.help "filepath for output executable"
  )

programInterpret :: O.Parser Bool
programInterpret = O.switch
  ( O.long "interpret"
  <> O.short 'i'
  <> O.help "interpret only, don't compile"
  )

programBufferSize :: O.Parser Int
programBufferSize = O.option O.auto
  ( O.long "cells"
  <> O.short 'c'
  <> O.metavar "INT"
  <> O.value 65536
  <> O.showDefault
  <> O.help "program buffer size"
  )

programTarget :: O.Parser Target
programTarget = O.option parseTarget
  ( O.long "target"
  <> O.short 't'
  <> O.value TargetC99
  <> O.showDefaultWith showTarget
  <> O.help "compilation target"
  )
  where
    parseTarget = O.eitherReader $ \s -> case map toLower s of
      "c99" -> Right TargetC99
      "c"   -> Right TargetC99

      "asm" -> Right TargetASM

      _     -> Left $ "Unknown Target: " ++ s

    showTarget = \case
      TargetC99 -> "C99"
      TargetASM -> "x86_64 Assembly"

parseOptions :: O.Parser Options
parseOptions = Options
  <$> programInput
  <*> programOutput
  <*> programInterpret
  <*> programBufferSize
  <*> programTarget

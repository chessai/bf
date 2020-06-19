module UI
  ( ui
  , Options(..)
  , Input(..)
  ) where

import qualified Options.Applicative as O

ui :: IO Options
ui = O.execParser opts
  where
    opts = O.info (parseOptions O.<**> O.helper)
      (  O.fullDesc
      <> O.progDesc desc
      )
    desc = "bf - an optimising brainfuck compiler and interpreter."

data Input
  = InputFile FilePath
  | InputStdin String

data Options = Options
  { input :: FilePath
    -- ^ the input program
  , output :: FilePath
    -- ^ output executable name (optional -o, defaults to program name)
  , interpretOnly :: Bool
    -- ^ interpret only, don't compile
  , bufferSize :: Int
    -- ^ cell size of the tape buffer
  , asm :: Bool
    -- ^ whether or not to skip $CC
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

programAsm :: O.Parser Bool
programAsm = O.switch
  ( O.long "asm"
  <> O.showDefault
  <> O.help "do not use $CC; produce assembly directly"
  )

parseOptions :: O.Parser Options
parseOptions = Options
  <$> programInput
  <*> programOutput
  <*> programInterpret
  <*> programBufferSize
  <*> programAsm

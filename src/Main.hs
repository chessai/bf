-------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------

module Main (main) where

-------------------------------------------------------------------

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
import Data.Maybe
import Data.Primitive.ByteArray
import Data.Primitive.ByteArray.Atomic
import Data.Void
import Data.Word (Word8, Word16)
import GHC.Exts (RealWorld)
import qualified Options.Applicative as O
import System.Environment (lookupEnv)
import System.IO.Temp (writeSystemTempFile)
import System.Posix.Process (executeFile)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char hiding (asciiChar)
import qualified Text.Megaparsec.Char.Lexer as L
import System.Exit
-------------------------------------------------------------------

main :: IO ()
main = do
  perform =<< O.execParser opts
  where
    opts = O.info (parseOptions O.<**> O.helper)
      (  O.fullDesc
      <> O.progDesc desc
      )
    desc = "bf - an optimising brainfuck compiler and interpreter."

{-
args :: IO (FilePath, [String])
args = getArgs >>= \case
  [] -> do
    putStrLn "bf: fatal: no input"
    exitFailure
  (program : argv) -> do
    pure (program, argv)
-}

perform :: Options -> IO ()
perform o@Options{..}
  | interpretOnly = do
      program <- doParse input
      interpret program
  | otherwise = do
      program <- doParse input
      compile o program

-------------------------------------------------------------------
-- Types
-------------------------------------------------------------------

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
  | ZeroOut
    -- ^ An optimisation of [-] (setting the value at the current index to 0)
  deriving stock (Eq, Show)

type Buffer = MutableByteArray RealWorld
type Index  = Word16
type Program = [BF]

--------------------------------------------------------------------
-- Parsing/Pretty-printing
------------------------------------------------------------------

type Parser = ParsecT Void String Identity

doParse :: Input -> IO Program
doParse = \case
  InputFile progFile -> do
    progCode <- readFile progFile
    either fail pure (first errorBundlePretty (runParser parser progFile progCode))
  InputStdin expr -> do
    either fail pure (first errorBundlePretty (runParser parser expr expr))

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
      ZeroOut -> "[-]"

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

--------------------------------------------------------------------
-- Optimiser
------------------------------------------------------------------

optimise :: Program -> Program
optimise = fixpoint optimise'
  where
    optimise' = id
      . zeroOuts
      . removeZeroMoves
      . collapseMoves
      . stripWhitespace

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x0 =
  let x = f x0
  in if x == x0
       then x
       else fixpoint f x

zeroOuts :: Program -> Program
zeroOuts = map go
  where
    isSubVal :: BF -> Bool
    isSubVal (MoveVal n) | n < 0 = True
    isSubVal _ = False

    go :: BF -> BF
    go = \case
      Loop loop | all isSubVal loop -> ZeroOut
      x -> x

removeZeroMoves :: Program -> Program
removeZeroMoves = \case
  [] -> []
  MovePtr 0 : prog -> removeZeroMoves prog
  MoveVal 0 : prog -> removeZeroMoves prog
  Loop bfs : prog -> Loop (removeZeroMoves bfs) : removeZeroMoves prog
  x : prog -> x : removeZeroMoves prog

collapseMoves :: Program -> Program
collapseMoves = \case
  [] -> []
  MovePtr m : MovePtr n : prog -> collapseMoves (MovePtr (m + n) : prog)
  MoveVal m : MoveVal n : prog -> collapseMoves (MoveVal (m + n) : prog)
  Loop bfs : prog -> Loop (collapseMoves bfs) : collapseMoves prog
  x : prog -> x : collapseMoves prog

stripWhitespace :: Program -> Program
stripWhitespace = concatMap go
  where
    go :: BF -> [BF]
    go = \case
      Loop loop -> [Loop (filter (/= Whitespace) loop)]
      Whitespace -> []
      x -> [x]

--------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------

type Interpret = StateT Index (ReaderT Buffer IO)

interpret :: Program -> IO ()
interpret program = do
  buffer <- newPinnedByteArray 30_000
  let ptr = 0
  flip runReaderT buffer $ flip evalStateT ptr $ interpreter program

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
      ZeroOut -> do
        index <- get
        buffer <- ask
        writeByteArray @Word8 buffer (fromIntegral index) 0

asciiChar :: Word8 -> Char
asciiChar = chr . fromIntegral

w2i :: Word16 -> Int
w2i !w = fromIntegral w

--------------------------------------------------------------------
-- Compiler
------------------------------------------------------------------

data Input
  = InputFile FilePath
  | InputStdin String

data OptimisationLevel = O0 | O1 | O2 | O3

data Options = Options
  { input :: Input
    -- ^ the input program
  , output :: FilePath
    -- ^ output executable name (optional -o, defaults to program name)
  , interpretOnly :: Bool
    -- ^ interpret only, don't compile
  , optimisationLevel :: OptimisationLevel
  }

programInput :: O.Parser Input
programInput = (InputFile <$> fileInput) <|> (InputStdin <$> stdinInput)
  where
    fileInput = O.strOption
      ( O.long "input"
      <> O.short 'f'
      <> O.metavar "FILEPATH"
      <> O.help "filepath to input program"
      )
    stdinInput = O.strOption
      ( O.long "expr"
      <> O.short 'e'
      <> O.metavar "STRING"
      <> O.help "program on standard input"
      )

programOutput :: O.Parser FilePath
programOutput = O.strOption
  ( O.long "output"
  <> O.short 'o'
  <> O.metavar "FILEPATH"
  <> O.value "brainfreeze.out"
  <> O.showDefault
  <> O.help "filepath for output executable"
  )

programOptimisationLevel :: O.Parser OptimisationLevel
programOptimisationLevel = (O0 <$ o0) <|> (O1 <$ o1) <|> (O2 <$ o2) <|> (O3 <$ o3) <|> oDefault
  where
    o0 = O.switch (mconcat [O.long "O0", O.help "compile with no optimisations"])
    o1 = O.switch (mconcat [O.long "O1", O.help "compile with some optimisations"])
    o2 = O.switch (mconcat [O.long "O2", O.help "compile with aggressive optimisations"])
    o3 = O.switch (mconcat [O.long "O3", O.help "turn the O2 into the O3"])
    oDefault = pure O2

parseOptions :: O.Parser Options
parseOptions = Options
  <$> programInput
  <*> programOutput
  <*> O.switch
        ( O.long "interpret"
        <> O.short 'i'
        <> O.help "interpret only, don't compile"
        )
  <*> programOptimisationLevel

compileToC :: Program -> String
compileToC program = header ++ compiled ++ footer
  where
    header = unlines
      [ "#include <stdio.h>"
      , "#include <stdint.h>"
      , "int main() {"
      , "  uint8_t buffer[30000]={};"
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

      ZeroOut -> "*p = 0;"

ccOptLevel :: OptimisationLevel -> String
ccOptLevel = \case
  O0 -> "-O0"
  O1 -> "-O1"
  O2 -> "-O2"
  O3 -> "-O3"

compile :: Options -> Program -> IO ()
compile Options{..} program = do
  cc <- fromMaybe (fail "CC not found") <$> lookupEnv "CC"
  let programFile = output ++ ".c"
  let opt = ccOptLevel optimisationLevel
  programFile <- writeSystemTempFile programFile (compileToC program)
  executeFile cc True [programFile, "-o", output, opt] Nothing

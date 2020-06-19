{-# language LambdaCase #-}

module Compile
  ( compile
  , Target(..)
  ) where

import Data.Maybe
import System.Environment (lookupEnv)
import System.IO.Temp (writeSystemTempFile)
import System.Posix.Process (executeFile)

import AST
import Pretty

data Target
  = TargetC99
  | TargetASM

compileToC :: ()
  => Program
  -> Int
  -> String
compileToC program bufSize = header ++ compiled ++ footer
  where
    header = unlines
      [ "#include <stdio.h>"
      , "#include <stdint.h>"
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

compile :: ()
  => Program -- ^ program
  -> FilePath -- ^ output (via -o)
  -> Int -- ^ buffer size
  -> Target -- ^ compilation target
  -> IO ()
compile program out bufSize = \case
  TargetC99 -> do
    cc <- fromMaybe (fail "CC not found") <$> lookupEnv "CC"
    let opt = "-O2"
    let compiled = compileToC program bufSize
    ccInput <- writeSystemTempFile (out ++ ".c") compiled
    executeFile cc True [ccInput, opt, "-o", out] Nothing
  TargetASM -> do
    fail "ASM not supported yet"

module Parser
  ( doParse
  , doParsePure
  ) where

import Data.Bifunctor (first)
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char hiding (asciiChar)

import AST

type Parser = ParsecT Void String Identity

doParse :: FilePath -> IO Program
doParse progFile = do
  progCode <- readFile progFile
  either fail pure (doParsePure progFile progCode)

doParsePure :: FilePath -> String -> (Either String Program)
doParsePure progFile progFileContents =
  first errorBundlePretty
  $ runParser parser progFile progFileContents

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

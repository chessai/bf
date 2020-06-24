module Parser
  ( doParse
  , doParsePure
  ) where

import Data.Bifunctor (first)
import Data.Foldable
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
  $ runParser parser progFile
  $ filter (`elem` "+-<>.,[]")
  $ progFileContents

parser :: Parser Program
parser = bricks <|> pure Halt
  where
    bricks = fold <$> some brick
    brick = (flip Instr Halt <$> instruction) <|> loop

    loop = flip Loop Halt
      <$> between (char '[') (char ']') bricks

    instruction = choice
      [ Jump 1    <$ char '>'
      , Jump (-1) <$ char '<'

      , Update 0 1    <$ char '+'
      , Update 0 (-1) <$ char '-'

      , Input  0 <$ char ','
      , Output 0 <$ char '.'
      ]

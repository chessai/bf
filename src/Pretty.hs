{-# language LambdaCase #-}

module Pretty
  ( pretty
  , indent
  ) where

import AST

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
      Clear -> "[-]"
      Copy is -> "Copy(" ++ show is ++ ")"
      -- "[-" ++ evalState (copy is) 0 ++ "]"
--    copy :: [Int] -> State Int String
--    copy = fmap concat . traverse copy'
--      where
--        copy' i = do
      Halt -> "[+]"

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines



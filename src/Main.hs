-------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------

module Main (main) where

-------------------------------------------------------------------

import AST
--import Compile
import Optimiser
--import Interpreter
import Parser
--import UI

-------------------------------------------------------------------

main :: IO ()
main = do
  let input = "test-programs/hello.bf"
  program <- optimise <$> doParse input
  print program
  print $ anyIf program

anyIf :: BF a -> Bool
anyIf = \case
  Halt -> False
  Loop a b -> anyIf a || anyIf b
  If _ _ -> True
  Instr _ a -> anyIf a


--  o@Options{..} <- ui
--  program <- optimise <$> doParse input
--  if | interpretOnly -> do
--         interpret program
--     | otherwise -> do
--         driver program o

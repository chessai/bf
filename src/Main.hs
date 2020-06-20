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

import Compile
import Optimiser
import Interpreter
import Parser
import UI

-------------------------------------------------------------------

main :: IO ()
main = do
  o@Options{..} <- ui
  program <- optimise <$> doParse input
  if | interpretOnly -> do
         interpret program
     | otherwise -> do
         driver program o

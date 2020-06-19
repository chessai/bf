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
  Options{..} <- ui
  program <- optimise <$> doParse input
  if | interpretOnly -> do
         interpret program
     | otherwise -> do
         compile program output bufferSize TargetC99

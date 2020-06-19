{-# language LambdaCase #-}

module Optimiser
  ( optimise
  ) where

import AST

optimise :: Program -> Program
optimise = fixpoint optimise'
  where
    optimise' = id
      . copyLoops
      . clearLoops
      . contraction
      . stripWhitespace

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x0 =
  let x = f x0
  in if x == x0
       then x
       else fixpoint f x

stripWhitespace :: Program -> Program
stripWhitespace = concatMap go
  where
    go :: BF -> [BF]
    go = \case
      Loop loop -> [Loop (filter (/= Whitespace) loop)]
      Whitespace -> []
      x -> [x]

-- | Contract consecutive moves to the same index, and afterward,
--   remove 0-valued moves
contraction :: Program -> Program
contraction = removeZeroMoves . contraction'
  where
    -- collapse consecutive writes to the same index
    contraction' = \case
      [] -> []
      MovePtr m : MovePtr n : prog -> contraction (MovePtr (m + n) : prog)
      MoveVal m : MoveVal n : prog -> contraction (MoveVal (m + n) : prog)
      Loop bfs : prog -> Loop (contraction bfs) : contraction prog
      x : prog -> x : contraction prog

    -- after contraction we may have moves that are 0-valued. remove them
    removeZeroMoves = \case
      [] -> []
      MovePtr 0 : prog -> removeZeroMoves prog
      MoveVal 0 : prog -> removeZeroMoves prog
      Loop bfs : prog -> Loop (removeZeroMoves bfs) : removeZeroMoves prog
      x : prog -> x : removeZeroMoves prog

-- | Turn things like [-], [--], [---], etc. into a single write
clearLoops :: Program -> Program
clearLoops = map go
  where
    isSubVal :: BF -> Bool
    isSubVal (MoveVal n) | n < 0 = True
    isSubVal _ = False

    go :: BF -> BF
    go = \case
      Loop loop | all isSubVal loop -> Clear
      x -> x

-- | Turn a loop which serves to copy the contents of a cell to one or more
--   others into one write per target.
--
--   Example input:
--     [->+>+<<]
--   Example output:
--     *(p + 1) += *p;
--     *(p + 2) += *p;
--     *p = 0;
--
--  A copy loop can be identified by the following criteria:
--
--    1. It does not contain another loop
--    2. Starts with @'MoveVal' (-1)@
--    3. Returns to the start index at the end
--    4. Does not modify the start cell except for the initial @'MoveVal' (-1)@
copyLoops :: Program -> Program
copyLoops = id
{-
map go
  where
    containsLoop :: [BF] -> Bool
    containsLoop = any (\case { Loop _ -> True; _ -> False; })

    startsWithNeg1 :: [BF] -> Bool
    startsWithNeg1 = \case
      [] -> False
      (MoveVal (-1) : _) -> True
      _ -> False

    returnsToStartIndex :: [BF] -> Bool
    returnsToStartIndex = (== 0) . getSum . foldMap gatherMovePtrs
      where
        gatherMovePtrs = \case
          MovePtr n -> Sum n
          _ -> 0

    isCopyLoop :: [BF] -> Bool
    isCopyLoop loop = not (containsLoop loop)
      && startsWithNeg1 loop
      && returnsToStartIndex loop

    go :: BF -> BF
    go = \case
      Loop loop -> undefined
      x -> x
-}

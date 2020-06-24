{-# language
    LambdaCase
  , ScopedTypeVariables
  , ViewPatterns
#-}

module Optimiser
  ( optimise
  ) where

import Control.Monad ((>=>))
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import AST

type Optimisation = Program -> Program

optimise :: Optimisation
optimise = fixEq op
  where
    op = id
      . eliminateClearLoops
      . contract
      . loopsToMul
      -- . loopsToIfs

-- m[p + x] = m[p + y] * 1
-- m[p + y] = m[p + x] * 1
dedupMulSet :: Optimisation
dedupMulSet = go
  where
    go = \case
      x@(Instr i@(MulSet s d 1) (Instr (MulSet s' d' 1) r))
        | d == s' && s == d' -> Instr i r
        | otherwise -> x
      Loop a b -> Loop (go a) (go b)
      If a b -> Loop (go a) (go b)
      x -> x

loopsToMul :: Optimisation
loopsToMul = go
  where
    go = \case
      Loop (updateOrJump >=> simulate -> Just mul) r -> mul <> single (Set 0 0) <> r
      If a b -> If (go a) (go b)
      x -> x

    updateOrJump :: Program -> Maybe Program
    updateOrJump = \case
      Halt -> Just Halt
      Instr i@(Update _ _) r -> Just (Instr i r)
      Instr i@(Jump _) r -> Just (Instr i r)
      _ -> Nothing

    simulate :: Program -> Maybe Program
    simulate = analyse . foldl' computeDelta (Map.empty, 0)

    computeDelta :: (Map Int Int, Int) -> Instruction -> (Map Int Int, Int)
    computeDelta (deltas, offset) = \case
      -- Updates change the value at the current offset + their offset
      Update o n ->
        let deltas' = Map.insertWith (+) (offset + o) n deltas
        in (deltas', offset)
      -- Jumps move the offset
      Jump n -> (deltas, offset + n)
      -- This shouldn't happen because of updateOrJump
      _ -> error "Optimiser: loopsToMul: computeDelta: invariant violated"

    analyse :: (Map Int Int, Int) -> Maybe Program
    analyse (deltas, offset)
      | offset /= 0 || Map.findWithDefault 0 offset deltas /= (-1) = Nothing
      | otherwise = Just $ Map.foldrWithKey buildMul Halt (Map.delete 0 deltas)
      where
        buildMul off val = Instr (MulUpdate 0 off val)


eliminateClearLoops :: Optimisation
eliminateClearLoops = go
  where
    go = \case
      -- A loop where we update an offset
      -- repeatedly. If n is negative, this is
      -- a clear loop. If n is 0 or positive, the
      -- program should halt.
      Loop (Instr (Update o n) Halt) r
        | n < 0 -> Instr (Set o 0) r
        | otherwise -> Halt
      -- A clear loop that is evidently non-recursive
      If (Instr (Set 0 0) Halt) r -> Instr (Set 0 0) r
      -- Recurse into all other instructions
      Instr i r -> Instr i (go r)
      -- Everything else is gucci
      x -> x

loopsToIfs :: Optimisation
loopsToIfs = go
  where
    go = \case
      Loop (canTransform >=> simulate -> Just p) r -> If (p <> single (Set 0 0)) r
      x -> x

    canTransform :: Program -> Maybe Program
    canTransform = \case
      Halt -> Just Halt
      Instr i@(ifElem -> True) r -> Just (Instr i r)
      _ -> Nothing
      where
        ifElem = \case
          Update _ _ -> True
          Set n _ -> n /= 0
          MulSet _ n _ -> n /= 0
          MulUpdate _ n _ -> n /= 0
          _ -> False

    simulate :: Program -> Maybe Program
    simulate = analyse . foldl' computeClears (Set.singleton 0, Halt, 0)

    computeClears :: (Set Int, Program, Int) -> Instruction -> (Set Int, Program, Int)
    computeClears (clears, prog, origin) = \case
      Update o n
        | o == 0 -> (clears, prog, origin + n)
        | otherwise -> (,,)
            (Set.delete o clears)
            (prog <> single (MulUpdate 0 o n))
            origin
      i@(MulUpdate _ d _) -> (,,)
        (Set.delete d clears)
        (prog <> single i)
        origin
      i@(Set o n) ->
        let clears' | n == 0 = Set.insert o clears
                    | otherwise = Set.delete o clears
            prog' = prog <> single i
        in (clears', prog', origin)
      _ -> error "Optimiser: loopsToIfs: computeClears: unexpected input"

    analyse :: (Set Int, Program, Int) -> Maybe Program
    analyse (clears, prog, n) = if n == (-1)
      then check prog
      else Nothing
      where
        check = \case
          Halt -> Just Halt
          Instr i@(cleared -> True) r -> Just (Instr i r)
          _ -> Nothing

        cleared = \case
          MulSet s _ _ -> s `Set.member` clears
          MulUpdate s _ _ -> s `Set.member` clears
          _ -> True

contract :: Optimisation
contract = go
  where
    go = \case
      -- Cannot optimise Halt, Input, or Output
      Halt
        -> Halt
      Instr (Input o) rest
        -> Instr (Input o) (go rest)
      Instr (Output o) rest
        -> Instr (Output o) (go rest)

      -- Loops and Ifs just recurse
      Loop as bs
        -> Loop (go as) (go bs)
      If as bs
        -> If (go as) (go bs)

      -- Combine double-jump
      Instr (Jump m) (Instr (Jump n) r)
        -> go (Instr (Jump (m + n)) r)
      Instr (Jump m) rest
        -> Instr (Jump m) (go rest)

      -- Combine updates to the same offset
      Instr (Update o m) rest@(Instr (Update o' n) r)
        -> if o == o'
           then go (Instr (Update o (m + n)) r)
           else Instr (Update o m) (go rest)
      Instr (Update o m) rest
        -> Instr (Update o m) (go rest)

      -- Remove redundant writes to the same address
      Instr (Set o m) rest@(Instr (Set o' n) r)
        -> if o == o'
           then go (Instr (Set o n) r)
           else Instr (Set o m) (go rest)
      Instr (Set o m) rest
        -> Instr (Set o m) (go rest)

      x -> x

{-
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


contract :: Optimisation
contract = mapBlocks instrEq go
  where
    go a@(Instr x _) =
      let numInstrs = instrSum a
      in case x of
           Update o _ ->
             case instrSum a of
               0 -> Halt
               n -> single (Update o n)
           Set o n ->
             single
             $ Set o
             $ maybe n instrVal
             $ lastInstruction a
           Jump _ ->
             single
             $ Jump numInstrs
           MulUpdate s d _ ->
             single
             $ MulUpdate s d numInstrs
           MulSet s d n ->
             single
             $ MulSet s d
             $ maybe n instrVal
             $ lastInstruction a
           _ -> a
    go _ = error "Optimiser: contract: unexpected input"

groupBy :: (a -> a -> Bool) -> BF a -> BF (BF a)
groupBy p = \case
  Halt -> Halt
  Loop as bs -> Loop (groupBy p as) (groupBy p bs)
  If as bs -> If (groupBy p as) (groupBy p bs)
  Instr x0 xs0 ->
    let (ys, zs) = go p x0 xs0
    in Instr (Instr x0 ys) zs
    where
      go q z = \case
        Instr x xs ->
          let (ys, zs) = go q x xs
          in if q z x
             then (Instr x ys, zs)
             else (Halt, Instr (Instr x ys) zs)
        Halt -> (Halt, Halt)
        Loop as bs -> (Halt, Loop (groupBy p as) (groupBy p bs))
        If as bs -> (Halt, If (groupBy p as) (groupBy p bs))

mapBlocks :: (a -> a -> Bool) -> (BF a -> BF a) -> BF a -> BF a
mapBlocks pr f = foldMap go . groupBy pr
  where
    go = \case
      Halt -> Halt
      Loop as bs -> Loop as bs
      If as bs -> If as bs
      Instr p r -> f (single p) <> r

instrEq :: Instruction -> Instruction -> Bool
instrEq a b = toConstr a == toConstr b && offsets a == offsets b
  where
    offsets :: Instruction -> Maybe (Either Int (Int, Int))
    offsets = \case
      Jump _ -> Nothing
      Update o _ -> Just (Left o)
      Set o _ -> Just (Left o)
      MulUpdate s d _ -> Just (Right (s, d))
      MulSet s d _ -> Just (Right (s, d))
      Input o -> Just (Left o)
      Output o -> Just (Left o)

instrSum :: Program -> Int
instrSum = getSum . foldMap (Sum . instrVal)

instrVal :: Instruction -> Int
instrVal = \case
  Jump n -> n
  Update _ n -> n
  Set _ n -> n
  MulUpdate _ _ n -> n
  MulSet _ _ n -> n
  _ -> 0

lastInstruction :: BF a -> Maybe a
lastInstruction = getLast . foldMap (Last . Just)
-}

single :: a -> BF a
single x = Instr x Halt

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f x = let x' = f x in if x == x' then x else fixEq f x'

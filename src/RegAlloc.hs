module RegAlloc
  (
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype InterferenceGraph name = InterferenceGraph (Map name (Set name))
empty :: InterferenceGraph name
empty = InterferenceGraph Map.empty

module Test.Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe)
import Data.SSA.CFG (CFG, BID(..))
import Data.SSA.CFG as CFG
import Data.Tuple.Nested ((/\))
import HLOCG.Program (Inst(..), Type(..))
import HLOCG.Target.ECMAScript (translateCFG)
import Prelude

main :: ∀ eff. Eff (console :: CONSOLE | eff) Unit
main = traverse_ (log <<< translateCFG) example

example :: Maybe (CFG Inst)
example = do
  cfg <- pure CFG.empty

  x        <- pure $ EntryBID
  y /\ cfg <- pure $ CFG.addB cfg
  z /\ cfg <- pure $ CFG.addB cfg

  a /\ cfg <- CFG.addI x (ConstI32 1)      cfg
  b /\ cfg <- CFG.addI x (ConstI32 2)      cfg
  c /\ cfg <- CFG.addI x (AddI I32 a b)    cfg
  d /\ cfg <- CFG.addI x (ConstBool false) cfg
  _ /\ cfg <- CFG.addI x (If d y z)        cfg

  _ /\ cfg <- CFG.addI y (Goto z)          cfg

  _ /\ cfg <- CFG.addI z (Ret I32 c)       cfg

  pure cfg

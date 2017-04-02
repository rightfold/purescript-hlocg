module Test.Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe)
import Data.SSA.CFG (CFG)
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
  a /\ cfg <- CFG.addI CFG.EntryBID (ConstI32 1)   cfg
  b /\ cfg <- CFG.addI CFG.EntryBID (ConstI32 2)   cfg
  c /\ cfg <- CFG.addI CFG.EntryBID (AddI I32 a b) cfg
  _ /\ cfg <- CFG.addI CFG.EntryBID (Ret I32 c)    cfg
  pure cfg

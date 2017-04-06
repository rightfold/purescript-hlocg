module Test.Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (traverse_)
import Data.List (List(Nil), (:))
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.SSA.CFG (CFG, BID(..))
import Data.SSA.CFG as CFG
import Data.Tuple.Nested ((/\))
import HLOCG.Module (Global(..), Module(..))
import HLOCG.Program (Inst(..), OnOverflow(..), Type(..))
import HLOCG.Target.ECMAScript (translateModule)
import Prelude

main :: ∀ eff. Eff (console :: CONSOLE | eff) Unit
main = traverse_ (log <<< translateModule) example

example :: Maybe Module
example = do
  cfg <- pure CFG.empty

  w        <- pure $ EntryBID
  x /\ cfg <- pure $ CFG.addB cfg
  y /\ cfg <- pure $ CFG.addB cfg
  z /\ cfg <- pure $ CFG.addB cfg

  a /\ cfg <- CFG.addI w (ConstI32 1)      cfg
  b /\ cfg <- CFG.addI w (ConstI32 2)      cfg
  let o = OnOverflowGoto z
  c /\ cfg <- CFG.addI w (AddI o I32 a b)  cfg
  d /\ cfg <- CFG.addI w (ConstBool false) cfg
  _ /\ cfg <- CFG.addI w (If d x y)        cfg

  _ /\ cfg <- CFG.addI x (Goto z)          cfg

  _ /\ cfg <- CFG.addI y (Ret I32 c)       cfg

  e /\ cfg <- CFG.addI z (Lambda I32 ((I32 /\ a) : (I32 /\ b) : Nil) CFG.empty) cfg
  f /\ cfg <- CFG.addI z (Call I32 e (c : d : Nil)) cfg
  _ /\ cfg <- CFG.addI z (Ret I32 f)       cfg

  pure <<< Module <<< Map.singleton "f" <<< Program Any $ cfg

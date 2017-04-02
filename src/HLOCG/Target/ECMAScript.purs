module HLOCG.Target.ECMAScript
  ( translateCFG
  , translateInst
  ) where

import Data.Foldable (foldMap)
import Data.SSA.CFG (CFG, IID(..), allBs, allIs)
import Data.Tuple (uncurry)
import HLOCG.Program (Inst(..))
import Prelude

translateCFG :: CFG Inst -> String
translateCFG = foldMap (uncurry translateInst) <<< (foldMap <<< flip allIs <*> allBs)

translateInst :: IID -> Inst -> String
translateInst = go
  where
  go iid (ConstI32 x) = var iid $ show x
  go iid (ConstF64 x) = var iid $ show x
  go iid (AddI _ a b) = var iid $ val a <> " + " <> val b <> " | 0"
  go iid (AddF _ a b) = var iid $ val a <> " + " <> val b
  go _   (Ret _ x)    = "return " <> val x <> ";\n"

  var iid e = "const " <> val iid <> " = " <> e <> ";\n"

  val (IID iid) = "v" <> show iid

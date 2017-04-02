module HLOCG.Target.ECMAScript
  ( translateCFG
  , translateInst
  ) where

import Data.Foldable (foldMap)
import Data.List (List)
import Data.SSA.CFG (BID(..), CFG, IID(..), allBs, allIs)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import HLOCG.Program (Inst(..))
import Prelude

translateCFG :: CFG Inst -> String
translateCFG cfg =
  "for (let b;;) {\nswitch (b) {\n" <>
  foldMap (translateB <*> allIs `flip` cfg) (allBs cfg) <>
  "}\n}\n"

translateB :: BID -> List (IID /\ Inst) -> String
translateB bid is =
  "case " <> translateBID bid <> ":\n" <>
  foldMap (uncurry translateInst) is

translateInst :: IID -> Inst -> String
translateInst = go
  where
  go iid (ConstI32 x) = var iid $ show x
  go iid (ConstF64 x) = var iid $ show x
  go iid (AddI _ a b) = var iid $ val a <> " + " <> val b <> " | 0"
  go iid (AddF _ a b) = var iid $ val a <> " + " <> val b
  go iid (Goto x)     = "b = " <> blk x <> ";\ncontinue;\n"
  go _   (Ret _ x)    = "return " <> val x <> ";\n"

  var iid e = "var " <> val iid <> " = " <> e <> ";\n"

  blk = translateBID

  val (IID iid) = "v" <> show iid

translateBID :: BID -> String
translateBID EntryBID  = "void 0"
translateBID (BID bid) = show bid

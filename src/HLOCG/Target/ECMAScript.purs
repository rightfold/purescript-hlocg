module HLOCG.Target.ECMAScript
  ( translateCFG
  , translateInst
  ) where

import Data.Foldable (foldMap)
import Data.List (List)
import Data.SSA.CFG (BID(..), CFG, IID(..), allBs, allIs)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import HLOCG.Program (Inst(..), OnOverflow(..))
import Prelude

translateCFG :: CFG Inst -> String
translateCFG cfg =
  "for (var b;;) switch (b) {\n" <>
  foldMap (translateB <*> allIs `flip` cfg) (allBs cfg) <>
  "}\n"

translateB :: BID -> List (IID /\ Inst) -> String
translateB bid is =
  "case " <> translateBID bid <> ":\n" <>
  foldMap (uncurry translateInst) is

translateInst :: IID -> Inst -> String
translateInst = go
  where
  go iid (ConstBool x)  = var iid $ show x
  go iid (ConstI32 x)   = var iid $ show x
  go iid (ConstF64 x)   = var iid $ show x
  go iid (AddI o _ a b) = case o of
    OnOverflowWrap      -> var iid $ val a <> " + " <> val b <> " | 0"
    OnOverflowJump x    ->
      var iid (val a <> " + " <> val b) <>
      "if (" <> val iid <> " < -2147483648 || " <> val iid <> " > 2147483647) {\n" <>
      "b = " <> blk x <> ";\ncontinue;\n" <>
      "}\n"
    OnOverflowUndefined -> var iid $ val a <> " + " <> val b <> " | 0"
  go iid (AddF _ a b)   = var iid $ val a <> " + " <> val b
  go iid (If x a b)     = "b = " <> val x <> " ? " <> blk a <>
                                            " : " <> blk b <>
                         ";\ncontinue;\n"
  go iid (Goto x)       = "b = " <> blk x <> ";\ncontinue;\n"
  go _   (Ret _ x)      = "return " <> val x <> ";\n"

  var iid e = "var " <> val iid <> " = " <> e <> ";\n"

  blk = translateBID

  val (IID iid) = "v" <> show iid

translateBID :: BID -> String
translateBID EntryBID  = "void 0"
translateBID (BID bid) = show bid

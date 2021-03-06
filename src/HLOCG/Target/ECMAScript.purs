module HLOCG.Target.ECMAScript
  ( translateModule
  , translateCFG
  , translateInst
  ) where

import Data.Foldable (foldMap, intercalate)
import Data.List (List)
import Data.Map as Map
import Data.SSA.CFG (BID(..), CFG, IID(..), allBs, allIs)
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\))
import HLOCG.Module (Global(..), Module(..))
import HLOCG.Program (Inst(..), OnOverflow(..))
import Prelude

translateModule :: Module -> String
translateModule (Module m) =
  foldMap (uncurry translateGlobal) $
    Map.toUnfoldable m :: List _

translateGlobal :: String -> Global -> String
translateGlobal n (Program _ p) =
  "exports." <> n <> " = function() {\n" <>
  translateCFG p <>
  "};\n"

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
  go iid (ConstBool x)   = var iid $ show x
  go iid (ConstI32 x)    = var iid $ show x
  go iid (ConstF64 x)    = var iid $ show x
  go iid (AddI o _ a b)  = case o of
    OnOverflowWrap      -> var iid $ val a <> " + " <> val b <> " | 0"
    OnOverflowGoto x    ->
      var iid (val a <> " + " <> val b) <>
      "if (" <> val iid <> " < -2147483648 || " <> val iid <> " > 2147483647) {\n" <>
      "b = " <> blk x <> ";\ncontinue;\n" <>
      "}\n"
    OnOverflowUndefined -> var iid $ val a <> " + " <> val b <> " | 0"
  go iid (AddF _ a b)    = var iid $ val a <> " + " <> val b
  go iid (Lambda _ as x) = var iid $
    "function() {\n" <>
    translateCFG x <>
    "}.bind(null" <> foldMap (append ", " <<< val <<< snd) as <> ")"
  go iid (Call _ a bs)   = var iid $ val a <> "(" <> intercalate ", " (map val bs) <> ")"
  go iid (If x a b)      = "b = " <> val x <> " ? " <> blk a <>
                                            " : " <> blk b <>
                         ";\ncontinue;\n"
  go iid (Goto x)        = "b = " <> blk x <> ";\ncontinue;\n"
  go _   (Ret _ x)       = "return " <> val x <> ";\n"

  var iid e = "var " <> val iid <> " = " <> e <> ";\n"

  blk = translateBID

  val (IID iid) = "v" <> show iid

translateBID :: BID -> String
translateBID EntryBID  = "void 0"
translateBID (BID bid) = show bid

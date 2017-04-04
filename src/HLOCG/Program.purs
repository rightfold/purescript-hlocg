module HLOCG.Program
  ( Type(..)
  , Inst(..)
  , OnOverflow(..)
  ) where

import Data.Set as Set
import Data.SSA.CFG (class I, BID, IID)

-- | Representation type.
data Type
  = Any
  | Bool
  | I32
  | F64

-- | Instruction.
data Inst
  = ConstBool Boolean
  | ConstI32 Int
  | ConstF64 Number
  | AddI OnOverflow Type IID IID
  | AddF Type IID IID
  | If IID BID BID
  | Goto BID
  | Ret Type IID

-- | Overflow policy.
data OnOverflow
  = OnOverflowWrap
  | OnOverflowGoto BID
  | OnOverflowUndefined

instance iInst :: I Inst where
  targets (ConstBool _)  = Set.empty
  targets (ConstI32 _)   = Set.empty
  targets (ConstF64 _)   = Set.empty
  targets (AddI o _ _ _) = case o of
    OnOverflowWrap      -> Set.empty
    OnOverflowGoto x    -> Set.singleton x
    OnOverflowUndefined -> Set.empty
  targets (AddF _ _ _)   = Set.empty
  targets (If _ a b)     = Set.fromFoldable [a, b]
  targets (Goto x)       = Set.singleton x
  targets (Ret _ _)      = Set.empty

  operands (ConstBool _)  = Set.empty
  operands (ConstI32 _)   = Set.empty
  operands (ConstF64 _)   = Set.empty
  operands (AddI _ _ a b) = Set.fromFoldable [a, b]
  operands (AddF _ a b)   = Set.fromFoldable [a, b]
  operands (If x _ _)     = Set.singleton x
  operands (Goto _)       = Set.empty
  operands (Ret _ x)      = Set.singleton x

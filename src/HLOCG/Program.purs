module HLOCG.Program
  ( Type(..)
  , Inst(..)
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
  | AddI Type IID IID
  | AddF Type IID IID
  | If IID BID BID
  | Goto BID
  | Ret Type IID

instance iInst :: I Inst where
  targets (ConstBool _) = Set.empty
  targets (ConstI32 _)  = Set.empty
  targets (ConstF64 _)  = Set.empty
  targets (AddI _ _ _)  = Set.empty
  targets (AddF _ _ _)  = Set.empty
  targets (If _ a b)    = Set.fromFoldable [a, b]
  targets (Goto x)      = Set.singleton x
  targets (Ret _ _)     = Set.empty

  operands (ConstBool _) = Set.empty
  operands (ConstI32 _)  = Set.empty
  operands (ConstF64 _)  = Set.empty
  operands (AddI _ a b)  = Set.fromFoldable [a, b]
  operands (AddF _ a b)  = Set.fromFoldable [a, b]
  operands (If x _ _)    = Set.singleton x
  operands (Goto _)      = Set.empty
  operands (Ret _ x)     = Set.singleton x

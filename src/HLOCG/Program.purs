module HLOCG.Program
  ( Type(..)
  , Inst(..)
  ) where

import Data.Set as Set
import Data.SSA.CFG (class I, IID)

-- | Representation type.
data Type
  = Any
  | I32
  | F64

-- | Instruction.
data Inst
  = ConstI32 Int
  | ConstF64 Number
  | AddI IID IID
  | AddF IID IID
  | Ret IID

instance iInst :: I Inst where
  targets (ConstI32 _) = Set.empty
  targets (ConstF64 _) = Set.empty
  targets (AddI _ _)   = Set.empty
  targets (AddF _ _)   = Set.empty
  targets (Ret _)      = Set.empty

  operands (ConstI32 _) = Set.empty
  operands (ConstF64 _) = Set.empty
  operands (AddI a b)   = Set.fromFoldable [a, b]
  operands (AddF a b)   = Set.fromFoldable [a, b]
  operands (Ret x)      = Set.singleton x

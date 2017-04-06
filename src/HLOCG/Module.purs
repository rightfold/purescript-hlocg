module HLOCG.Module
  ( Global(..)
  , Module(..)
  ) where

import Data.Map (Map)
import Data.SSA.CFG (CFG)
import HLOCG.Program (Inst, Type)

data Global
  = Program Type (CFG Inst)

newtype Module = Module (Map String Global)

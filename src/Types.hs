module Types where

import Clash.Prelude

-- Internal bit size
type NumBits = 32

-- Registers
type NumRegs    = 32
type Reg        = Signed NumBits
type UReg       = Unsigned NumBits
type RegIdxSize = 5
type RegIdx     = Unsigned RegIdxSize
type RegFile    = Vec NumRegs Reg

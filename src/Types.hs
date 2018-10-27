module Types where

import Clash.Prelude

--
-- Common types
--

-- Register
type Reg = Signed 32

-- Register file
type RegFile = Vec 32 Reg

-- Register index
type Ridx = Unsigned 5

-- Immediate value types
type Imm20 = Signed 20
type Imm12 = Signed 12
type UImm12 = Unsigned 12

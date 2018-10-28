module ALU where

import Clash.Prelude
import Types
import Instructions
import Registers

sToU :: KnownNat n => Signed n -> Unsigned n
sToU s = unpack $ pack s

alu regs instr = case instr of
    ADDI rs1 rd imm     -> replace rd ((regs !! rs1) + (resize imm) :: Reg) regs
    SLTI rs1 rd imm     -> replace rd (unpack ((boolToBV $ (regs !! rs1) <= (resize imm))) :: Reg) regs
    SLTIU rs1 rd imm    -> replace rd (unpack ((boolToBV $ (sToU (regs !! rs1)) <= (resize imm))) :: Reg) regs
    XORI rs1 rd imm     -> replace rd ((regs !! rs1) `xor` (resize imm) :: Reg) regs
    ORI rs1 rd imm      -> replace rd ((regs !! rs1) .|. (resize imm) :: Reg) regs
    ANDI rs1 rd imm     -> replace rd ((regs !! rs1) .&. (resize imm) :: Reg) regs
    _                   -> regs

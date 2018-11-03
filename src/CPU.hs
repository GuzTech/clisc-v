module CPU where

import Clash.Prelude

import Types
import Instructions
import ALU

cpu
  :: (RegFile, Reg)                   -- Internal state (register file, rd)
  -> BitVector NumBits                -- Instruction
--  -> BitVector NumBits                
  -> ((RegFile, Reg), (RegFile, Reg)) -- (New state, new rd)
cpu (regs, pc) i = ((regs', pc_next), (regs', pc_next)) where
  -- Decoding stage
  (instr, instr_type)       = decodeInstr i
  a@(rs1, rs2, imm, rd_idx) = decodeStage i instr_type regs
  rs1u = unpack $ pack rs1 :: UReg
  rs2u = unpack $ pack rs2 :: UReg

  -- ALU stage
  alu_result = alu (instr, a)

  -- Write-back stage
  regs' = case instr_type of
    S_TYPE -> regs
    B_TYPE -> regs
    U_TYPE -> case instr of
      LUI   -> replace rd_idx imm regs
      AUIPC -> replace rd_idx (pc + imm) regs
    _      -> case instr of
      JAL  -> replace rd_idx (pc + 4) regs
      JALR -> replace rd_idx (pc + 4) regs
      _    -> replace rd_idx alu_result regs

  -- Program counter
  pc_next = case instr of
    JAL   -> pc + imm
    JALR  -> unpack ((slice d31 d1 $ rs1 + imm) ++# 0b0)
    BEQ   -> case (rs1 == rs2) of
      True -> pc + imm
      _    -> pc + 4
    BNE   -> case (rs1 /= rs2) of
      True -> pc + imm
      _    -> pc + 4
    BLT   -> case (rs1 < rs2) of
      True -> pc + imm
      _    -> pc + 4
    BGE   -> case (rs1 >= rs2) of
      True -> pc + imm
      _    -> pc + 4
    BLTU  -> case (rs1u < rs2u) of
      True -> pc + imm
      _    -> pc + 4
    BGEU  -> case (rs1u >= rs2u) of
      True -> pc + imm
      _    -> pc + 4
    _     -> pc + 4

cpuM = mealy cpu ((repeat 0) :: RegFile, 0)
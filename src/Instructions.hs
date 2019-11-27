-- CLISC-V: A RISC-V (RV32I) written in CLaSH <https://clash-lang.org>
--
-- Copyright (C) 2018 Oguz Meteer <info@guztech.nl>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF 
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

module Instructions where

import Clash.Prelude
import GHC.Generics
import Control.DeepSeq

import Types

data Instr
  = LUI
  | AUIPC
  | JAL
  | JALR
  | BEQ
  | BNE
  | BLT
  | BGE
  | BLTU
  | BGEU
  | LB
  | LH
  | LW
  | LBU
  | LHU
  | SB
  | SH
  | SW
  | ADDI
  | SLTI
  | SLTIU
  | XORI
  | ORI
  | ANDI
  | SLLI
  | SRLI
  | SRAI
  | ADD
  | SUB
  | SLL
  | SLT
  | SLTU
  | XOR
  | SRL
  | SRA
  | OR
  | AND
  | FENCE
  | FENCE_I
  | ECALL
  | EBREAK
  deriving (Show, Eq, Generic, NFData, ShowX)

data InstrType
  = R_TYPE
  | I_TYPE
  | S_TYPE
  | B_TYPE
  | U_TYPE
  | J_TYPE
  | F_TYPE -- FENCE | FENCE.I
  deriving (Show, Eq, Generic, NFData, ShowX)

decodeInstr
  :: BitVector NumBits
  -> (Instr, InstrType)
decodeInstr i = o where
  instr  = slice d6 d0 i
  func3  = slice d14 d12 i
  func7  = slice d31 d25 i
  bit20  = i ! 20
  bit30  = i ! 30
  o     = case instr of
    0b0110111 -> (LUI,   U_TYPE)
    0b0010111 -> (AUIPC, U_TYPE)
    0b1101111 -> (JAL,   J_TYPE)
    0b1100111 -> (JALR,  I_TYPE)
    0b1100011 -> case func3 of
      0b000 -> (BEQ,  B_TYPE)
      0b001 -> (BNE,  B_TYPE)
      0b100 -> (BLT,  B_TYPE)
      0b101 -> (BGE,  B_TYPE)
      0b110 -> (BLTU, B_TYPE)
      0b111 -> (BGEU, B_TYPE)
    0b0000011 -> case func3 of
      0b000 -> (LB,  I_TYPE)
      0b001 -> (LH,  I_TYPE)
      0b010 -> (LW,  I_TYPE)
      0b100 -> (LBU, I_TYPE)
      0b101 -> (LHU, I_TYPE)
    0b0100111 -> case func3 of
      0b000 -> (SB, S_TYPE)
      0b001 -> (SH, S_TYPE)
      0b010 -> (SW, S_TYPE)
    0b0010011 -> case func3 of
      0b000 -> (ADDI,  I_TYPE)
      0b010 -> (SLTI,  I_TYPE)
      0b011 -> (SLTIU, I_TYPE)
      0b100 -> (XORI,  I_TYPE)
      0b110 -> (ORI,   I_TYPE)
      0b111 -> (ANDI,  I_TYPE)
    0b0110011 -> case func3 of
      0b000 -> case bit30 of
        0 -> (ADD, R_TYPE)
        1 -> (SUB, R_TYPE)
      0b001 -> (SLL,  R_TYPE)
      0b010 -> (SLT,  R_TYPE)
      0b011 -> (SLTU, R_TYPE)
      0b100 -> (XOR,  R_TYPE)
      0b101 -> case bit30 of
        0 -> (SRL, R_TYPE)
        1 -> (SRA, R_TYPE)
      0b110 -> (OR,  R_TYPE)
      0b111 -> (AND, R_TYPE)
    0b0001111 -> case func3 of
      0b000 -> (FENCE,   F_TYPE)
      0b001 -> (FENCE_I, F_TYPE)
    0b1110011 -> case func3 of
      0b000 -> case bit20 of
        0 -> (ECALL,  I_TYPE)
        1 -> (EBREAK, I_TYPE)

decodeStage
  :: BitVector NumBits                        -- Raw BitVector of instruction
  -> InstrType                                -- Instruction type
  -> RegFile                                  -- Register file
  -> (Reg, Reg, Reg, RegIdx, RegIdx, RegIdx)  -- (rs1, rs2, imm, rs1_idx, rs2_idx, rd_idx)
decodeStage i iType regs = (rs1, rs2, imm, rs1_idx, rs2_idx, rd_idx) where
  -- Register indices
  rs1_idx = unpack (slice d19 d15 $ pack i)
  rs2_idx = unpack (slice d24 d20 $ pack i)
  rd_idx  = (unpack $ slice d11 d7 i) :: RegIdx
  -- Register values
  rs1 = regs !! rs1_idx
  rs2 = regs !! rs2_idx
  -- Immediate types
  immI = (signExtend (unpack (slice d31 d20 i))) :: Reg
  immS = (signExtend (unpack ((slice d31 d25 i) ++# (slice d11 d7 i)))) :: Reg
  immB = (signExtend (unpack ((slice d31 d31 i) ++# (slice d7 d7 i) ++# (slice d30 d25 i) ++# (slice d11 d8 i) ++# (0 :: BitVector 1)))) ::Reg
  immU = (unpack ((slice d31 d12 i) ++# (0 :: BitVector 12))) :: Reg
  immJ = (signExtend (unpack ((slice d31 d31 i) ++# (slice d19 d12 i) ++# (slice d20 d20 i) ++# (slice d30 d21 i) ++# (0 :: BitVector 1)))) :: Reg
  -- Final immediate
  imm  = case iType of
    I_TYPE -> immI
    S_TYPE -> immS
    B_TYPE -> immB
    U_TYPE -> immU
    J_TYPE -> immJ

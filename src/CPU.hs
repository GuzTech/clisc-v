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

module CPU where

import Clash.Prelude
import Debug.Trace
import qualified Data.List as L

import Types
import Instructions
import ALU

cpu
  :: ( RegFile                 -- Current register file
     , U32                     -- Current program counter
     , Instr                   -- Previous instruction
     , RegIdx                  -- Previous destination register
     , Maybe U32               -- Previous instruction memory address
     , Maybe U32               -- Previous data memory address
     , Bool                    -- Is a data fetch going on?
     , Reg
     , Reg)
  -> (Maybe (BitVector NumBits), Maybe (BitVector NumBits))     -- Instruction & data input
  -> ((RegFile, U32, Instr, RegIdx, Maybe U32, Maybe U32, Bool, Reg, Reg), -- New internal state
      (RegFile, Maybe U32, Instr, Maybe U32, BV32, U32, Reg))   -- Output
cpu ( regs
    , pc
    , prev_instr
    , prev_rd_idx
    , curr_imem_addr
    , curr_dmem_addr
    , curr_data_fetch
    , prev_imm
    , prev_rs1)
  (instrM , dataM) =
  (
    ( regs'
    , pc_next
    , curr_instr
    , curr_rd_idx
    , next_imem_addr
    , next_dmem_addr
    , next_data_fetch
    , curr_imm
    , curr_rs1)
  , ( regs'
    , curr_imem_addr
    , instr
    , curr_dmem_addr
    , i
    , pc
    , curr_rs1 )
  ) where
  -- Decoding stage
  i = case instrM of
    Just instr -> instr
    Nothing    -> undefined

  i_valid = case instrM of
    Just _  -> True
    Nothing -> False

  d_valid = case dataM of
    Just _  -> True
    Nothing -> False

  stall = ((not curr_data_fetch) && (not i_valid)) ||
          (curr_data_fetch && (not d_valid))

  (instr, instr_type)                       = decodeInstr i
  (rs1, rs2, imm, rs1_idx, rs2_idx, rd_idx) = decodeStage i instr_type regs
  rs1u = unpack $ pack rs1 :: UReg
  rs2u = unpack $ pack rs2 :: UReg
  curr_rs1 = case i_valid of
    False -> prev_rs1
    True  -> rs1
  
  curr_rd_idx = case i_valid of
    True -> rd_idx
    _    -> prev_rd_idx

  curr_imm = case i_valid of
    True -> imm
    _    -> prev_imm

  curr_instr = case i_valid of
    True -> instr
    _    -> prev_instr

  -- ALU stage
  alu_result = alu (instr, (curr_rs1, rs2, imm, rd_idx))

  -- Write-back stage
  pcS = unpack (pack pc) :: S32
  regs' = case (stall, i_valid) of
    (False, True) -> case instr_type of
      S_TYPE -> regs
      B_TYPE -> regs
      U_TYPE -> case instr of
        LUI   -> replace rd_idx alu_result regs
        AUIPC -> replace rd_idx (pcS + imm) regs
      _      -> case instr of
        JAL  -> replace rd_idx (pcS + 4) regs
        JALR -> replace rd_idx (pcS + 4) regs
        LW   -> case dataM of
          Just d  -> replace rd_idx (unpack d) regs
          Nothing -> regs
        _    -> replace rd_idx alu_result regs
    (_, _) -> regs

  -- Program counter
  pc_nextS = case (stall, i_valid) of
    (False, True) -> case instr of
      JAL   -> pcS + imm
      JALR  -> unpack ((slice d31 d1 $ rs1 + imm) ++# 0b0)
      BEQ   -> case (rs1 == rs2) of
        True -> pcS + imm
        _    -> pcS + 4
      BNE   -> case (rs1 /= rs2) of
        True -> pcS + imm
        _    -> pcS + 4
      BLT   -> case (rs1 < rs2) of
        True -> pcS + imm
        _    -> pcS + 4
      BGE   -> case (rs1 >= rs2) of
        True -> pcS + imm
        _    -> pcS + 4
      BLTU  -> case (rs1u < rs2u) of
        True -> pcS + imm
        _    -> pcS + 4
      BGEU  -> case (rs1u >= rs2u) of
        True -> pcS + imm
        _    -> pcS + 4
      _     -> pcS + 4
    (_, _) -> pcS
  
  pc_next = unpack $ pack pc_nextS

  -- Instruction memory address
  next_imem_addr = case (i_valid, curr_instr) of
    (True, LW) -> Nothing
    (True, _)  -> Just pc_next
    (False, _) -> Just pc

  -- Data memory address
  load_addr = (unpack (pack (curr_rs1 + curr_imm))) :: U32
  (next_data_fetch, next_dmem_addr) = case (d_valid, curr_instr) of
    (False, LW) -> (True,  Just load_addr)
    (_, _)      -> (False, Nothing)

cpuM = mealy cpu ((repeat 0) :: RegFile, 0, ADDI, 0, Nothing, Nothing, False, 0, 0)

test_cpuM = printX $ L.take 10 $ simulate_lazy @System cpuM [
  (Nothing, Nothing),
  (Nothing, Nothing),
  (Just 0b00000000000100001000000010010011, Nothing),
  (Nothing, Nothing),
  (Just 0b00000000110000000010000100000011, Nothing),
  (Nothing, Nothing),
  (Nothing, Just 0b01010101010101010101010101010101),
  (Nothing, Nothing),
  (Just 0b00000000000000000000111111100111, Nothing),
  (Nothing, Nothing)]

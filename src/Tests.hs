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

module Tests where

import Clash.Prelude
import qualified Data.List as L

import Types
import Instructions
import ALU
import CPU
import Memory
import SoC

--regs = (repeat 4) :: RegFile
--regs' = $(listToVecTH [(0 :: Signed 32)..31]) :: RegFile
--
--instr_bits = ((-1)      :: BitVector 12) ++# -- imm
--             (0         :: BitVector 5) ++#  -- rs1
--             (0b000     :: BitVector 3) ++#  -- func3
--             (0         :: BitVector 5) ++#  -- rd
--             (0b0010011 :: BitVector 7)      -- ADDI
--d = decodeInstr instr_bits
--ds = decodeStage instr_bits (snd d) regs
--a = alu (fst d, ds)
--c = cpu (regs, 0) $ Just instr_bits
--
--xor_instr = (0         :: BitVector 7) ++# -- func7
--            (2         :: BitVector 5) ++# -- rs2
--            (4         :: BitVector 5) ++# -- rs1
--            (0b100     :: BitVector 3) ++# -- func3
--            (0         :: BitVector 5) ++# -- rd
--            (0b0110011 :: BitVector 7)     -- XOR
--c_xor = cpu (regs', 0) $ Just xor_instr
--
--bne_instr = 0b11111110010000010001111111100011 -- BNE (rs1_idx = 2, rs2_idx = 4, imm = -1)
--c_bne = cpu (regs', 0) $ Just bne_instr
--
--jal_instr = ((-1)      :: BitVector 20) ++# -- imm
--            (31        :: BitVector 5) ++#  -- rd
--            (0b1101111 :: BitVector 7)      -- JAL
--c_jal = cpu (regs', 0) $ Just jal_instr
--
--jalr_instr = ((-1)      :: BitVector 12) ++# -- imm
--             (3         :: BitVector 5) ++#  -- rs1
--             (0b000     :: BitVector 3) ++#  -- func3
--             (31        :: BitVector 5) ++#  -- rd
--             (0b1100111 :: BitVector 7)      -- JAL
--c_jalr = cpu (regs', 0) $ Just jalr_instr

-- CPU (Mealy version)
cM = L.take 6 $ simulate_lazy cpuM [(Just 0b00000000001100000000000010010011, Nothing) :: (Maybe BV32, Maybe BV32), (Just 0b00000000000100000000000100010011, Nothing) , (Nothing, Nothing), (Just 0b01000000001000001000000110110011, Nothing), (Just 0b00000000000000000000111111100111, Nothing), (Nothing, Nothing)]

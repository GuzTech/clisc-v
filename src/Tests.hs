module Tests where

import Clash.Prelude

import Types
import Instructions
import ALU
import CPU

regs = (repeat 4) :: RegFile
regs' = $(listToVecTH [(0 :: Signed 32)..31]) :: RegFile

instr_bits = ((-1)      :: BitVector 12) ++# -- imm
             (0         :: BitVector 5) ++#  -- rs1
             (0b000     :: BitVector 3) ++#  -- func3
             (0         :: BitVector 5) ++#  -- rd
             (0b0010011 :: BitVector 7)      -- ADDI
d = decodeInstr instr_bits
ds = decodeStage instr_bits (snd d) regs
a = alu (fst d, ds)
c = cpu (regs, 0) instr_bits

xor_instr = (0         :: BitVector 7) ++# -- func7
            (2         :: BitVector 5) ++# -- rs2
            (4         :: BitVector 5) ++# -- rs1
            (0b100     :: BitVector 3) ++# -- func3
            (0         :: BitVector 5) ++# -- rd
            (0b0110011 :: BitVector 7)     -- XOR
c_xor = cpu (regs', 0) xor_instr

bne_instr = 0b11111110010000010001111111100011 -- BNE (rs1_idx = 2, rs2_idx = 4, imm = -1)
c_bne = cpu (regs', 0) bne_instr

jal_instr = ((-1)      :: BitVector 20) ++# -- imm
            (31        :: BitVector 5) ++#  -- rd
            (0b1101111 :: BitVector 7)      -- JAL
c_jal = cpu (regs', 0) jal_instr

jalr_instr = ((-1)      :: BitVector 12) ++# -- imm
             (3         :: BitVector 5) ++#  -- rs1
             (0b000     :: BitVector 3) ++#  -- func3
             (31        :: BitVector 5) ++#  -- rd
             (0b1100111 :: BitVector 7)      -- JAL
c_jalr = cpu (regs', 0) jalr_instr

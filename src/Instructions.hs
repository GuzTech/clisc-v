module Instructions where

import Clash.Prelude
import Types

data Instr = LUI Ridx Imm20         -- Load upper immediate (rd)
           | AUIPC Ridx Imm20       -- Add upper immediate to pc (rd)
           | JAL Ridx Imm20         -- (rd)
           | JALR Ridx Ridx Imm12   -- (rs1, rd)
           | BEQ Ridx Ridx Imm12    -- (rs1, rs2)
           | BNE Ridx Ridx Imm12    -- (rs1, rs2)
           | BLT Ridx Ridx Imm12    -- (rs1, rs2)
           | BGE Ridx Ridx Imm12    -- (rs1, rs2)
           | BLTU Ridx Ridx Imm12   -- (rs1, rs2)
           | BGEU Ridx Ridx Imm12   -- (rs1, rs2)
           | LB
           | LH
           | LW
           | LBU
           | LHU
           | SB
           | SH
           | SW
           | ADDI Ridx Ridx Imm12   -- Add s-ext imm to rs1 (rs1, rd)
           | SLTI Ridx Ridx Imm12   -- Set less than imm (rs1, rd)
           | SLTIU Ridx Ridx UImm12 -- Set less than unsigned imm (rs1, rd)
           | XORI Ridx Ridx Imm12   -- XOR s-ext imm with rs1 (rs1, rd)
           | ORI Ridx Ridx Imm12    -- OR s-ext imm with rs1 (rs1, rd)
           | ANDI Ridx Ridx Imm12   -- AND s-ext imm with rs1 (rs1, rd)
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
           | FENCEI
           | ECALL
           | EBREAK
           | CSRRW
           | CSRRS
           | CSRRC
           | CSRRWI
           | CSRRSI
           | CSRRCI
           deriving (Show, Eq)



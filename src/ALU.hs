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

module ALU where

import Clash.Prelude

import Types
import Instructions

sToU :: KnownNat n => Signed n -> Unsigned n
sToU s = unpack $ pack s

alu
  :: (Instr, (Reg, Reg, Reg, RegIdx))
  -> Reg
alu (instr, (rs1, rs2, imm, rd_idx)) = rd where
  shamt = (unpack $ slice d4 d0 imm) :: Unsigned 5
  rs1u = sToU rs1
  rs2u = sToU rs2
  immu = sToU imm
  rd = case instr of
    LUI   -> imm
    ADDI  -> rs1 + imm
    SLTI  -> unpack $ boolToBV $ rs1 < imm
    SLTIU -> unpack $ boolToBV $ rs1u < immu
    XORI  -> rs1 `xor` imm
    ORI   -> rs1 .|. imm
    ANDI  -> rs1 .&. imm
    SLLI  -> shiftL rs1 $ fromIntegral shamt
    SRLI  -> unpack (pack $ shiftR rs1u (fromIntegral shamt))
    SRAI  -> shiftR rs1 $ fromIntegral shamt
    ADD   -> rs1 + rs2
    SLT   -> unpack $ boolToBV $ rs1 < rs2
    SLTU  -> unpack $ boolToBV $ rs1u < rs2u
    AND   -> rs1 .&. rs2
    OR    -> rs1 .|. rs2
    XOR   -> rs1 `xor` rs2
    SLL   -> shiftL rs1 $ fromIntegral ((resize rs2u) :: Unsigned 5)
    SRL   -> unpack (pack $ shiftR rs1u $ fromIntegral ((resize rs2u) :: Unsigned 5))
    SUB   -> rs1 - rs2
    SRA   -> shiftR rs1 $ fromIntegral rs2

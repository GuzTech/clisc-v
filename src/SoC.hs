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

module SoC where

import Clash.Prelude

import Types
import Instructions
import CPU
import Memory

soc
  :: (Signal System (Maybe (BitVector NumBits), Maybe (BitVector NumBits)) -> Signal System (RegFile, Maybe U32, Instr, Maybe U32, BV32, U32, Reg))
  -> (Signal System (Maybe U32) -> Signal System (Maybe BV32))
  -> (Signal System (Maybe U32) -> Signal System (Maybe BV32))
  -> Signal System (Maybe U32, Instr, Maybe U32, RegFile)
soc cpu imem dmem = o where
  iM = imem imem_addr
  dM = dmem dmem_addr
  (regs, imem_addr, instr, dmem_addr, i, pc, rs1) = unbundle (cpu $ bundle (iM, dM))
  o = bundle (imem_addr, instr, dmem_addr, regs)

soc_inst = soc cpuM memory memory

{-# ANN topEntity
  (Synthesize
    { t_name   = "soc"
    , t_inputs = []
    , t_output = PortProduct "" [PortName "imem_addr", PortName "instr", PortName "dmem_addr", PortName "regs"]
    }
  ) #-}
topEntity
  :: Clock System
  -> Reset System
  -> Signal System (Maybe U32, Instr, Maybe U32, RegFile)
topEntity clk rst = exposeClockResetEnable soc_inst clk rst enableGen
  
main = do
  putStrLn "(imem_addr:4),(raw_instr:32),(dmem_addr:-1),(regs:-1)"
  printX $ sampleN_lazy 10 $ soc_inst

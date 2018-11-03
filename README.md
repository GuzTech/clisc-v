# CLISC-V: A RISC-V (RV32I) processor written in CLaSH

CLISC-V is a RISC-V processor that implements the RV32I instruction set. It is written in [CLaSH](https://clash-lang.org), a functional hardware description language. The aim for this implementation is to be easy to read and straightforward.

## Status

The following instructions have not yet been implemented:
 - LB(U), LH(U), LW
 - SB, SH, SW
 - FENCE, FENCE.I
 - ECALL, EBREAK
 - CSRR[W/S/C/WI/SI/CI]

The memory access subsystem has also not been implemented yet. The other instructions are implemented but have not been extensively tested. The goal is to use CLaSH to generate Verilog and use [SymbiYosys](https://github.com/YosysHQ/SymbiYosys) and [riscv-formal](https://github.com/cliffordwolf/riscv-formal) to formally verify the core.

## Licence

CLISC-V is free and open hardware and is licensed under the [ISC licence](http://en.wikipedia.org/wiki/ISC_license).
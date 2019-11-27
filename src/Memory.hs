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

module Memory where

import Clash.Prelude
import Debug.Trace
import qualified Data.List as L

import Types

-- Configuration for the memory
mem_delay_cycles = 1 :: Unsigned 4
mem_size_words   = 4 :: Unsigned 3
mem_file         = "memory.txt"

mem
  :: Unsigned 4               -- Internal counter used for delaying the memory request
  -> Maybe U32                -- Memory address
  -> (Unsigned 4, Maybe BV32) -- Output will be valid after a specified delay
mem cnt addrM = (cnt', dataM) where
  req_valid = case addrM of
    Just _  -> True
    Nothing -> False

  cnt' | cnt == mem_delay_cycles = 0
       | otherwise               = cnt + 1
  
  -- Just use lower 8 bits of the address.
  -- Needed for not blowing up memory usage.
  addr = case addrM of
    Just a  -> a
    Nothing -> 0

  a          = unpack $ slice d4 d2 addr
  addr_valid = (a < mem_size_words) && req_valid

  dataM | cnt == mem_delay_cycles = case addr_valid of
                                      True -> Just $ asyncRomFilePow2 mem_file a
                                      _    -> Nothing
        | otherwise               = Nothing

memory = mealy mem 0

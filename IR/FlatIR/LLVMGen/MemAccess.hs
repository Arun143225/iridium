-- Copyright (c) 2013 Eric McCorkle.
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
-- 02110-1301 USA
{-# OPTIONS_GHC -Wall -Werror #-}

-- | This module contains common code for generating memory accesses.
--
-- Note: this will probably get absorbed into whatever codegen monad I
-- end up creating.
module IR.FlatIR.LLVMGen.MemAccess(
       genLoad,
--       genStore
       ) where

import Data.Word
import Data.Interval(Interval(..), toIntervalList)
import IR.FlatIR.Syntax

import qualified LLVM.Core as LLVM

-- | Generate a load, adding the necessary metadata and volatility.
genLoad :: LLVM.ContextRef
        -- ^ The LLVM Context
        -> LLVM.BuilderRef
        -- ^ The LLVM Instruction builder handle
        -> LLVM.ValueRef
        -- ^ The pointer LLVM value
        -> Mutability
        -- ^ The Mutability of the value being loaded
        -> Type
        -- ^ The type of the value being loaded
        -> IO LLVM.ValueRef
        -- ^ The load instruction LLVM value
genLoad ctx builder addr mut ty =
  let
    setVolatility :: Mutability -> LLVM.ValueRef -> IO ()
    setVolatility Volatile val = LLVM.setVolatile val True
    setVolatility VolatileOnce val = LLVM.setVolatile val True
    setVolatility _ _ = return ()

    -- XXX Also do TBAA here when we have the machinery for it
    addTypeMetadata :: LLVM.ValueRef -> Type -> IO ()
    addTypeMetadata val IntType { intSize = size, intSigned = signed,
                                  intIntervals = intervals } =
      let
        extremelow :: Integer
        extremelow
          | signed = negate (2 ^ (size - 1))
          | otherwise = 0

        extremehigh :: Integer
        extremehigh
          | signed = (2 ^ (size - 1)) - 1
          | otherwise = 2 ^ size

        intervalVals :: LLVM.TypeRef -> Interval Integer -> [LLVM.ValueRef]
        intervalVals llvmty (Interval low high) =
          [ LLVM.constInt llvmty low signed,
            LLVM.constInt llvmty (high + 1) signed ]
        intervalVals llvmty (Single single) =
          [ LLVM.constInt llvmty single signed,
            LLVM.constInt llvmty (single + 1) signed ]
        intervalVals llvmty (Min low) =
          [ LLVM.constInt llvmty low signed,
            LLVM.constInt llvmty extremehigh signed ]
        intervalVals llvmty (Max high) =
          [ LLVM.constInt llvmty extremelow signed,
            LLVM.constInt llvmty high signed ]
      in do
        mdkind <- LLVM.getMDKindIDInContext ctx "range"
        intty <- LLVM.intTypeInContext ctx size
        md <- LLVM.mdNodeInContext ctx (concat (map (intervalVals intty)
                                                    (toIntervalList intervals)))
        LLVM.setMetadata val (mdkind :: Word) md
    addTypeMetadata _ _ = return ()
  in do
    out <- LLVM.buildLoad builder addr ""
    setVolatility mut out
    addTypeMetadata out ty
    return out

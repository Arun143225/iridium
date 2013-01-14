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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

-- | This module defines a class of things that can be directly
-- converted into LLVM values.
module IR.FlatIR.LLVMGen.LLVMValue(
       LLVMValue(..)
       ) where

import IR.FlatIR.Syntax

import qualified LLVM.Core as LLVM

-- | This class represents things that can be directly and easily
-- converted into LLVM values.  Most of these will be wrappers for
-- values in some way; conversion of most things requires more
-- context.
class LLVMValue a where
  -- | Convert an object into an LLVM value
  toValue :: a -> LLVM.ValueRef

instance LLVMValue Fieldname where
  toValue (Fieldname fname) = LLVM.constInt LLVM.int32Type fname False
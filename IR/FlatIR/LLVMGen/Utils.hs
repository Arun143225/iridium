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
{-# OPTIONS_GHC -Wall #-}

-- | Utility code for compiling to LLVM.  (This may be merged into
-- SimpleIR itself)
module IR.FlatIR.LLVMGen.Utils(
       booltype,
       getGlobalType,
       getGlobalMutability,
       getActualType
       ) where

import Data.Array.IArray
import Data.Graph.Inductive
import Data.Interval
import Data.Pos
import IR.FlatIR.Syntax

-- | The Flat IR type representing booleans.
booltype :: Pos -> Type
booltype p = IntType { intSigned = False, intSize = 1, intPos = p,
                       intIntervals = fromIntervalList [Interval 0 1] }

-- | Get the type of a global, constructing a function type if need
-- be.
getGlobalType :: Graph gr => Module gr -> Globalname -> Type
getGlobalType (Module { modGlobals = globals}) name =
  case globals ! name of
    Function { funcRetTy = retty, funcValTys = valtys,
               funcParams = params, funcPos = p } ->
      FuncType { funcTyRetTy = retty, funcTyPos = p,
                 funcTyArgTys = (map ((!) valtys) params) }
    GlobalVar { gvarTy = ty } -> ty

-- | Get the mutability of a global.  Note that all functions are
-- immutable.
getGlobalMutability :: Graph gr => Module gr -> Globalname -> Mutability
getGlobalMutability (Module { modGlobals = globals }) name =
  case globals ! name of
    GlobalVar { gvarMutability = mut } -> mut
    Function {} -> Immutable

-- | Chase down references and get a concrete type (if it
-- leads to an opaque type, then return the named type
getActualType :: Graph gr => Module gr -> Type -> Type
getActualType irmodule @ (Module { modTypes = types })
              idty @ (IdType { idName = tyname }) =
  case types ! tyname of
    (_, Just ty) -> getActualType irmodule ty
    _ -> idty
getActualType _ ty = ty

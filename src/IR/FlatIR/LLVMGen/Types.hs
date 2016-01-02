-- Copyright (c) 2015 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

-- | This module contains code that converts FlatIR types into LLVM types.
module IR.FlatIR.LLVMGen.Types(
       toLLVMType
       ) where

import Data.Array
import Data.Graph.Inductive.Graph
import IR.Common.Ptr
import IR.FlatIR.Syntax
import Prelude hiding (mapM_, mapM, foldr, foldl, sequence)

import qualified Data.ByteString.UTF8 as Strict
import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.AddrSpace as LLVM
import qualified LLVM.General.AST.Type as LLVM

-- | Generate the LLVM type for a given Flat IR type.
toLLVMType :: Graph gr =>
              Module tagty typedesc gr
           -- ^ The FlatIR module being translated
           -> Type tagty
           -> LLVM.Type
           -- ^ The corresponding LLVM type
toLLVMType m FuncType { funcTyRetTy = retty, funcTyArgTys = argtys } =
  let
    retty' = toLLVMType m retty
    argtys' = map (toLLVMType m) argtys
  in
   LLVM.FunctionType { LLVM.resultType = retty', LLVM.argumentTypes = argtys',
                       LLVM.isVarArg = False }
toLLVMType m StructType { structPacked = packed, structFields = fields } =
  let
    fieldtys = map (\(_, _, ty) -> toLLVMType m ty) (elems fields)
  in
   LLVM.StructureType { LLVM.elementTypes = fieldtys, LLVM.isPacked = packed }
toLLVMType m ArrayType { arrayLen = Just size, arrayElemTy = inner } =
  let
    inner' = toLLVMType m inner
  in
   LLVM.ArrayType { LLVM.nArrayElements = fromIntegral size,
                    LLVM.elementType = inner' }
toLLVMType m ArrayType { arrayLen = Nothing, arrayElemTy = inner } =
  let
    inner' = toLLVMType m inner
  in
   LLVM.ArrayType { LLVM.elementType = inner', LLVM.nArrayElements = 0 }
toLLVMType m PtrType { ptrTy = Native { nativeTy = inner } } =
  let
    inner' = toLLVMType m inner
  in
   LLVM.PointerType { LLVM.pointerReferent = inner',
                      LLVM.pointerAddrSpace = LLVM.AddrSpace 0 }
toLLVMType Module { modTypes = types, modTags = tags }
           PtrType { ptrTy = Tagged { taggedTag = tagid } } =
  let
    TagDesc { tagDescTy = tname } = tags ! tagid
    innerty' = case types ! tname of
      Anon {} -> LLVM.UnName $! fromIntegral $! fromEnum tagid
      Name { nameStr = bstr } -> LLVM.Name $! Strict.toString bstr
      TypeDef { typeDefStr = bstr } -> LLVM.Name $! Strict.toString bstr

  in
   LLVM.PointerType { LLVM.pointerReferent = LLVM.NamedTypeReference $!
                                             innerty',
                      LLVM.pointerAddrSpace = LLVM.AddrSpace 0 }
toLLVMType Module { modTypes = types } IdType { idName = tyid } =
  let
    tyname = case types ! tyid of
      Anon {} -> LLVM.UnName $! fromIntegral $! fromEnum tyid
      Name { nameStr = bstr } -> LLVM.Name $! Strict.toString bstr
      TypeDef { typeDefStr = bstr } -> LLVM.Name $! Strict.toString bstr
  in
    LLVM.NamedTypeReference $! tyname
toLLVMType _ IntType { intSize = 1 } = LLVM.i1
toLLVMType _ IntType { intSize = 8 } = LLVM.i8
toLLVMType _ IntType { intSize = 16 } = LLVM.i16
toLLVMType _ IntType { intSize = 32 } = LLVM.i32
toLLVMType _ IntType { intSize = 64 } = LLVM.i64
toLLVMType _ IntType { intSize = size } =
  LLVM.IntegerType { LLVM.typeBits = fromIntegral size }
toLLVMType _ FloatType { floatSize = 16 } = LLVM.half
toLLVMType _ FloatType { floatSize = 32 } = LLVM.float
toLLVMType _ FloatType { floatSize = 64 } = LLVM.double
toLLVMType _ FloatType { floatSize = 80 } = LLVM.x86_fp80
toLLVMType _ FloatType { floatSize = 128 } = LLVM.fp128
toLLVMType _ FloatType { floatSize = n } =
  error ("Cannot generate floating point type with " ++ show n ++ " bits")
toLLVMType _ (UnitType _) = error "Don't generate LLVM for UnitType"

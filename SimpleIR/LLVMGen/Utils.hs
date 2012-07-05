-- Copyright (c) 2012 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
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

-- | Utility code for compiling to LLVM.  (This may be merged into
-- SimpleIR itself)
module SimpleIR.LLVMGen.Utils(
       booltype,
       getGlobalType,
       getActualType,
       toLLVMType
       ) where

import Data.Array(Array)
import Data.Array.IArray
import Data.Array.Unboxed(UArray)
import Data.Graph.Inductive
import SimpleIR

import qualified LLVM.Core as LLVM

-- | The Simple IR type representing booleans.
booltype :: Type
booltype = IntType False 1

-- | Get the type of a global, constructing a function type if need
-- be.
getGlobalType :: Graph gr => Module gr -> Globalname -> Type
getGlobalType (Module { modGlobals = globals}) name =
  case globals ! name of
    Function { funcRetTy = retty, funcValTys = valtys,
               funcParams = params} ->
      FuncType retty (map ((!) valtys) params)
    GlobalVar { gvarTy = ty } -> ty

-- | Chase down references and get a concrete type (if it
-- leads to an opaque type, then return the named type
getActualType :: Graph gr => Module gr -> Type -> Type
getActualType mod @ (Module { modTypes = types }) (IdType tyname) =
  case types ! tyname of
    (_, Just ty) -> getActualType mod ty
    _ -> IdType tyname
getActualType _ ty = ty

-- | Generate the LLVM type for a given Simple IR type.
toLLVMType :: Graph gr => Module gr -> LLVM.ContextRef ->
              UArray Typename LLVM.TypeRef -> Type -> IO LLVM.TypeRef
toLLVMType (Module { modGCHeaders = gcheaders }) ctx types =
  let
    toLLVMType' :: Type -> IO LLVM.TypeRef
    toLLVMType' (StructType packed fields) =
      do
        fieldtys <- mapM (\(_, _, ty) -> toLLVMType' ty) (elems fields)
        LLVM.structTypeInContext ctx fieldtys packed
    toLLVMType' (ArrayType (Just size) inner) =
      do
        inner <- toLLVMType' inner
        return (LLVM.arrayType inner size)
    toLLVMType' (ArrayType Nothing inner) =
      do
        inner <- toLLVMType' inner
        return (LLVM.arrayType inner 0)
    toLLVMType' (PtrType (BasicObj inner)) =
       do
        inner <- toLLVMType' inner
        return (LLVM.pointerType inner 0)
    toLLVMType' (PtrType (GCObj _ id)) =
      let
        (tname, _, _) = gcheaders ! id
      in
        return (LLVM.pointerType (types ! tname) 0)
    toLLVMType' (IdType id) = return (types ! id)
    toLLVMType' (IntType _ 1) = LLVM.int1TypeInContext ctx
    toLLVMType' (IntType _ 8) = LLVM.int8TypeInContext ctx
    toLLVMType' (IntType _ 16) = LLVM.int16TypeInContext ctx
    toLLVMType' (IntType _ 32) = LLVM.int32TypeInContext ctx
    toLLVMType' (IntType _ 64) = LLVM.int64TypeInContext ctx
    toLLVMType' (IntType _ size) = LLVM.intTypeInContext ctx size
    toLLVMType' (FloatType 32) = LLVM.floatTypeInContext ctx
    toLLVMType' (FloatType 64) = LLVM.doubleTypeInContext ctx
    toLLVMType' (FloatType 128) = LLVM.fp128TypeInContext ctx
  in
    toLLVMType'

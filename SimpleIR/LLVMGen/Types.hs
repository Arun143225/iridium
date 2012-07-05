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

module SimpleIR.LLVMGen.Types(
       genTypeDefs,
       toLLVMType
       ) where

import Data.Array.IO hiding (unsafeFreeze)
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.Foldable
import Data.Graph.Inductive.Graph
import Data.Traversable
import Foreign.Ptr
import Prelude hiding (mapM_, mapM, foldr, foldl, sequence)
import SimpleIR

import qualified LLVM.Core as LLVM

-- | Generate an array mapping typenames to LLVM types.
genTypeDefs :: Graph gr => Module gr -> LLVM.ContextRef ->
               IO (UArray Typename LLVM.TypeRef)
genTypeDefs (Module { modTypes = types, modGCHeaders = gcheaders }) ctx =
  let
    -- Fill in the array of types
    initTypeArray :: IOUArray Typename LLVM.TypeRef -> IO ()
    initTypeArray typemap =
      let
        -- Translate a SimpleIR type into an LLVM type.  We need
        -- the map from typenames to (uninitialized) LLVM types to
        -- do this.
        genLLVMType :: Type -> IO LLVM.TypeRef
        genLLVMType (StructType packed fields) =
          do
            fieldtys <- mapM (\(_, _, ty) -> genLLVMType ty) (elems fields)
            LLVM.structTypeInContext ctx fieldtys packed
        genLLVMType (ArrayType (Just size) inner) =
          do
            inner <- genLLVMType inner
            return (LLVM.arrayType inner size)
        genLLVMType (ArrayType Nothing inner) =
          do
            inner <- genLLVMType inner
            return (LLVM.arrayType inner 0)
        genLLVMType (PtrType (BasicObj inner)) =
          do
            inner <- genLLVMType inner
            return (LLVM.pointerType inner 0)
        genLLVMType (PtrType (GCObj _ id)) =
          let
            (tname, _, _) = gcheaders ! id
          in do
            innerty <- updateEntry tname
            return (LLVM.pointerType innerty 0)
        genLLVMType (IdType id) = updateEntry id
        genLLVMType (IntType _ 1) = LLVM.int1TypeInContext ctx
        genLLVMType (IntType _ 8) = LLVM.int8TypeInContext ctx
        genLLVMType (IntType _ 16) = LLVM.int16TypeInContext ctx
        genLLVMType (IntType _ 32) = LLVM.int32TypeInContext ctx
        genLLVMType (IntType _ 64) = LLVM.int64TypeInContext ctx
        genLLVMType (IntType _ size) = LLVM.intTypeInContext ctx size
        genLLVMType (FloatType 32) = LLVM.floatTypeInContext ctx
        genLLVMType (FloatType 64) = LLVM.doubleTypeInContext ctx
        genLLVMType (FloatType 128) = LLVM.fp128TypeInContext ctx

        -- Grab the type entry for this type name, possibly
        -- (re)initializing it
        updateEntry :: Typename -> IO LLVM.TypeRef
        updateEntry ind =
          case types ! ind of
            (_, Just ty @ (StructType packed fields)) ->
              do
                ent <- readArray typemap ind
                if LLVM.isOpaqueStruct ent
                  then do
                    fieldtys <- mapM (\(_, _, ty) -> genLLVMType ty)
                                     (elems fields)
                    LLVM.structSetBody ent fieldtys packed
                    return ent
                  else return ent
            (_, Just ty) ->
              do
                ent <- readArray typemap ind
                if ent == nullPtr
                  then do
                    newty <- genLLVMType ty
                    writeArray typemap ind newty
                    return ent
                  else return ent
            _ -> readArray typemap ind
      in
        mapM_ (\ind -> updateEntry ind >> return ()) (indices types)

    -- Initialize structures and opaques to empty named structures and
    -- everything else to null pointers.
    initEntry :: (String, Maybe Type) -> IO LLVM.TypeRef
    initEntry (str, Nothing) = LLVM.structCreateNamed ctx str
    initEntry (str, Just (StructType _ _)) = LLVM.structCreateNamed ctx str
    initEntry _ = return nullPtr
  in do
    elems <- mapM initEntry (elems types)
    typearr <- newListArray (bounds types) elems
    initTypeArray typearr
    unsafeFreeze typearr

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

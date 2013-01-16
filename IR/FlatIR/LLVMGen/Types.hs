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

-- | This module contains code that converts FlatIR types into LLVM types.
module IR.FlatIR.LLVMGen.Types(
       genTypeDefs,
       toLLVMType
       ) where

import Data.Array.IO hiding (unsafeFreeze)
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.Foldable
import Data.Graph.Inductive.Graph
import Data.Traversable
import Data.Word
import Foreign.Ptr
import IR.FlatIR.Syntax
import Prelude hiding (mapM_, mapM, foldr, foldl, sequence)

import qualified LLVM.Core as LLVM

-- | Generate an array mapping typenames to LLVM types.
genTypeDefs :: Graph gr =>
               Module gr
            -- ^ The FlatIR module being translated
            -> LLVM.ContextRef
            -- ^ The LLVM Context handle
            -> IO (UArray Typename LLVM.TypeRef)
            -- ^ An array mapping Typenames to LLVM Type handles
genTypeDefs (Module { modTypes = types, modGCHeaders = gcheaders }) ctx =
  let
    -- Fill in the array of types
    initTypeArray :: IOUArray Typename LLVM.TypeRef -> IO ()
    initTypeArray typemap =
      let
        -- Translate a FlatIR type into an LLVM type.  We need
        -- the map from typenames to (uninitialized) LLVM types to
        -- do this.
        genLLVMType :: Type -> IO LLVM.TypeRef
        genLLVMType (FuncType { funcTyRetTy = retty,
                                funcTyArgTys = argtys }) =
          do
            retty' <- genLLVMType retty
            argtys' <- mapM genLLVMType argtys
            return (LLVM.functionType retty' argtys' False)
        genLLVMType (StructType { structPacked = packed,
                                  structFields = fields }) =
          do
            fieldtys <- mapM (\(_, _, ty) -> genLLVMType ty) (elems fields)
            LLVM.structTypeInContext ctx fieldtys packed
        genLLVMType (ArrayType { arrayLen = Just size,
                                 arrayElemTy = inner }) =
          do
            inner' <- genLLVMType inner
            return (LLVM.arrayType inner' size)
        genLLVMType (ArrayType { arrayLen = Nothing,
                                 arrayElemTy = inner }) =
          do
            inner' <- genLLVMType inner
            return (LLVM.arrayType inner' (0 :: Word))
        genLLVMType (PtrType { ptrTy = Native { nativeTy = inner } }) =
          do
            inner' <- genLLVMType inner
            return (LLVM.pointerType inner' (0 :: Word))
        genLLVMType (PtrType { ptrTy = GC { gcTy = gcid } }) =
          let
            (tname, _, _) = gcheaders ! gcid
          in do
            innerty' <- updateEntry tname
            return (LLVM.pointerType innerty' (0 :: Word))
        genLLVMType (IdType { idName = tyid }) = updateEntry tyid
        genLLVMType (IntType { intSize = 1 }) = LLVM.int1TypeInContext ctx
        genLLVMType (IntType { intSize = 8 }) = LLVM.int8TypeInContext ctx
        genLLVMType (IntType { intSize = 16 }) = LLVM.int16TypeInContext ctx
        genLLVMType (IntType { intSize = 32 }) = LLVM.int32TypeInContext ctx
        genLLVMType (IntType { intSize = 64 }) = LLVM.int64TypeInContext ctx
        genLLVMType (IntType { intSize = size }) =
          LLVM.intTypeInContext ctx size
{-
        genLLVMType (FloatType { floatSize = 16 }) =
          LLVM.halfTypeInContext ctx
-}
        genLLVMType (FloatType { floatSize = 32 }) =
          LLVM.floatTypeInContext ctx
        genLLVMType (FloatType { floatSize = 64 }) =
          LLVM.doubleTypeInContext ctx
        genLLVMType (FloatType { floatSize = 128 }) =
          LLVM.fp128TypeInContext ctx
        genLLVMType (FloatType { floatSize = n }) =
          error ("Cannot generate floating point type with " ++
                 show n ++ " bits")
        genLLVMType (UnitType _) = error "Don't generate LLVM for UnitType"

        -- Grab the type entry for this type name, possibly
        -- (re)initializing it
        updateEntry :: Typename -> IO LLVM.TypeRef
        updateEntry ind =
          case types ! ind of
            (_, Just (StructType { structPacked = packed,
                                   structFields = fields })) ->
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
    initEntry (str, Just (StructType {})) = LLVM.structCreateNamed ctx str
    initEntry _ = return nullPtr
  in do
    elems' <- mapM initEntry (elems types)
    typearr <- newListArray (bounds types) elems'
    initTypeArray typearr
    unsafeFreeze typearr

-- | Generate the LLVM type for a given Flat IR type.
toLLVMType :: Graph gr =>
              Module gr
           -- ^ The FlatIR module being translated
           -> LLVM.ContextRef
           -- ^ The LLVM Context handle
           -> UArray Typename LLVM.TypeRef
           -- ^ An array mapping Typenames to LLVM Type handles
           -> Type
           -- ^ The type for which LLVM is being generated
           -> IO LLVM.TypeRef
           -- ^ The corresponding LLVM type
toLLVMType (Module { modGCHeaders = gcheaders }) ctx types =
  let
    toLLVMType' :: Type -> IO LLVM.TypeRef
    toLLVMType' (FuncType { funcTyRetTy = retty, funcTyArgTys = argtys }) =
      do
        retty' <- toLLVMType' retty
        argtys' <- mapM toLLVMType' argtys
        return (LLVM.functionType retty' argtys' False)
    toLLVMType' (StructType { structPacked = packed,
                              structFields = fields }) =
      do
        fieldtys <- mapM (\(_, _, ty) -> toLLVMType' ty) (elems fields)
        LLVM.structTypeInContext ctx fieldtys packed
    toLLVMType' (ArrayType { arrayLen = Just size,
                             arrayElemTy = inner }) =
      do
        inner' <- toLLVMType' inner
        return (LLVM.arrayType inner' size)
    toLLVMType' (ArrayType { arrayLen = Nothing,
                             arrayElemTy = inner }) =
      do
        inner' <- toLLVMType' inner
        return (LLVM.arrayType inner' (0 :: Word))
    toLLVMType' (PtrType { ptrTy = Native { nativeTy = inner } }) =
       do
        inner' <- toLLVMType' inner
        return (LLVM.pointerType inner' (0 :: Word))
    toLLVMType' (PtrType { ptrTy = GC { gcTy = gcid } }) =
      let
        (tname, _, _) = gcheaders ! gcid
      in
        return (LLVM.pointerType (types ! tname) (0 :: Word))
    toLLVMType' (IdType { idName = tyid }) = return (types ! tyid)
    toLLVMType' (IntType { intSize = 1 }) = LLVM.int1TypeInContext ctx
    toLLVMType' (IntType { intSize = 8 }) = LLVM.int8TypeInContext ctx
    toLLVMType' (IntType { intSize = 16 }) = LLVM.int16TypeInContext ctx
    toLLVMType' (IntType { intSize = 32 }) = LLVM.int32TypeInContext ctx
    toLLVMType' (IntType { intSize = 64 }) = LLVM.int64TypeInContext ctx
    toLLVMType' (IntType { intSize = size }) =
      LLVM.intTypeInContext ctx size
{-
    toLLVMType' (FloatType { floatSize = 16 }) =
      LLVM.halfTypeInContext ctx
-}
    toLLVMType' (FloatType { floatSize = 32 }) =
      LLVM.floatTypeInContext ctx
    toLLVMType' (FloatType { floatSize = 64 }) =
      LLVM.doubleTypeInContext ctx
    toLLVMType' (FloatType { floatSize = 126 }) =
      LLVM.fp128TypeInContext ctx
    toLLVMType' (FloatType { floatSize = n }) =
      error ("Cannot generate floating point type with " ++
             show n ++ " bits")
    toLLVMType' (UnitType _) = error "Don't generate LLVM for UnitType"
  in
    toLLVMType'

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
import IR.FlatIR.LLVMGen.Context
import IR.FlatIR.Syntax
import Prelude hiding (mapM_, mapM, foldr, foldl, sequence)

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Constant as LLVM

-- | Generate an array mapping typenames to LLVM types.
genTypeDefs :: Graph gr =>
               Module gr
            -- ^ The FlatIR module being translated
            -> IO (UArray Word LLVM.Type)
            -- ^ An array mapping Typenames to LLVM Type handles
genTypeDefs Module { modTypes = types, modGCHeaders = gcheaders } ctx =
  let
    -- Fill in the array of types
    initTypeArray :: IOUArray Word LLVM.Type -> IO ()
    initTypeArray typemap =
      let
        -- Translate a FlatIR type into an LLVM type.  We need
        -- the map from typenames to (uninitialized) LLVM types to
        -- do this.
        genLLVMType :: Type -> IO LLVM.Type
        genLLVMType FuncType { funcTyRetTy = retty,
                               funcTyArgTys = argtys } =
          do
            retty' <- genLLVMType retty
            argtys' <- mapM genLLVMType argtys
            return LLVM.FunctionType { LLVM.resultType = retty',
                                       LLVM.argumentTypes = argtys',
                                       LLVM.isVarArg = False }
        genLLVMType StructType { structPacked = packed,
                                 structFields = fields } =
          do
            fieldtys <- mapM (\(_, _, ty) -> genLLVMType ty) (elems fields)
            return LLVM.StructureType { LLVM.elementTypes = fieldtys,
                                        LLVM.isPacked = packed }
        genLLVMType ArrayType { arrayLen = Just size,
                                arrayElemTy = inner } =
          do
            inner' <- genLLVMType inner
            return LLVM.ArrayType { LLVM.elementType = inner',
                                    LLVM.nArrayElements = size }
        genLLVMType ArrayType { arrayLen = Nothing,
                                arrayElemTy = inner } =
          do
            inner' <- genLLVMType inner
            return LLVM.ArrayType { LLVM.elementType = inner',
                                    LLVM.nArrayElements = 0 }
        genLLVMType PtrType { ptrTy = Native { nativeTy = inner } } =
          do
            inner' <- genLLVMType inner
            return LLVM.PointerType { LLVM.pointerReferent = inner',
                                      LLVM.pointerAddrSpace = 0 }
        genLLVMType PtrType { ptrTy = GC { gcTy = gcid } } =
          let
            (tname, _, _) = gcheaders ! gcid
          in do
            innerty' <- updateEntry tname
            return LLVM.PointerType { LLVM.pointerReferent = innerty',
                                      LLVM.pointerAddrSpace = 0 }
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
           -> UArray Typename LLVM.Type
           -- ^ An array mapping Typenames to LLVM Type handles
           -> Type
           -- ^ The type for which LLVM is being generated
           -> IO (LLVM.Type, Operand)
           -- ^ The corresponding LLVM type
toLLVMType Module { modGCHeaders = gcheaders } types =
  let
    toLLVMType' :: Type -> LLVMGen (LLVM.Type, Operand)
    toLLVMType' ty @ FuncType { funcTyRetTy = retty, funcTyArgTys = argtys } =
      let
        tydata :: LLVMGen (LLVM.Type, [Operand])
        tydata =
          let
            argdata :: LLVMGen ([LLVM.Type], [Operand])
            argdata =
              do
                res <- mapM toLLVMType' argtys
                return $! unzip res
          in do
            (retty', retmds) <- toLLVMType' retty
            (argtys', argmds) <- argdata
            return (LLVM.FunctionType { LLVM.resultType = retty',
                                        LLVM.argumentTypes = argtys',
                                        LLVM.isVarArg = False },
                    retmds : argmds)
      in do
        (llvmty, tymds) <- tydata
        ent <- getTypeDebugMD ty
        case ent of
          Just mdnode -> return (llvmty, mdnode)
          Nothing ->
            let
              mdcontent retargsmd =
                [Just $! LLVM.Int { LLVM.integerBits = 32,
                                    -- DW_TAG_subroutine_type
                                    LLVM.integerValue = 0xc0015 },
                 Just $! LLVM.Int { LLVM.integerBits = 32,
                                    LLVM.integerValue = 0 },
                 Just $! LLVM.MetadataStringOperand "",
                 Nothing,
                 Just $! LLVM.Int { LLVM.integerBits = 32,
                                    LLVM.integerValue = 0 },
                 Just $! LLVM.Int { LLVM.integerBits = 32,
                                    LLVM.integerValue = 0 },
                 Just $! LLVM.Int { LLVM.integerBits = 32,
                                    LLVM.integerValue = 0 },
                 Just $! LLVM.Int { LLVM.integerBits = 32,
                                    LLVM.integerValue = 0 },
                 Just $! LLVM.Int { LLVM.integerBits = 32,
                                    LLVM.integerValue = 0 },
                 Nothing,
                 retargsmd,
                 Just $! LLVM.Int { LLVM.integerBits = 32,
                                    LLVM.integerValue = 0 },
                 Nothing,
                 Nothing,
                 Nothing]
            in do
              retargsmd <- createMetadataNode tymds
              mdnode <- createTypeDebugMD ty (mdcontent retargs)
              return (llvmty, mdnode)
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
    toLLVMType' ty @ IntType { intSize = 1 } =
      let
        llvmty = LLVM.IntegerType { LLVM.typeBits = 1 }
      in do
        ent <- getTypeDebugMD ty
        case ent of
          Just mdnode -> return (llvmty, mdnode)
          Nothing ->
            let
              mdcontent = [Just $! LLVM.Int { LLVM.integerBits = 32,
                                              -- DW_TAG_base_type
                                              LLVM.integerValue = 0xc0024 },
                           Nothing,
                           Just $! LLVM.MetadataStringOperand "boolean",
                           Nothing,
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 0 },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 1 },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 1 },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 0 },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 0 },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 2 }]
            in do
              mdnode <- createTypeDebugMD ty mdcontent
              return (llvmty, mdnode)
    toLLVMType' ty @ IntType { intSize = size, intSigned = signed } =
      let
        llvmty = LLVM.IntegerType { LLVM.typeBits = size }
      in do
        ent <- getTypeDebugMD ty
        case ent of
          Just mdnode -> return (llvmty, mdnode)
          Nothing ->
            let
              name = case size of
                8 | signed -> "byte"
                  | otherwise -> "unsigned byte"
                16 | signed -> "short"
                   | otherwise -> "unsigned short"
                32 | signed -> "int"
                   | otherwise -> "unsigned int"
                64 | signed -> "long"
                   | otherwise -> "unsigned long"
                _ | signed -> "int" ++ show size
                  | otherwise -> "unsigned int" ++ show size
              format = if signed then 5 else 7
              align | size <= 8 = 8
                    | size <= 16 = 16
                    | size <= 32 = 32
                    | size <= 64 = 64
                    | size <= 128 = 128
              mdcontent = [Just $! LLVM.Int { LLVM.integerBits = 32,
                                              -- DW_TAG_base_type
                                              LLVM.integerValue = 0xc0024 },
                           Nothing,
                           Just $! LLVM.MetadataStringOperand name,
                           Nothing,
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 0 },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = size },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = align },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 0 },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 0 },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = format }]
            in do
              mdnode <- createTypeDebugMD ty mdcontent
              return (llvmty, mdnode)
    toLLVMType' ty @ FloatType { floatSize = size } =
      let
        llvmty = LLVM.FloatingPointType { LLVM.typeBits = size,
                                          LLVM.floatingPointFormat = LLVM.IEEE }
      in do
        ent <- getTypeDebugMD ty
        case ent of
          Just mdnode -> return (llvmty, mdnode)
          Nothing ->
            let
              (name, align) = case size of
                16 -> ("half float", 16)
                32 -> ("float", 32)
                64 -> ("double", 64)
                80 -> ("long double", 128)
                128 -> ("quad", 128)
                _ -> error "Invalid size"
              mdcontent = [Just $! LLVM.Int { LLVM.integerBits = 32,
                                              -- DW_TAG_base_type
                                              LLVM.integerValue = 0xc0024 },
                           Nothing,
                           Just $! LLVM.MetadataStringOperand name,
                           Nothing,
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 0 },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = size },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = align },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 0 },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 0 },
                           Just $! LLVM.Int { LLVM.integerBits = 32,
                                              LLVM.integerValue = 4 }]
            in do
              mdnode <- createTypeDebugMD ty mdcontent
              return (llvmty, mdnode)
    toLLVMType' (UnitType _) = error "Don't generate LLVM for UnitType"
  in
    toLLVMType'

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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}

module Control.Monad.LLVMGen.Metadata.Debug(
       MonadDebugMetadata(..),
       DebugMetadataT,
       DebugMetadata,
       runDebugMetadataT,
       runDebugMetadata
       ) where

import Control.Applicative
import Control.Monad.Artifacts.Class
import Control.Monad.CommentBuffer
import Control.Monad.Comments
import Control.Monad.CompileParams.Class
import Control.Monad.LLVMGen.Metadata.Class
import Control.Monad.LLVMGen.Metadata.Debug.Class
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.Keywords
import Control.Monad.Loader.Class
import Control.Monad.Messages
import Control.Monad.SourceFiles
import Control.Monad.SourceBuffer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Symbols
import Data.Array.Unboxed
import Data.HashTable.IO(BasicHashTable)
import Data.Intervals
import Data.Position.DWARFPosition
import Data.Position.Filename
import LLVM.General.AST
import LLVM.General.AST.Constant

import qualified Data.ByteString.UTF8 as Strict
import qualified Data.HashTable.IO as HashTable
import qualified Data.Position.Point as Point

data Context =
  Context {
    -- | Map from 'FileInfo's to metadatas representing DWARF file objects.
    ctxFileDebugMD :: !(BasicHashTable FileInfo Operand),
    -- | Map from 'FileInfo's to metadata ID's for compilation units.
    -- Note that the actual compilation units are assembled at the
    -- end.
    ctxCompUnitMD :: !(BasicHashTable FileInfo Word),
    -- | Map from position info to metadatas representing DWARF
    -- position objects.
    ctxPosDebugMD :: !(BasicHashTable Point.PointInfo Operand),
    -- | Map from 'Type's to metadatas representing DWARF type descriptors.
    ctxTypeDebugMD :: !(BasicHashTable Type Operand),
    -- | Map from block positions to medatadas representing the blocks.
    ctxBlockMD :: !(BasicHashTable SimplePosition Operand),
    -- | Array mapping 'Globalname's to metadata ID's.
    ctxGlobalIDs :: !(UArray Word Word),
    -- | Array mapping 'Typename's to metadata ID's.
    ctxTypeDefIDs :: !(UArray Word Word)
  }

newtype DebugMetadataT m a =
  DebugMetadataT { unpackDebugMetadataT :: ReaderT Context m a }

type DebugMetadata = DebugMetadataT IO

-- | Generate global metadata IDs.  These are not defined yet.
genGlobalIDs :: (MonadIO m, MonadMetadata m) =>
                Module tagty tagdescty gr ->
                m (UArray Word Word)
genGlobalIDs Module { modGlobals = globals } =
  let
    (lo, hi) = bounds globals
    loword = toEnum lo
    hiword = toEnum hi
    num = hiword - loword
  in do
    mds <- replicateM num getMetadataID
    return array (loword, hiword) mds

-- | Generate typedef metadata IDs.  These are not defined yet.
genTypeDefIDs :: (MonadIO m, MonadMetadata m) =>
                Module tagty tagdescty gr ->
                m (UArray Word Word)
genTypeDefIDs Module { modTypes = types } =
  let
    (lo, hi) = bounds types
    loword = toEnum lo
    hiword = toEnum hi
    num = hiword - loword
  in do
    mds <- replicateM num getMetadataID
    return array (loword, hiword) mds

runDebugMetadataT :: (MonadIO m, MonadMetadata m) =>
                     DebugMetadataT m a
                  -- ^ The @DebugMetadataT@ monad transformer to execute.
                  -> Module tagty tagdescty gr
                  -> m a
runDebugMetadataT c mod =
  do
    fileTab <- liftIO HashTable.new
    compUnitTab <- liftIO HashTable.new
    posTab <- liftIO HashTable.new
    typeTab <- liftIO HashTable.new
    blockTab <- liftIO HashTable.new
    globalArr <- genGlobalIDs mod
    typedefArr <- genTypeDefIDs mod
    runReaderT (unpackDebugMetadataT c)
               Context { ctxFileDebugMD = fileTab, ctxCompUnitMD = compUnitTab,
                         ctxPosDebugMD = posTab, ctxTypeDebugMD = typeTab,
                         ctxBlockMD = blockTab, ctxGlobalMD = globalArr,
                         ctxTypeDefIDs = typedefArr }

runDebugMetadata :: DebugMetadata a
                 -- ^ The @DebugMetadata@ monad to execute.
                 -> IO a
runDebugMetadata = runDebugMetadataT

getCompUnitMD' :: (MonadIO m, MonadMetadata m, MonadCompileParams m) =>
                  FileInfo
               -> ReaderT Context m Operand
getCompUnitMD' finfo @ FileInfo { fileInfoName = fname, fileInfoDir = dir } =
  do
    Context { ctxCompUnitMD = tab, ctxLangCode = langcode } <- ask
    CompileParamVals { cpLangId = langcode, cpMain = ismain,
                       cpOptimize = isopt, cpFlags = flags,
                       cpProducerName = prodname,
                       cpRuntimeVersion = vers } <- compileParams
    res <- liftIO $! HashTable.lookup tab finfo
    case res of
      Just out -> out
      Nothing ->
        let
          createMD =
            let
              mdcontent = [Just $! ConstantOperand $!
                           Int { integerBits = 32,
                                 -- DW_TAG_compile_unit
                                 integerValue = 0xc0011 },
                           Nothing,
                           Just $! ConstantOperand $!
                           Int { integerBits = 32,
                                 integerValue = langcode },
                           Just $! MetadataStringOperand $!
                           Strict.toString fname,
                           Just $! MetadataStringOperand $!
                           Strict.toString dir,
                           Just $! MetadataStringOperand $!
                           Strict.toString prodname,
                           Just $! ConstantOperand $!
                           Int { integerBits = 1,
                                 integerValue = if ismain then 1 else 0 },
                           Just $! ConstantOperand $!
                           Int { integerBits = 1,
                                 integerValue = if isopt then 1 else 0 },
                           Just $! MetadataStringOperand $!
                           Strict.toString flags,
                           Just $! ConstantOperand $!
                           Int { integerBits = 32,
                                 integerValue = vers }]
            in do
              idx <- lift $! createMetadataNode mdcontent
              return $! MetadataNodeOperand $!
                        MetadataNodeReference $!
                        MetadataNodeID idx
        in do
          idx <- createMD
          liftIO $! HashTable.insert tab finfo idx
          return idx

getFileInfoMD' :: (MonadIO m, MonadMetadata m) =>
                  FileInfo
               -> ReaderT Context m Operand
getFileInfoMD' finfo @ FileInfo { fileInfoName = fname, fileInfoDir = dir } =
  do
    Context { ctxFileDebugMD = tab } <- ask
    res <- liftIO $! HashTable.lookup tab finfo
    case res of
      Just out -> return out
      Nothing ->
        let
          createMD =
            let
              mdcontent = [Just $! ConstantOperand $!
                           Int { integerBits = 32,
                                 -- DW_TAG_file_type
                                 integerValue = 0xc0029 },
                           Just $! MetadataStringOperand $!
                           Strict.toString fname,
                           Just $! MetadataStringOperand $!
                           Strict.toString dir,
                           Nothing]

            in do
              idx <- lift $! createMetadataNode mdcontent
              return $! MetadataNodeOperand $!
                        MetadataNodeReference $!
                        MetadataNodeID idx
        in do
          idx <- createMD
          liftIO $! HashTable.insert tab finfo idx
          return idx


getSimplePositionMD :: (MonadIO m, MonadMetadata m, MonadPositions m) =>
                        SimplePosition
                     -> ReaderT Context m Operand
getSimplePositionMD pos =
  let
    startpoint = case pos of
      Span { spanStart = startpoint' } -> startpoint'
      Point { pointPos = startpoint' } -> startpoint'
  in do
    key @ Point.PointInfo { Point.pointLine = line,
                            Point.pointColumn = col,
                            Point.pointFile = fname } <- pointInfo startpoint
    Context { ctxPosDebugMD = tab } <- ask
    res <- liftIO $! HashTable.lookup tab key
    case res of
      Just out -> return out
      Nothing ->
        let
          createMD filemd =
            let
              mdcontent = [Just $! ConstantOperand $! Int 32 (toInteger line),
                           Just $! ConstantOperand $! Int 32 (toInteger col),
                           Just filemd,
                           Nothing]
            in do
              idx <- lift $! createMetadataNode mdcontent
              return $! MetadataNodeOperand $!
                        MetadataNodeReference $!
                        MetadataNodeID idx
        in do
          fileinfo <- fileInfo fname
          filemd <- getFileInfoMD' fileinfo
          idx <- createMD filemd
          liftIO $! HashTable.insert tab key idx
          return idx

startPoint :: DWARFPosition funcid Typename -> Maybe Position.Point
startPoint Func { funcPos = Point { pointPos = point } } = Just point
startPoint Func { funcPos = Span { spanStart = point } } = Just point
startPoint TypeDef { typeDefPos = Point { pointPos = point } } = Just point
startPoint TypeDef { typeDefPos = Span { spanStart = point } } = Just point
startPoint Block { blockPos = Point { pointPos = point } } = Just point
startPoint Block { blockPos = Span { spanStart = point } } = Just point
startPoint Simple { simplePos = Point { pointPos = point } } = Just point
startPoint Simple { simplePos = Span { spanStart = point } } = Just point
startPoint _ = Nothing

getContextMD' :: (MonadIO m, MonadMetadata m, MonadPositions m) =>
                 Module -> DWARFPosition Globalname Typename ->
                 ReaderT Context m (Maybe Operand)
-- For a Def, look up the global's metadata.  It should have already
-- been generated.
getContextMD' _ Def { defId = globalname } =
  do
    Context { ctxGlobalIDs = globalids } <- ask
    return $! MetadataNodeOperand $! MetadataNodeReference $!
              MetadataNodeID $! globalids ! (toEnum globalname)
-- For a TypeDef, look up the typedef medatada ID.  It should have
-- already been generated.
getContextMD' _ TypeDef { typeDefId = tyname } =
  do
    Context { ctxTypeDefIDs = typedefids } <- ask
    return $! MetadataNodeOperand $! MetadataNodeReference $!
              MetadataNodeID $! typedefids ! (toEnum tyname)
-- For a block, we'll need to generate the block ourselves.
getContextMD' mod Block { blockCtx = ctx } =
  case startPoint ctx of
    -- Technically this shouldn't happen, but handle it anyway.
    Nothing -> return Nothing
    -- Get the start point and generate the block info.
    Just startpoint ->
      do
        Point.PointInfo { pointLine = line, pointColumn = col,
                          pointFile = fname } <- pointInfo startpoint
        Context { ctxBlockMD = tab } <- ask
        res <- liftIO $! HashTable.lookup tab pos
        case res of
          -- Create the block metadata if it doesn't exist
          Nothing ->
            let
              createMD ctxmd filemd =
                let
                  mdcontent =
                    [Just $! ConstantOperand $! Int { integerBits = 32,
                                                      -- DW_TAG_lexical_block
                                                      integerValue = 0xc000b },
                     filemd,
                     ctxmd,
                     Just $! ConstantOperand $! Int { integerBits = 32,
                                                      integerValue = line },
                     Just $! ConstantOperand $! Int { integerBits = 32,
                                                      integerValue = col },
                     Nothing]
                in do
                  idx <- lift $! createMetadataNode mdcontent
                  return $! MetadataNodeOperand $!
                            MetadataNodeReference $!
                            MetadataNodeID idx
            in do
              ctxmd <- getContextMD' mod ctx
              fileinfo <- fileInfo fname
              filemd <- getFileInfoMD' fileinfo
              md <- createMD ctxmd
              liftIO $! HashTable.insert tab key idx
              return md
          -- Otherwise just return it
          out -> return out
-- The context for a simple position is its compile unit descriptor.
getContextMD' mod Simple { simplePos = pos } =
  let
    startpoint = case ctx of
      Point { pointPos = point } -> point
      Span { spanStart = point } -> point
  in do
    Point.PointInfo { pointFile = fname } <- pointInfo startpoint
    getCompUnitMD' mod fname
-- Other positions have no context.
getPositionMD' _ Synthetic {} = return Nothing
getPositionMD' _ File {} = return Nothing
getPositionMD' _ CmdLine = return Nothing

-- XXX Use the typedef ID array here
createTypeDefMD :: (MonadIO m, MonadMetadata m, MonadPositions m) =>
                   FileInfo -> Integer -> String -> Maybe Operand ->
                   ReaderT Context m Operand
createTypeDefMD FileInfo { fileInfoName = fname, fileInfoDir = dir }
                line str tymd =
  let
    mdcontent =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_typedef
                                        integerValue = 0xc0016 },
       Just $! MetadataStringOperand (Strict.toString fname),
       Just $! MetadataStringOperand (Strict.toString dir),
       Just $! MetadataStringOperand str,
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = line },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       tymd]
  in do
    idx <- lift $! createMetadataNode $! mdcontent
    return $! MetadataNodeOperand $! MetadataNodeReference $! MetadataNodeID idx

genTypeDefMD' :: (MonadIO m, MonadMetadata m, MonadPositions m) =>
                 Typename
              -> TypeDef
              -> ReaderT Context m ()
genTypeDefMD' tyname Anon { anonTy = ty, anonPos = pos } =
  let
    typedefty = Id { idType = tyname, idPos = pos }
  in do
    Context { ctxTypeDebugMD = tab } <- ask
    typemd <- getTypeMD' ty
    typedefmd <- createTypeDefMD _ "" (Just ty)
    liftIO $! HashTable.insert tab typedefty typedefmd
genTypeDefMD' tyname Name { nameStr = str, namePos = pos } =
  let
    typedefty = Id { idType = tyname, idPos = pos }
  in do
    Context { ctxTypeDebugMD = tab } <- ask
    typedefmd <- createTypeDefMD _ (Strict.toString str) Nothing
    liftIO $! HashTable.insert tab typedefty typedefmd
genTypeDefMD' tyname TypeDef { typeDefStr = str, typeDefTy = ty,
                               typeDefPos = pos } =
  let
    typedefty = Id { idType = tyname, idPos = pos }
  in do
    Context { ctxTypeDebugMD = tab } <- ask
    typemd <- getTypeMD' ty
    typedefmd <- createTypeDefMD _ (Strict.toString str) (Just ty)
    liftIO $! HashTable.insert tab typedefty typedefmd

createTypeDefMD :: (MonadIO m, MonadMetadata m, MonadPositions m,
                    MonadCompileParams m) =>
                   [Maybe Operand] ->
                   ReaderT Context m (Maybe Operand)
createTypeDefMD mdcontent =
  do
    idx <- lift $! createMetadataNode $! mdcontent
    return $! MetadataNodeOperand $! MetadataNodeReference $! MetadataNodeID idx

getTypeMD' :: (MonadIO m, MonadMetadata m, MonadPositions m,
               MonadCompileParams m) =>
           -> Type tagty
           -> ReaderT Context m (Maybe Operand)
getTypeMD' ty =
  do
    (md, _, _) <- createTypeMD
    return $! Just md

createTypeMD :: (MonadIO m, MonadMetadata m, MonadPositions m,
                 MonadCompileParams m) =>
             -> Type tagty
             -> ReaderT Context m (Operand, Integer, Integer)
createTypeMD ty @ FuncType { funcTyRetTy = retty, funcTyArgTys = argtys,
                             funcTyPos = pos }
  let
    mdcontent size elems =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_subroutine_type
                                        integerValue = 0xc0015 },
       Nothing,
       Nothing,
       Just $! MetadataStringOperand "",
       Nothing,
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = toInteger size },
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = toInteger size },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Nothing,
       Just elems,
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Nothing,
       Nothing]
  in do
    Context { ctxTypeDebugMD = tab } <- ask
    CompileParamVals { cpPtrSize = ptrsize } <- compileParams
    retmd <- getTypeMD' retmd
    argsmd <- mapM getTypeMD' argtys
    typemd <- lift $! createMetadataNode (mdcontent ptrsize (retmd : argsmd))
    liftIO $! HashTable.insert tab ty typemd
    return (MetadataNodeOperand $! MetadataNodeReference $!
            MetadataNodeID typemd, 0, 0)
createTypeMD ty @ StructType { structFields = fields } =
  let
    createFieldMD :: (MonadIO m, MonadMetadata m, MonadPositions m,
                      MonadCompileParams m) =>
                     Operand -> Operand -> Operand ->
                     ([Operand], Word) -> Field ->
                     ReaderT Context m ([Operand], Word)
    createFieldMD structmd filemd ctxmd (fields, offset)
                  Field { fieldName = bstr, fieldMutability = _,
                          fieldTy = ty, fieldPos = pos } =
      let
        fieldmd linenum size align =
          [Just $! ConstantOperand $! Int { integerBits = 32,
                                            -- DW_TAG_member
                                            integerValue = 0xc000d },
           filemd,
           ctxmd,
           Just $! MetadataStringOperand (toString bstr),
           Just $! ConstantOperand $! Int { integerBits = 32,
                                            integerValue = toInteger linenum },
           Just $! ConstantOperand $! Int { integerBits = 64,
                                            integerValue = toInteger size },
           Just $! ConstantOperand $! Int { integerBits = 64,
                                            integerValue = toInteger align },
           Just $! ConstantOperand $! Int { integerBits = 64,
                                            integerValue = toInteger offset },
           Just $! ConstantOperand $! Int { integerBits = 64,
                                            integerValue = 0 },
           structmd]
      in do
        (innerty, innersize, inneralign) <- createTypeMD inner

    mdcontent size align fields =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_struct_type
                                        integerValue = 0xc0013 },
       Nothing,
       Nothing,
       Just $! MetadataStringOperand "",
       Nothing,
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = size },
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = align },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Nothing,
       Just fields,
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Nothing,
       Nothing]
  in do
    structmd <- getMetadataID
    (fieldmds, offset) <- foldM (createField structmd) ([], 0) (reverse fields)
    fields <- lift $! createMetadataNode [Just $! MetadataNodeOperand $!
                                          MetadataNodeReference $!
                                          MetadataNodeID subrange]
createTypeMD ty @ Variant { variantForms = variants, variantPos = pos } =
  let
    -- Create one enumeration value metadata
    createEnumValMD :: (MonadIO m, MonadMetadata m, MonadPositions m,
                        MonadCompileParams m) =>
                       (Variantname, Variant) -> ReaderT Context m Operand
    createEnumValMD (vid, Variant { variantName = bstr }) =
      let
        enumvalmd =
          [Just $! ConstantOperand $! Int { integerBits = 32,
                                            -- DW_TAG_enumerator
                                            integerValue = 0xc0028 },
           Just $! MetadataStringOperand (toString bstr),
           Just $! ConstantOperand $! Int { integerBits = 64,
                                            integerValue = toInteger arrlen }]
      in do
        enumval <- createMetadatNode enumvalmd
        return (enumval : enumvals, variants)

    -- Fold-function to create all enumeration value metadatas and see
    -- if there are any non-unit fields.
    createEnumValMDs :: (MonadIO m, MonadMetadata m, MonadPositions m,
                         MonadCompileParams m) =>
                        ([Operand], Bool) ->
                        (Variantname, Variant) ->
                        ReaderT Context m ([Operand], Bool)
    createEnumValMDs (enumvalmds, allunits)
                     ent @ (_, Variant { variantTy = UnitType {} }) =
      do
        enumvalmd <- createEnumValMD ent
        return (enumvalmd : enumvalmds, allunits)
    createEnumValMDs (enumvalmds, _)
                     ent @ (_, Variant { variantTy = basety }) =
      do
        enumvalmd <- createEnumValMD ent
        return (enumvalmd : enumvalmds, False)

    -- Create the enumeration type metadata
    enummd enumvals =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_enum_type
                                        -- XXX get the right value
                                        integerValue = 0xc0004 },
       Nothing,
       Nothing,
       Just $! MetadataStringOperand "",
       Nothing,
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = size },
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = align },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Nothing,
       Just $! MetadataNodeOperand $! MetadataNodeReference $!
               MetadataNodeID enumvals,
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Nothing,
       Nothing]

    (_, nvariants) = bounds variants
    nbits | nvariants < 0x100 = 8
          | nvariants < 0x10000 = 16
          | nvariants < 0x10000000 = 32
          | otherwise = 64
    kindinterval = fromIntervals [Interval 0 (nvariants - 1)]
    kindty = IntType { intSigned = False, intSize = nbits,
                       intIntervals = kindinterval, intPos = pos }
    varbounds = (fromEnum 0, fromEnum 1)

    -- Fold-function to create all enumeration value metadatas and see
    -- if there are any non-unit fields.
    createEnumFieldMD :: (MonadIO m, MonadMetadata m, MonadPositions m,
                          MonadCompileParams m) =>
                         Operand ->
                         (Operand, Word, Word) ->
                         Variant ->
                         ReaderT Context m (Operand, Word, Word)
    createEnumFieldMD kindtymd (fieldmds, maxsize, maxalign)
                      Variant { variantTy = UnitType {} } =
      return (kindtymd : fieldmds, max maxsize nbits, max maxalign nbits)
    createEnumValMDs _ (fieldmds, maxsize, maxalign)
                     Variant { variantTy = ty, variantPos = pos }
      let
        fieldty = StructType { structFields = array (fromEnum 0, fromEnum 1)
                                                    [kindty, ty] }
                               structPacked = False, structPos = pos }
      in do
        (fieldtymd, fieldsize, fieldalign) <- createTypeMD fieldty
        return (fieldtymd : fieldtymd, max maxsize fieldsize,
                max maxalign fieldalign)

    mdcontent size align fields =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_union_type
                                        integerValue = 0xc0017 },
       Nothing,
       Nothing,
       Just $! MetadataStringOperand "",
       Nothing,
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = size },
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = align },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Nothing,
       Just fields,
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Nothing,
       Nothing]
  in do
    (enumvals, allunits) <- foldM createEnumValMDs ([], True) (assocs variants)
    enumtymd <- enummd enumvals
    if allunits
      -- If the variants are all unit types, don't create structures.
      -- Just return the enum.
      then return (enummd, nbits, nbits)
      -- Otherwise, return the structures.
      else do
        (fieldtymds, size, align) <- mapM createEnumFieldMD (elems variants)
        typemd <- lift $! createMetadataNode (mdcontent size align fieldtymds)
        return (typemd, size, align)
createTypeMD ty @ ArrayType { arrayLen = Just arrlen, arrayElemTy = inner,
                              arrayPos = pos }
  let
    subrange =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_subrange_type
                                        integerValue = 0xc0021 },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = toInteger arrlen }]

    mdcontent innerty innersize inneralign elems =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_array_type
                                        integerValue = 0xc0001 },
       Nothing,
       Nothing,
       Just $! MetadataStringOperand "",
       Nothing,
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = innersize },
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = inneralign },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Just innerty,
       Just elems,
       Nothing,
       Nothing,
       Nothing]
  in do
    Context { ctxTypeDebugMD = tab } <- ask
    (innerty, innersize, inneralign) <- createTypeMD inner
    subrange <- lift $! createMetadatNode subrange
    elems <- lift $! createMetadataNode [Just $! MetadataNodeOperand $!
                                         MetadataNodeReference $!
                                         MetadataNodeID subrange]
    typemd <- lift $! createMetadataNode (mdcontent innerty innersize
                                                    inneralign elems)
    liftIO $! HashTable.insert tab ty typemd
    return (MetadataNodeOperand $! MetadataNodeReference $!
            MetadataNodeID typemd, 0, 0)
createTypeMD ty @ ArrayType { arrayLen = Nothing, arrayElemTy = inner,
                              arrayPos = pos }
  let
    mdcontent innerty =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_array_type
                                        integerValue = 0xc0001 },
       Nothing,
       Nothing,
       Just $! MetadataStringOperand "",
       Nothing,
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       innerty,
       Nothing,
       Nothing,
       Nothing,
       Nothing]
  in do
    Context { ctxTypeDebugMD = tab } <- ask
    innerty <- getTypeMD' inner
    typemd <- lift $! createMetadataNode (mdcontent innerty)
    liftIO $! HashTable.insert tab ty typemd
    return (MetadataNodeOperand $! MetadataNodeReference $!
            MetadataNodeID typemd, 0, 0)
createTypeMD ty @ PtrType { ptrTy = ptrty, ptrPos = pos } =
  let
    inner = case ptrty of
      Native { nativeTy = inner' } -> inner'
      Tagged { taggedTag = tagid } -> IdType { idType = _, idPos = pos }

    mdcontent size innerty =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_pointer_type
                                        integerValue = 0xc0011 },
       Nothing,
       Nothing,
       Just $! MetadataStringOperand "",
       Nothing,
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = toInteger size },
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = toInteger size },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       innerty]
  in do
    Context { ctxTypeDebugMD = tab } <- ask
    CompileParamVals { cpPtrSize = ptrsize } <- compileParams
    innerty <- getTypeMD' inner
    typemd <- lift $! createMetadatNode (mdcontent ptrsize innerty)
    liftIO $! HashTable.insert tab ty typemd
    return (MetadataNodeOperand $! MetadataNodeReference $!
            MetadataNodeID typemd, ptrsize, ptrsize)
createTypeMD ty @ IntType { intSigned = True, intSize = size } =
  let
    namestr = case size of
      16 -> "unsigned short"
      32 -> "unsigned int"
      64 -> "unsigned long"
      _ -> "u" ++ show size

    mdcontent =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_base_type
                                        integerValue = 0xc0024 },
       Nothing,
       Nothing,
       Just $! MetadataStringOperand namestr,
       Nothing,
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = toInteger size },
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = toInteger size },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 7 }]
  in do
    Context { ctxTypeDebugMD = tab } <- ask
    typemd <- lift $! createMetadataNode mdcontent
    liftIO $! HashTable.insert tab ty typemd
    return (MetadataNodeOperand $! MetadataNodeReference $!
            MetadataNodeID typemd, toInteger size, toInteger size)
createTypeMD ty @ IntType { intSigned = True, intSize = size } =
  let
    namestr = case size of
      16 -> "short"
      32 -> "int"
      64 -> "long"
      _ -> "i" ++ show size

    mdcontent =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_base_type
                                        integerValue = 0xc0024 },
       Nothing,
       Nothing,
       Just $! MetadataStringOperand namestr,
       Nothing,
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = toInteger size },
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = toInteger size },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 5 }]
  in do
    Context { ctxTypeDebugMD = tab } <- ask
    typemd <- lift $! createMetadataNode mdcontent
    liftIO $! HashTable.insert tab ty typemd
    return (MetadataNodeOperand $! MetadataNodeReference $!
            MetadataNodeID typemd, toInteger size, toInteger size)
createTypeMD ty @ FloatType { floatSize = size } =
  let
    namestr = case size of
      16 -> "half"
      32 -> "float"
      64 -> "double"
      80 -> "long double"
      128 -> "quad"
      _ -> "f" ++ show size

    mdcontent =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_base_type
                                        integerValue = 0xc0024 },
       Nothing,
       Nothing,
       Just $! MetadataStringOperand namestr,
       Nothing,
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = toInteger size },
       Just $! ConstantOperand $! Int { integerBits = 64,
                                        integerValue = toInteger size },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 4 }]
  in do
    Context { ctxTypeDebugMD = tab } <- ask
    typemd <- lift $! createMetadataNode mdcontent
    liftIO $! HashTable.insert tab ty typemd
    return (MetadataNodeOperand $! MetadataNodeReference $!
            MetadataNodeID typemd, toInteger size, toInteger size)
createTypeMD ty @ IdType { idName = tyname } =
  do
    Context { ctxTypeDefIDs = typedefids } <- ask
    return (MetadataNodeOperand $! MetadataNodeReference $!
            MetadataNodeID $! typedefids ! tyname,
            _, _)
createTypeMD ty @ UnitType {} =
  let
    mdcontent =
      [Just $! ConstantOperand $! Int { integerBits = 32,
                                        -- DW_TAG_structure_type
                                        integerValue = 0xc0013 },
       Nothing,
       Nothing,
       Just $! MetadataStringOperand "unit",
       Nothing,
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 64, integerValue = 0 },
       Just $! ConstantOperand $! Int { integerBits = 32, integerValue = 0 },
       Nothing,
       Nothing,
       Nothing,
       Nothing,
       Nothing]
  in do
    Context { ctxTypeDebugMD = tab } <- ask
    typemd <- lift $! createTypeDefMD mdcontent
    liftIO $! HashTable.insert tab ty typemd
    return (MetadataNodeOperand $! MetadataNodeReference $!
            MetadataNodeID typemd, 0, 0)

instance Monad m => Applicative (DebugMetadataT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) => Alternative (DebugMetadataT m) where
  empty = lift empty
  s1 <|> s2 = DebugMetadataT (unpackDebugMetadataT s1 <|>
                              unpackDebugMetadataT s2)

instance Functor (DebugMetadataT m) where
  fmap = fmap

instance Monad m => Monad (DebugMetadataT m) where
  return = DebugMetadataT . return
  s >>= f = DebugMetadataT $ unpackDebugMetadataT s >>= unpackDebugMetadataT . f

instance MonadMetadata m => MonadMetadata (DebugMetadataT m) where
  getMetadataID = lift getMetadataID
  addMetadataNode md = lift . addMetadataNode md
  createMetadataNode = lift . createMetadataNode

instance (MonadIO m, MonadMetadata m) =>
         MonadDebugMetadata (DebugMetadataT m) where
  getFileInfoMD = DebugMetadataT . getFileInfoMD'
  getPositionMD = DebugMetadataT . getPositionMD'

instance MonadIO m => MonadIO (DebugMetadataT m) where
  liftIO = DebugMetadataT . liftIO

instance MonadTrans DebugMetadataT where
  lift = DebugMetadataT . lift

instance MonadArtifacts path m => MonadArtifacts path (DebugMetadataT m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCommentBuffer m => MonadCommentBuffer (DebugMetadataT m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (DebugMetadataT m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (DebugMetadataT m) where
  callCC f =
    DebugMetadataT (callCC (\c -> unpackDebugMetadataT
                                    (f (DebugMetadataT . c))))

instance (MonadError e m) => MonadError e (DebugMetadataT m) where
  throwError = lift . throwError
  m `catchError` h =
    DebugMetadataT (unpackDebugMetadataT m `catchError`
                      (unpackDebugMetadataT . h))

instance MonadGenpos m => MonadGenpos (DebugMetadataT m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (DebugMetadataT m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadKeywords p t m => MonadKeywords p t (DebugMetadataT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadMessages msg m => MonadMessages msg (DebugMetadataT m) where
  message = lift . message

instance MonadLoader path info m =>
         MonadLoader path info (DebugMetadataT m) where
  load = lift . load

instance MonadPositions m => MonadPositions (DebugMetadataT m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadSourceFiles m => MonadSourceFiles (DebugMetadataT m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m => MonadSourceBuffer (DebugMetadataT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (DebugMetadataT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (DebugMetadataT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadPlus m => MonadPlus (DebugMetadataT m) where
  mzero = lift mzero
  mplus s1 s2 = DebugMetadataT (mplus (unpackDebugMetadataT s1)
                                   (unpackDebugMetadataT s2))

instance MonadFix m => MonadFix (DebugMetadataT m) where
  mfix f = DebugMetadataT (mfix (unpackDebugMetadataT . f))

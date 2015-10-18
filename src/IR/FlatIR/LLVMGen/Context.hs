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
{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

module IR.FlatIR.LLVMGen.Context(
       LLVMGen,
       Context(..),
       getMetadataID,
       addMetadataNode,
       getFileInfoMD,
       getCompUnitMD,
       getPositionMD,
       getTypeDebugMD,
       createTypeDebugMD
       ) where

import Control.Monad.State
import Data.HashTable.IO(BasicHashTable)
import Data.Intervals
import Data.Word
import IR.FlatIR.Syntax
import LLVM.General.AST hiding (Type)
import LLVM.General.AST.Constant

import qualified Data.ByteString.UTF8 as Strict
import qualified Data.HashTable.IO as HashTable

data Context =
  Context {
    ctxLanguageID :: !Word,
    ctxMainModule :: !FileInfo,
    ctxProducer :: !Strict.ByteString,
    ctxFlags :: !Strict.ByteString,
    -- | Current Metadata ID
    ctxMetadataID :: !Word,
    -- | All metadata nodes.
    ctxMetadataNodes :: ![(Word, [Maybe Operand])],
    -- | Map from FileInfos to metadatas representing DWARF file objects.
    ctxFileDebugMD :: !(BasicHashTable FileInfo Operand),
    -- | Map from FileInfos to metadata ID's for compilation units.
    -- Note that the actual compilation units are assembled at the
    -- end.
    ctxCompUnitMD :: !(BasicHashTable FileInfo Word),
    -- | Map from position info to metadatas representing DWARF position objects.
    ctxPosDebugMD :: !(BasicHashTable (FileInfo, Word, Word) Operand),
    -- | Map from Type's to metadatas representing DWARF type descriptors
    ctxTypeDebugMD :: !(BasicHashTable Type Operand),
    -- | Map from Intervals and sizes to metadatas representing LLVM range info.
    ctxRangeMD :: !(BasicHashTable (Word, Intervals) Operand)
  }

type LLVMGen = StateT Context IO

getMetadataID :: LLVMGen Word
getMetadataID =
  do
    ctx @ Context { ctxMetadataID = idx } <- get
    put ctx { ctxMetadataID = idx + 1 }
    return idx

addMetadataNode :: Word
                -- ^ Name of the metadata being defined.
                -> [Maybe Operand]
                -- ^ Contents of the metadata.
                -> LLVMGen ()
addMetadataNode idx contents =
  do
    Context { ctxMetadataNodes = nodes } <- get
    liftIO $! HashTable.insert nodes idx contents

createMetadataNode :: [Maybe Operand]
                   -- ^ Contents of the metadata.
                   -> LLVMGen Word
createMetadataNode contents =
  do
    idx <- getMetadataID
    addMetadataNode idx contents
    return idx

getTypeDebugMD :: Type
               -> LLVMGen (Maybe Operand)
getTypeDebugMD ty =
  do
    Context { ctxTypeDebugMD = tab } <- get
    liftIO $! HashTable.lookup tab ty

createTypeDebugMD :: Type
                  -> [Maybe Operand]
                  -> LLVMGen Operand
createTypeDebugMD ty mdcontent =
  do
    Context { ctxTypeDebugMD = tab } <- get
    idx <- createMetadataNode mdcontent
    liftIO $! HashTable.insert tab ty (MetadataNodeID idx)
    return $! MetadataNodeID idx

getFileInfoMD :: FileInfo
              -> LLVMGen Operand
getFileInfoMD finfo @ FileInfo { fileInfoName = fname, fileInfoDir = dir } =
  do
    ctx @ Context { ctxFileDebugMD = tab } <- get
    res <- liftIO $! HashTable.lookup tab finfo
    case res of
      Just out -> return out
      Nothing ->
        let
          mdcontent = [Just $! Int { integerBits = 32,
                                     integerValue = 0xc0029 }, -- DW_TAG_file_type
                       Just $! MetadataStringOperand $! Strict.toString fname,
                       Just $! MetadataStringOperand $! Strict.toString dir,
                       Nothing]
        in do
          idx <- createMetadataNode mdcontent
          liftIO $! HashTable.insert tab finfo (MetadataNodeID idx)
          return $! MetadataNodeID idx

getCompUnitMD :: FileInfo
              -> LLVMGen Operand
getCompUnitMD finfo @ FileInfo { fileInfoName = fname, fileInfoDir = dir } =
  do
    ctx @ Context { ctxCompUnitMD = tab, ctxLanguageID = langid,
                    ctxProducer = producer, ctxFlags = flags } <- get
    res <- liftIO $! HashTable.lookup tab finfo
    case res of
      Just out -> return out
      Nothing ->
        do
          enumMD <- getMetadataID
          typesMD <- getMetadataID
          funcsMD <- getMetadataID
          globalsMD <- getMetadataID
          idx <- createMetadataNode
                   [Just $! Int { integerBits = 32,
                                  integerValue = 0xc0011 }, -- DW_TAG_file_type
                    Just $! Int { integerBits = 32,
                                  integerValue = 0 },
                    Just $! Int { integerBits = 32,
                                  integerValue = langid },
                    Just $! MetadataStringOperand $! Strict.toString fname,
                    Just $! MetadataStringOperand $! Strict.toString dir,
                    Just $! MetadataStringOperand $! Strict.toString producer,
                    Just $! Int { integerBits = 32,
                                  integerValue = 0 },
                    Just $! Int { integerBits = 32,
                                  integerValue = 1 },
                    Just $! MetadataStringOperand $! Strict.toString flags,
                    Just $! Int { integerBits = 32,
                                  integerValue = 0 },
                    Just $! MetadataNodeID enumMD,
                    Just $! MetadataNodeID typesMD,
                    Just $! MetadataNodeID funcsMD,
                    Just $! MetadataNodeID globalsMD]
          liftIO $! HashTable.insert tab finfo idx
          -- XXX Add metadatas to some kind of table
          return $! MetadataNodeID idx

getPositionMD :: PositionInfo
              -> LLVMGen (Maybe Operand)
getPositionMD pos =
  let
    key = case pos of
      Span { spanFile = fileinfo, spanStartLine = line,
             spanStartColumn = col } ->
        Just (fileinfo, line, col)
      Point { pointFile = fileinfo, pointLine = line, pointColumn = col } ->
        Just (fileinfo, line, col)
      _ -> Nothing
  in case key of
    Nothing -> return Nothing
    Just (fileinfo, line, col) ->
      do
        ctx @ Context { ctxPosDebugMD = tab } <- get
        res <- liftIO $! HashTable.lookup tab key
        case res of
          Just out -> return $! Just out
          Nothing ->
            let
              mdcontent filemd = [Just $! Int 32 line,
                                  Just $! Int 32 col,
                                  Just filemd,
                                  Nothing]
            in do
              filemd <- getFileInfoMD fileinfo
              idx <- createMetadataNode $! mdcontent filemd
              liftIO $! HashTable.insert tab key (MetadataNodeID idx)
              return $! Just $! MetadataNodeID idx

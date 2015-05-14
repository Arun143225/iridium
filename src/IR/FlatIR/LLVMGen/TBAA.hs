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
{-# OPTIONS_GHC -Wall -Werror #-}

module IR.FlatIR.LLVMGen.TBAA(
       TBAA(..),
       buildTBAA
       ) where

import Control.Monad
import Control.Monad.Trans
import Data.HashTable.IO(BasicHashTable)
import Data.Word
import IR.Common.TBAA
import IR.FlatIR.LLVMGen.Context
import IR.FlatIR.Syntax
import LLVM.General.AST hiding (Type)
import LLVM.General.AST.Constant

import qualified Data.ByteString.UTF8 as Strict
import qualified Data.HashTable.IO as HashTable

-- | Construct a mapping table and metadata definitions for TBAA info.
buildTBAA :: TBAA Type
          -- ^ The TBAA info.
          -> LLVMGen (BasicHashTable Type MetadataNodeID)
          -- ^ A mapping table to metadata IDs, definitions, and the
          -- new metadata index.
buildTBAA TBAA { tbaaNodes = nodes } =
  let
    buildTBAANode :: Word -> [(Type, MetadataNodeID)] ->
                     TBAANode Type -> LLVMGen [(Type, MetadataNodeID)]
    buildTBAANode parent accum
                  TBAANode { tbaaName = name, tbaaConst = con,
                             tbaaChildren = children, tbaaType = ty } =
      let
        parentref = MetadataNodeReference (MetadataNodeID parent)
        mdcontent =
          if con
            then [MetadataStringOperand (Strict.toString name),
                  MetadataNodeOperand parentref,
                  ConstantOperand Int { integerBits = 1,
                                        integerValue = 1 }]
            else [MetadataStringOperand (Strict.toString name),
                  MetadataNodeOperand parentref]
      in do
        idx <- getMetadataID
        addMetadataNode idx (map Just mdcontent)
        foldM (buildTBAANode idx) ((ty, MetadataNodeID idx) : accum) children

    tbaaroot = [Just (MetadataStringOperand "TBAA")]
  in do
    idx <- getMetadataID
    addMetadataNode idx tbaaroot
    contents <- foldM (buildTBAANode idx) [] nodes
    liftIO $! HashTable.fromList contents

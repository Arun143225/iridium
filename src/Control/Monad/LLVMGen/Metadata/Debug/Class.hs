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

module Control.Monad.LLVMGen.Metadata.Debug.Class(
       MonadDebugMetadata(..)
       ) where

import Control.Monad.LLVMGen.Metadata.Class
import Data.Position.DWARFPosition
import Data.Position.Filename
import IR.Common.Names
import IR.FlatIR.Syntax
import LLVM.AST hiding (Type)

--import qualified Data.ByteString as Strict

-- | A class of monads which create and maintain LLVM debug metadata.
class MonadMetadata m => MonadDebugMetadata m where
  -- | Get a metadata node for a compilation unit.
  {-
  getCompUnitMD :: FileInfo
                -- ^ The file info for the compilation unit.
                -> Word
                -- ^ The DWARF Language ID code.
                -> Strict.ByteString
                -- ^ The producer string.
                -> Strict.ByteString
                -- ^ The flags passed to the producer.
                -> m Operand
-}
  -- | Create a metadata node for file information.
  getFileInfoMD :: FileInfo
                -- ^ The file info for which to create metadata.
                -> m (Maybe Operand)
  -- | Get a metadata node for a given position.
  getPositionMD :: DWARFPosition Globalname Typename
                -- ^ The position.
                -> m (Maybe Operand)
  -- | Generate a metadata node for a typedef.
  genTypeDefMD :: Typename
               -- ^ The typename associated with this typedef.
               -> TypeDef tagty
               -- ^ The typedef for which to get a metadata node.
               -> m ()

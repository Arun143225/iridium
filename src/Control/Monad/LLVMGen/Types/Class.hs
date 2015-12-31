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

module Control.Monad.LLVMGen.Types.Class(
       MonadTypes(..)
       ) where

import Control.Monad.CommentBuffer
import Control.Monad.Comments
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.FileLoader
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.Keywords
import Control.Monad.List
import Control.Monad.MemoryLoader
import Control.Monad.Messages
import Control.Monad.Positions
import Control.Monad.Reader
import Control.Monad.SkipComments
import Control.Monad.SourceFiles
import Control.Monad.State
import Control.Monad.Symbols
import Control.Monad.Trans.Journal
import Control.Monad.Writer
import IR.FlatIR.Syntax

import qualified LLVM.General.AST as LLVM

-- | A Monad class providing access to type mappings.
class Monad m => MonadTypes m where
  -- | Generate the LLVM type for a given FlatIR type.
  toLLVMType :: Type -> m LLVM.Type

instance MonadTypes m => MonadTypes (CommentBufferT m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (CommentsT m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (ContT c m) where
  toLLVMType = lift . toLLVMType

instance (MonadTypes m) => MonadTypes (ExceptT e m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (GenposT m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (GensymT m) where
  toLLVMType = lift . toLLVMType

instance (MonadTypes m, Monoid w) => MonadTypes (JournalT w m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (KeywordsT pos tok m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (ListT m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (MemoryLoaderT info m) where
  toLLVMType = lift . toLLVMType

instance (MonadTypes m, Monoid msgs) => MonadTypes (MessagesT msgs msg m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (PositionsT m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (ReaderT r m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (SkipCommentsT m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (SourceFilesT m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (FileLoaderT m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (StateT s m) where
  toLLVMType = lift . toLLVMType

instance MonadTypes m => MonadTypes (SymbolsT m) where
  toLLVMType = lift . toLLVMType

instance (MonadTypes m, Monoid w) => MonadTypes (WriterT w m) where
  toLLVMType = lift . toLLVMType

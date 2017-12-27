-- Copyright (c) 2016 Eric McCorkle.  All rights reserved.
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

module Control.Monad.LLVMGen.Globals.Class(
       MonadGlobals(..)
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
import IR.Common.Names
import LLVM.AST.Name

-- | A class of monads which create and maintain LLVM metadata.
class Monad m => MonadGlobals m where
  -- | Get an LLVM 'Operand' representing a given global.
  getGlobalVal :: Globalname -> m Name
  -- | Create a new LLVM global ID.
  createGlobalID :: m Name

instance MonadGlobals m => MonadGlobals (CommentBufferT m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (CommentsT m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (ContT c m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance (MonadGlobals m) => MonadGlobals (ExceptT e m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (GenposT m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (GensymT m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance (MonadGlobals m, Monoid w) => MonadGlobals (JournalT w m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (KeywordsT pos tok m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (ListT m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (MemoryLoaderT info m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance (MonadGlobals m, Monoid msgs) =>
         MonadGlobals (MessagesT msgs msg m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (PositionsT m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (ReaderT r m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (SkipCommentsT m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (SourceFilesT m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (FileLoaderT m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (StateT s m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance MonadGlobals m => MonadGlobals (SymbolsT m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance (MonadGlobals m, Monoid w) => MonadGlobals (WriterT w m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

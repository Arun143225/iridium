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

module Control.Monad.CompileParams.Class(
       MonadCompileParams(..),
       CompileParamVals(..),
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

-- | A datatype representing parameters to the compiler.
data CompileParamVals =
  CompileParamVals {
    -- | The DWARF Language ID.
    cpLangId :: Word,
    -- | The name of the compiler (will be indicated in debugging information).
    cpProducerName :: String,
    -- | Whether or not this module is the main module for the program.
    cpMain :: Bool,
    -- | Whether or not this module is optimized.
    cpOptimize :: Bool,
    -- | The runtime version.
    cpRuntimeVersion :: Word,
    -- | The compiler flags.
    cpFlags :: String
  }

-- | A monad class providing access to the compiler parameters.
class Monad m => MonadCompileParams m where
  -- | Get the compiler parameters.
  compileParams :: m CompileParamVals

instance MonadCompileParams m => MonadCompileParams (CommentBufferT m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (CommentsT m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (ContT c m) where
  compileParams = lift compileParams

instance (MonadCompileParams m) => MonadCompileParams (ExceptT e m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (GenposT m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (GensymT m) where
  compileParams = lift compileParams

instance (MonadCompileParams m, Monoid w) =>
         MonadCompileParams (JournalT w m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (KeywordsT pos tok m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (ListT m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (MemoryLoaderT info m) where
  compileParams = lift compileParams

instance (MonadCompileParams m, Monoid msgs) =>
         MonadCompileParams (MessagesT msgs msg m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (PositionsT m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (ReaderT r m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (SkipCommentsT m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (SourceFilesT m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (FileLoaderT m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (StateT s m) where
  compileParams = lift compileParams

instance MonadCompileParams m => MonadCompileParams (SymbolsT m) where
  compileParams = lift compileParams

instance (MonadCompileParams m, Monoid w) =>
         MonadCompileParams (WriterT w m) where
  compileParams = lift compileParams

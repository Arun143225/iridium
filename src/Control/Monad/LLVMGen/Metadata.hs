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

module Control.Monad.LLVMGen.Metadata(
       MonadMetadata(..),
       MetadataT,
       Metadata,
       runMetadataT,
       runMetadata
       ) where

import Control.Applicative
import Control.Monad.Artifacts.Class
import Control.Monad.CommentBuffer
import Control.Monad.Comments
import Control.Monad.LLVMGen.Metadata.Class
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.Keywords
import Control.Monad.Loader.Class
import Control.Monad.Messages
import Control.Monad.SourceFiles
import Control.Monad.SourceBuffer
import Control.Monad.State
import Control.Monad.Symbols
import LLVM.General.AST hiding (Type)

data Context =
  Context {
    -- | Current Metadata ID
    ctxMetadataID :: !Word,
    -- | All metadata nodes.
    ctxMetadataNodes :: ![(Word, [Maybe Operand])]
    -- | Map from FileInfos to metadatas representing DWARF file objects.
  }

newtype MetadataT m a =
  MetadataT { unpackMetadataT :: StateT Context m a }

type Metadata = MetadataT IO

runMetadataT :: Monad m =>
                MetadataT m a
             -- ^ The @MetadataT@ monad transformer to execute.
             -> m (a, [(Word, [Maybe Operand])])
runMetadataT c =
  do
    (out, Context { ctxMetadataNodes = nodes }) <-
      runStateT (unpackMetadataT c) Context { ctxMetadataNodes = [],
                                              ctxMetadataID = 0 }
    return (out, nodes)

runMetadata :: Metadata a
            -- ^ The @Metadata@ monad to execute.
            -> IO (a, [(Word, [Maybe Operand])])
runMetadata = runMetadataT

getMetadataID' :: Monad m => StateT Context m Word
getMetadataID' =
  do
    ctx @ Context { ctxMetadataID = idx } <- get
    put ctx { ctxMetadataID = idx + 1 }
    return idx

addMetadataNode' :: Monad m => Word -> [Maybe Operand] -> StateT Context m ()
addMetadataNode' idx operands =
  do
    ctx @ Context { ctxMetadataNodes = nodes } <- get
    put ctx { ctxMetadataNodes = (idx, operands) : nodes }

instance Monad m => Monad (MetadataT m) where
  return = MetadataT . return
  s >>= f = MetadataT $ unpackMetadataT s >>= unpackMetadataT . f

instance Monad m => Applicative (MetadataT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) => Alternative (MetadataT m) where
  empty = lift empty
  s1 <|> s2 = MetadataT (unpackMetadataT s1 <|> unpackMetadataT s2)

instance Functor (MetadataT m) where
  fmap = fmap

instance MonadIO m => MonadMetadata (MetadataT m) where
  getMetadataID = MetadataT getMetadataID'
  addMetadataNode idx = MetadataT . addMetadataNode' idx

instance MonadIO m => MonadIO (MetadataT m) where
  liftIO = MetadataT . liftIO

instance MonadTrans MetadataT where
  lift = MetadataT . lift

instance MonadArtifacts path m => MonadArtifacts path (MetadataT m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCommentBuffer m => MonadCommentBuffer (MetadataT m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (MetadataT m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (MetadataT m) where
  callCC f =
    MetadataT (callCC (\c -> unpackMetadataT (f (MetadataT . c))))

instance (MonadError e m) => MonadError e (MetadataT m) where
  throwError = lift . throwError
  m `catchError` h =
    MetadataT (unpackMetadataT m `catchError` (unpackMetadataT . h))

instance MonadGenpos m => MonadGenpos (MetadataT m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (MetadataT m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadKeywords p t m => MonadKeywords p t (MetadataT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadMessages msg m => MonadMessages msg (MetadataT m) where
  message = lift . message

instance MonadLoader path info m => MonadLoader path info (MetadataT m) where
  load = lift . load

instance MonadPositions m => MonadPositions (MetadataT m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadSourceFiles m => MonadSourceFiles (MetadataT m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m => MonadSourceBuffer (MetadataT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (MetadataT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (MetadataT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadPlus m => MonadPlus (MetadataT m) where
  mzero = lift mzero
  mplus s1 s2 = MetadataT (mplus (unpackMetadataT s1)
                                   (unpackMetadataT s2))

instance MonadFix m => MonadFix (MetadataT m) where
  mfix f = MetadataT (mfix (unpackMetadataT . f))

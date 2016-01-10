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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}

module Control.Monad.CompileParams(
       MonadCompileParams(..),
       CompileParamVals(..),
       CompileParamsT,
       CompileParams,
       runCompileParamsT,
       runCompileParams,
       mapCompileParamsT
       ) where

import Control.Applicative
import Control.Monad.Artifacts.Class
import Control.Monad.CommentBuffer
import Control.Monad.Comments
import Control.Monad.CompileParams.Class
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.Journal
import Control.Monad.Keywords
import Control.Monad.Loader.Class
import Control.Monad.LLVMGen.Globals.Class
import Control.Monad.LLVMGen.Metadata.Class
import Control.Monad.Messages
import Control.Monad.SourceFiles
import Control.Monad.SourceBuffer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Symbols
import Control.Monad.Writer

newtype CompileParamsT m a =
  CompileParamsT { unpackCompileParamsT :: ReaderT CompileParamVals m a }

type CompileParams = CompileParamsT IO

runCompileParamsT :: MonadIO m =>
                     CompileParamsT m a
                  -- ^ The @CompileParamsT@ monad transformer to execute.
                  -> CompileParamVals
                  -- ^ Compiler parameters.
                  -> m a
runCompileParamsT c = runReaderT (unpackCompileParamsT c)

runCompileParams :: CompileParams a
                 -- ^ The @CompileParams@ monad to execute.
                 -> CompileParamVals
                 -- ^ Compiler parameters.
                 -> IO a
runCompileParams = runCompileParamsT

mapCompileParamsT :: (Monad m, Monad n) =>
                  (m a -> n b) -> CompileParamsT m a -> CompileParamsT n b
mapCompileParamsT f = CompileParamsT . mapReaderT f . unpackCompileParamsT

instance Monad m => Monad (CompileParamsT m) where
  return = CompileParamsT . return
  s >>= f = CompileParamsT $ unpackCompileParamsT s >>= unpackCompileParamsT . f

instance Monad m => Applicative (CompileParamsT m) where
  pure = return
  (<*>) = ap

instance (Monad m, Alternative m) => Alternative (CompileParamsT m) where
  empty = lift empty
  s1 <|> s2 = CompileParamsT (unpackCompileParamsT s1 <|>
                              unpackCompileParamsT s2)

instance Functor (CompileParamsT m) where
  fmap = fmap

instance MonadIO m => MonadIO (CompileParamsT m) where
  liftIO = CompileParamsT . liftIO

instance MonadTrans CompileParamsT where
  lift = CompileParamsT . lift

instance MonadArtifacts path m => MonadArtifacts path (CompileParamsT m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCommentBuffer m => MonadCommentBuffer (CompileParamsT m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (CompileParamsT m) where
  preceedingComments = lift . preceedingComments

instance Monad m => MonadCompileParams (CompileParamsT m) where
  compileParams = CompileParamsT ask

instance MonadCont m => MonadCont (CompileParamsT m) where
  callCC f =
    CompileParamsT (callCC (\c ->
                             unpackCompileParamsT (f (CompileParamsT . c))))

instance (MonadError e m) => MonadError e (CompileParamsT m) where
  throwError = lift . throwError
  m `catchError` h =
    CompileParamsT (unpackCompileParamsT m `catchError`
                    (unpackCompileParamsT . h))

instance MonadGenpos m => MonadGenpos (CompileParamsT m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (CompileParamsT m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadGlobals m => MonadGlobals (CompileParamsT m) where
  getGlobalVal = lift . getGlobalVal
  createGlobalID = lift createGlobalID

instance (Monoid w, MonadJournal w m) => MonadJournal w (CompileParamsT m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadKeywords p t m => MonadKeywords p t (CompileParamsT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadMessages msg m => MonadMessages msg (CompileParamsT m) where
  message = lift . message

instance MonadMetadata m => MonadMetadata (CompileParamsT m) where
  getMetadataID = lift getMetadataID
  addMetadataNode n = lift . addMetadataNode n
  createMetadataNode = lift . createMetadataNode

instance MonadLoader path info m =>
         MonadLoader path info (CompileParamsT m) where
  load = lift . load

instance MonadPositions m => MonadPositions (CompileParamsT m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadSourceFiles m => MonadSourceFiles (CompileParamsT m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m => MonadSourceBuffer (CompileParamsT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (CompileParamsT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (CompileParamsT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadReader r m => MonadReader r (CompileParamsT m) where
  ask = lift ask
  local f = mapCompileParamsT (local f)

instance MonadWriter w m => MonadWriter w (CompileParamsT m) where
  tell = lift . tell
  listen = mapCompileParamsT listen
  pass = mapCompileParamsT pass

instance MonadPlus m => MonadPlus (CompileParamsT m) where
  mzero = lift mzero
  mplus s1 s2 = CompileParamsT (mplus (unpackCompileParamsT s1)
                                   (unpackCompileParamsT s2))

instance MonadFix m => MonadFix (CompileParamsT m) where
  mfix f = CompileParamsT (mfix (unpackCompileParamsT . f))

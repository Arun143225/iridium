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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}

module Control.Monad.LLVMGen.Globals(
       MonadGlobals(..),
       GlobalsT,
       Globals,
       runGlobalsT,
       runGlobals,
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
import Data.Array
import Data.List
import IR.FlatIR.Syntax

import qualified Data.ByteString.UTF8 as Strict
import qualified LLVM.AST.Name as LLVM

type GlobalData = (Array Globalname LLVM.Name)

newtype GlobalsT m a =
  GlobalsT { unpackGlobalsT :: StateT Word (ReaderT GlobalData m) a }

type Globals = GlobalsT IO

runGlobalsT :: MonadIO m =>
               GlobalsT m a
            -- ^ The @GlobalsT@ monad transformer to execute.
            -> Module Globalname Typename gr
            -- ^ Compiler parameters.
            -> m a
runGlobalsT g Module { modGlobals = globals } =
  let
    mapaccfun num GlobalVar { gvarName = Nothing } =
      (num + 1, LLVM.UnName num)
    mapaccfun num GlobalVar { gvarName = Just DeclNames { linkageName = s } } =
      (num, LLVM.Name (Strict.toString s))
    mapaccfun num Function { funcName = Nothing } =
      (num + 1, LLVM.UnName num)
    mapaccfun num Function { funcName = Just DeclNames { linkageName = s } } =
      (num, LLVM.Name (Strict.toString s))

    (firstid, refs) = mapAccumR mapaccfun 0 (elems globals)
    globaldata = listArray (bounds globals) refs
  in do
    (out, _) <- runReaderT (runStateT (unpackGlobalsT g) firstid) globaldata
    return out


runGlobals :: Globals a
           -- ^ The @Globals@ monad to execute.
           -> Module Globalname Typename gr
           -- ^ Compiler parameters.
           -> IO a
runGlobals = runGlobalsT

getGlobalVal' :: MonadIO m =>
                 Globalname -> StateT Word (ReaderT GlobalData m) LLVM.Name
getGlobalVal' globalid =
  do
    arr <- ask
    return $! arr ! globalid

createGlobalID' :: MonadIO m => StateT Word (ReaderT GlobalData m) LLVM.Name
createGlobalID' =
  do
    newid <- get
    put (newid + 1)
    return $! LLVM.UnName newid

instance Monad m => Monad (GlobalsT m) where
  return = GlobalsT . return
  s >>= f = GlobalsT $ unpackGlobalsT s >>= unpackGlobalsT . f

instance Monad m => Applicative (GlobalsT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) => Alternative (GlobalsT m) where
  empty = lift empty
  s1 <|> s2 = GlobalsT (unpackGlobalsT s1 <|> unpackGlobalsT s2)

instance Functor (GlobalsT m) where
  fmap = fmap

instance MonadIO m => MonadIO (GlobalsT m) where
  liftIO = GlobalsT . liftIO

instance MonadTrans GlobalsT where
  lift = GlobalsT . lift . lift

instance MonadArtifacts path m => MonadArtifacts path (GlobalsT m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCommentBuffer m => MonadCommentBuffer (GlobalsT m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (GlobalsT m) where
  preceedingComments = lift . preceedingComments

instance MonadCompileParams m => MonadCompileParams (GlobalsT m) where
  compileParams = lift compileParams

instance MonadCont m => MonadCont (GlobalsT m) where
  callCC f = GlobalsT (callCC (\c -> unpackGlobalsT (f (GlobalsT . c))))

instance (MonadError e m) => MonadError e (GlobalsT m) where
  throwError = lift . throwError
  m `catchError` h =
    GlobalsT (unpackGlobalsT m `catchError` (unpackGlobalsT . h))

instance MonadGenpos m => MonadGenpos (GlobalsT m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (GlobalsT m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadIO m => MonadGlobals (GlobalsT m) where
  getGlobalVal = GlobalsT . getGlobalVal'
  createGlobalID = GlobalsT createGlobalID'

instance (Monoid w, MonadJournal w m) => MonadJournal w (GlobalsT m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadKeywords p t m => MonadKeywords p t (GlobalsT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadMessages msg m => MonadMessages msg (GlobalsT m) where
  message = lift . message

instance MonadMetadata m => MonadMetadata (GlobalsT m) where
  getMetadataID = lift getMetadataID
  addMetadataNode n = lift . addMetadataNode n
  createMetadataNode = lift . createMetadataNode

instance MonadLoader path info m => MonadLoader path info (GlobalsT m) where
  load = lift . load

instance MonadPositions m => MonadPositions (GlobalsT m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadSourceFiles m => MonadSourceFiles (GlobalsT m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m => MonadSourceBuffer (GlobalsT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (GlobalsT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (GlobalsT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadPlus m => MonadPlus (GlobalsT m) where
  mzero = lift mzero
  mplus s1 s2 = GlobalsT (mplus (unpackGlobalsT s1) (unpackGlobalsT s2))

instance MonadFix m => MonadFix (GlobalsT m) where
  mfix f = GlobalsT (mfix (unpackGlobalsT . f))

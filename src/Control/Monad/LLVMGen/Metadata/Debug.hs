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
import Control.Monad.LLVMGen.Metadata.Class
import Control.Monad.LLVMGen.Metadata.Debug.Class
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.Keywords
import Control.Monad.Loader.Class
import Control.Monad.Messages
import Control.Monad.SourceFiles
import Control.Monad.SourceBuffer
import Control.Monad.Reader
import Control.Monad.Symbols
import Data.HashTable.IO(BasicHashTable)
import Data.Position.DWARFPosition
import Data.Position.Filename
import Data.Word
import LLVM.General.AST hiding (Type)

import qualified Data.HashTable.IO as HashTable

data Context =
  Context {
    -- | Map from FileInfos to metadatas representing DWARF file objects.
    ctxFileDebugMD :: !(BasicHashTable FileInfo Operand),
    -- | Map from FileInfos to metadata ID's for compilation units.
    -- Note that the actual compilation units are assembled at the
    -- end.
    ctxCompUnitMD :: !(BasicHashTable FileInfo Word),
    -- | Map from position info to metadatas representing DWARF
    -- position objects.
    ctxPosDebugMD :: !(BasicHashTable (FileInfo, Word, Word) Operand),
    -- | Map from Type's to metadatas representing DWARF type descriptors
    ctxTypeDebugMD :: !(BasicHashTable Type Operand)
  }

newtype DebugMetadataT m a =
  DebugMetadataT { unpackDebugMetadataT :: ReaderT Context m a }

type DebugMetadata = DebugMetadataT IO

runDebugMetadataT :: Monad m =>
                     DebugMetadataT m a
                  -- ^ The @DebugMetadataT@ monad transformer to execute.
                  -> m a
runDebugMetadataT c =
  do
    fileTab <- HashTable.new
    compUnitTab <- HashTable.new
    posTab <- HashTable.new
    typeTab <- HashTable.new
    out <- runReaderT (unpackDebugMetadataT c)
                      Context { ctxFileDebugMD = fileTab,
                                ctxCompUnitMD = compUnitTab,
                                ctxPosDebugMD = posTab,
                                ctxTypeDebugMD = typeTab }
    return out

runDebugMetadata :: DebugMetadata a
                 -- ^ The @DebugMetadata@ monad to execute.
                 -> IO (a, [(Word, [Maybe Operand])])
runDebugMetadata = runDebugMetadataT

getDebugMetadataID' :: Monad m => StateT Context m Word
getDebugMetadataID' =
  do
    ctx @ Context { ctxDebugMetadataID = idx } <- get
    put ctx { ctxDebugMetadataID = idx + 1 }
    return idx

addDebugMetadataNode' :: Monad m => Word -> [Maybe Operand] -> StateT Context m ()
addDebugMetadataNode' idx operands =
  do
    ctx @ Context { ctxDebugMetadataNodes = nodes } <- get
    put ctx { ctxDebugMetadataNodes = (idx, operands) : nodes }

instance Monad m => Monad (DebugMetadataT m) where
  return = DebugMetadataT . return
  s >>= f = DebugMetadataT $ unpackDebugMetadataT s >>= unpackDebugMetadataT . f

instance Monad m => Applicative (DebugMetadataT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) => Alternative (DebugMetadataT m) where
  empty = lift empty
  s1 <|> s2 = DebugMetadataT (unpackDebugMetadataT s1 <|>
                              unpackDebugMetadataT s2)

instance Functor (DebugMetadataT m) where
  fmap = fmap

instance MonadIO m => MonadDebugMetadata (DebugMetadataT m) where
  getDebugMetadataID = DebugMetadataT getDebugMetadataID'
  addDebugMetadataNode idx = DebugMetadataT . addDebugMetadataNode' idx

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

instance (Error e, MonadError e m) => MonadError e (DebugMetadataT m) where
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

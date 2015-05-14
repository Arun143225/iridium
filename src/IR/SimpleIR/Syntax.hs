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

module IR.SimpleIR.Syntax(
       ) where

import Data.HashMap.Strict(HashMap)

import Data.HashMap.Strict as HashMap

-- | A basic block
data Block elem =
    Block {
      -- | The statements in the basic block
      blockBody :: ![elem],
      -- | The transfer for the basic block
      blockXfer :: !Transfer,
      -- | The position in source from which this arises.
      blockPos :: !Pos
    }

-- | The body of a function
data Body elem gr =
    Body {
      -- | The entry block
      bodyEntry :: !Label,
      -- | The CFG
      bodyCFG :: !(gr (Block elem) ())
    }

data Function elem gr = Function {
    -- | Return type
    funcRetTy :: !Type,
    -- | A map from identifiers for arguments and local variables to
    -- their types.
    funcArgTys :: !HashMap Id Type,
    -- | The function's body, if it has one
    funcBody :: !Maybe (Body elem gr),
    -- | The position in source from which this arises.
    funcPos :: !Pos
  }

-- | A module.  Represents a concept similar to an LLVM module.
data Module elem gr =
    Module {
      -- | Name of the module
      modName :: !String,
      -- | A map from typenames to their proper names and possibly their
      -- definitions
      modTypes :: !(Array Typename (String, Maybe Type)),
      -- | A map from GCHeaders to their definitions
      modGCHeaders :: Array GCHeader (Typename, Mobility, Mutability),
      -- | Generated GC types (this module will generate the signatures)
      modGenGCs :: ![GCHeader],
      -- | The top-level scope for this module.
      modScope :: !(Scope elem gr),
      -- | The position in source from which this arises.  This is here
      -- solely to record filenames in a unified way.
      modPos :: !Pos
    }

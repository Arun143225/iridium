-- Copyright (c) 2012 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
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

-- | This module contains code for generating GC header declarations
module SimpleIR.LLVMGen.GCHeaders(
       genGCHeaders
       ) where

import Data.Array(Array)
import Data.Array.IArray
import Data.Array.Unboxed(UArray)
import Data.Graph.Inductive.Graph
import Data.Traversable
import Prelude hiding (mapM_, mapM, foldr, foldl, sequence)
import SimpleIR

import qualified LLVM.Core as LLVM

mobilityStr Mobile = "mobile"
mobilityStr Immobile = "immobile"

mutabilityStr Immutable = "const"
mutabilityStr WriteOnce = "writeonce"
mutabilityStr Mutable = "mutable"
mutabilityStr (Custom str) = str

-- | Generate an array mapping GCHeaders to llvm globals
genGCHeaders :: Graph gr => Module gr -> LLVM.ModuleRef -> LLVM.ContextRef ->
             UArray Typename LLVM.TypeRef -> IO (Array GCHeader LLVM.ValueRef)
genGCHeaders (Module { modTypes = types, modGCHeaders = gcheaders })
          mod ctx typemap =
  let
    mapfun :: LLVM.TypeRef -> (Typename, Mobility, Mutability) ->
              IO LLVM.ValueRef
    mapfun hdrty (tname, mob, mut) =
      let
        (str, _) = types ! tname
        name = "core.gc.typedesc." ++ str ++ "." ++
          mobilityStr mob ++ "." ++ mutabilityStr mut
      in do
        val <- LLVM.addGlobal mod hdrty name
        LLVM.setGlobalConstant val True
        LLVM.setLinkage val LLVM.LinkerPrivateLinkage
        return val
  in do
    hdrty <- LLVM.structCreateNamed ctx "core.gc.typedesc"
    mapM (mapfun hdrty) gcheaders

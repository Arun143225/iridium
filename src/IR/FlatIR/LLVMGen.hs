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

-- | This module implements compilation of SimpleIR to LLVM.  Roughly
-- speaking, the process goes something like this:
--
-- 1) Convert all named types to LLVM types
-- 2) Generate metadata for generated GC types
-- 3) Generate declarations for all necessary accessors and modifiers
-- for GC types.
-- 4) Generate declarations for all globals, and convert their types.
-- 5) Compute dominance frontiers, and from that, phi-sets for each
-- function.
-- 6) Generate code for all functions.
--
-- What isn't implemented:
--   * Escape analysis to figure out what needs to be alloca'ed
--   * Storing local variables in alloca'ed slots at all
--
-- Notes:
--   * Variants can be treated like any other aggregate.  Anytime we
--     assign a particular variant to a variant typed value, set all
--     the fields for the other variants to undef.
--   * Local variables should probably be annotated with a source.
--     This would allow static link accessed variables to be bound
--     properly.
--   * Functions could take an additional argument list representing
--     static linking.
module IR.FlatIR.LLVMGen(
       toLLVM
       ) where

import Data.Graph.Inductive
import IR.FlatIR.Syntax

import qualified LLVM.Core as LLVM
import qualified IR.FlatIR.LLVMGen.GCAccessors as GCAccessors
import qualified IR.FlatIR.LLVMGen.GCHeaders as GCHeaders
import qualified IR.FlatIR.LLVMGen.Globals as Globals
import qualified IR.FlatIR.LLVMGen.Metadata as Metadata
import qualified IR.FlatIR.LLVMGen.Types as Types

-- | Generate LLVM IR from the FlatIR module.
toLLVM :: Graph gr
       => Module gr
       -- ^ The FlatIR module being translated.
       -> IO LLVM.ModuleRef
       -- ^ The LLVM Module.
toLLVM irmod @ Module { modName = name } =
  let
    genTypeDefs = Types.genTypeDefs irmod
    genGCHeaders = GCHeaders.genGCHeaders irmod
    genAccessors = GCAccessors.genAccessors irmod
    genMetadata = Metadata.genMetadata irmod
    genDecls = Globals.genDecls irmod
    genDefs = Globals.genDefs irmod
  in do
    typedefs <- genTypeDefs irmod
    _ <- genGCHeaders llvmmod ctx typedefs
    genMetadata llvmmod ctx
    decls <- genDecls llvmmod ctx typedefs
    genAccessors llvmmod ctx typedefs
    genDefs ctx decls typedefs
    return llvmmod

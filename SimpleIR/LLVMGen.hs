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
--   * For aggregates-as-values, insert a value in the value map
--     during final code generation describing the aggregate, and
--     mapping its fields to new values.  For phis, treat an
--     assignment to an aggregate as an assignment to all of its
--     fields.  Do this field expansion BEFORE creating the phi-sets
--   * Variants can be treated like any other aggregate.  Anytime we
--     assign a particular variant to a variant typed value, set all
--     the fields for the other variants to undef.
--   * Local variables should probably be annotated with a source.
--     This would allow static link accessed variables to be bound
--     properly.
--   * Functions could take an additional argument list representing
--     static linking.
module SimpleIR.LLVMGen(
       toLLVM
       ) where

import Data.Graph.Inductive
import SimpleIR

import qualified LLVM.Core as LLVM
import qualified SimpleIR.LLVMGen.GCAccessors as GCAccessors
import qualified SimpleIR.LLVMGen.GCHeaders as GCHeaders
import qualified SimpleIR.LLVMGen.Globals as Globals
import qualified SimpleIR.LLVMGen.Metadata as Metadata
import qualified SimpleIR.LLVMGen.Types as Types

-- | Generate LLVM IR from the SimpleIR module.
toLLVM :: Graph gr => Module gr -> IO LLVM.ModuleRef
toLLVM mod @ (Module { modName = name }) =
  let
    genTypeDefs = Types.genTypeDefs mod
    genGCHeaders = GCHeaders.genGCHeaders mod
    genAccessors = GCAccessors.genAccessors mod
    genMetadata = Metadata.genMetadata mod
    genDecls = Globals.genDecls mod
    genDefs = Globals.genDefs mod
  in do
    mod <- LLVM.moduleCreateWithName name
    ctx <- LLVM.getModuleContext mod
    typedefs <- genTypeDefs ctx
    gcheaderdecls <- genGCHeaders mod ctx typedefs
    genMetadata mod ctx
    decls <- genDecls mod ctx typedefs
    genAccessors mod ctx typedefs
    genDefs ctx decls typedefs
    return mod
-- Copyright (c) 2013 Eric McCorkle.
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
-- 02110-1301 USA
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

-- | A datatype representing parameters to the compiler.
data CompileParams =
  CompileParams {
    -- | The DWARF Language ID.
    languageId :: Word,
    -- | The name of the compiler (will be indicated in debugging information).
    producerName :: String,
    -- | Whether or not this module is the main module for the program.
    main :: Bool,
    -- | Whether or not this module is optimized.
    optimize :: Bool,
    -- | The runtime version.
    runtimeVersion :: Word,
    -- | The compiler flags.
    flags :: String,
  }

-- | Generate LLVM IR from the FlatIR module.
toLLVM :: Graph gr
       => Module gr
       -- ^ The FlatIR module being translated.
       -> IO LLVM.ModuleRef
       -- ^ The LLVM Module.
toLLVM irmod @ (Module { modName = name }) =
  let
    genTypeDefs = Types.genTypeDefs irmod
    genGCHeaders = GCHeaders.genGCHeaders irmod
    genAccessors = GCAccessors.genAccessors irmod
    genMetadata = Metadata.genMetadata irmod
    genDecls = Globals.genDecls irmod
    genDefs = Globals.genDefs irmod
  in do
    llvmmod <- LLVM.moduleCreateWithName name
    ctx <- LLVM.getModuleContext llvmmod
    typedefs <- genTypeDefs ctx
    _ <- genGCHeaders llvmmod ctx typedefs
    genMetadata llvmmod ctx
    decls <- genDecls llvmmod ctx typedefs
    genAccessors llvmmod ctx typedefs
    genDefs ctx decls typedefs
    return llvmmod
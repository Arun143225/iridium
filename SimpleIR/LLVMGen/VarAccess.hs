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

-- | This module implements tools for compiling variable reads and
-- writes.
module SimpleIR.LLVMGen.VarAccess(
       -- * Types
       Location(..),
       Index(..),
       Access(..),
       ValMap,

       -- * ValMap functions
       getVarLocation,

       -- * Indexing instruction generators
       genGEP,
       genExtractValue,

       -- * Access generators
       genVarAddr,
       genVarRead,
       genVarWrite
       ) where

import Data.Maybe
import SimpleIR.LLVMGen.LLVMValue

import qualified Data.Map as Map
import qualified LLVM.Core as Core

-- | Locations are stored in ValMaps to indicate how a given variable
-- is represented.
data Location =
  -- | A variable stored in an SSA binding
    BindLoc !LLVM.ValueRef
  -- | A variable stored in a memory location
  | MemLoc !Bool LLVM.ValueRef
  -- | A structure, which refers to other local variables
  | StructLoc (UArray Fieldname Word)

-- | This is a type used to store indexes for constructing
-- getelementptr and extractvalue instructions
data Index =
  -- | A field index.  We keep the field name, so we can index into
  -- structure types and locations.
    FieldInd Fieldname
  -- | A value.  These should only exist when indexing into an array.
  | ValueInd LLVM.ValueRef

-- | Accesses represent a slightly more complex value type.  These are
-- essentiall the dual of Locations, and are paired up with them in
-- genVarWrite to implement writes.
data Access =
  -- | An LLVM value
    DirectAcc !LLVM.ValueRef
  -- | Equivalent to a structure constant.
  | StructAcc (UArray Fieldname Word)

-- | A map from Ids to locations, representing the state of the
-- program.
type ValMap = Map Word Location

-- | Generate a getelementptr instruction from the necessary information
genGEP :: LLVM.BuilderRef -> LLVM.ValueRef -> [Index] -> IO LLVM.ValueRef
genGEP builder val [] = val
genGEP builder val indexes = LLVM.buildGEP builder val (map toValue indexes) ""

-- | Generate an extractvalue instruction from the necessary information
genExtractValue :: LLVM.BuilderRef -> LLVM.ValueRef -> [Index] ->
                   IO LLVM.ValueRef
genExtractValue builder val [] = return val
genExtractValue builder val (FieldInd index : indexes) =
  do
    inner <- getExtractValue indexes val
    val <- LLVM.buildExtractValue builder inner index ""

-- | Lookup a variable in a value map and return its location
getVarLocation :: ValMap -> Id -> Location
getVarLocation valmap (Id ind) =
  fromJust (Map.lookup valmap ind)

-- | Get the address of a variable, as well as whether or not it is 
genVarAddr :: LLVM.BuilderRef -> ValMap -> [Index] -> Id ->
              IO (LLVM.ValueRef, Bool)
genVarAddr valmap builder indexes (Id ind) =
  case getVarLocation valmap of
    MemLoc volatile addr ->
      do
        out <- genGEP builder addr indexes
        return (out, volatile)

-- | Generate an access to the given variable, with the given indexes.
genVarRead :: LLVM.BuilderRef -> ValMap -> [Index] -> Id -> IO Access
genVarRead builder valmap indexes id =
  case getVarLocation valmap id of
    -- Straightforward, it's a value.  Generate an extractelement and
    -- build a direct access.
    BindLoc val ->
      do
        out <- genExtractValue indexes val
        return (DirectAcc out)
    -- For a memory location, generate a GEP, then load, then build a
    -- direct access.
    MemLoc volatile addr ->
      do
        offaddr <- genGEP builder addr (map getOffsetValue offsets)
        val <- LLVM.buildLoad builder val ""
        LLVM.setVolatile val volatile
        return (DirectAcc val)
    -- For structures, we'll either recurse, or else build a structure
    -- access.
    StructLoc fields ->
      case indexes of
        -- If there's indexes, recurse
        (FieldInd ind : indexes) ->
          genVarRead builder indexes (fields ! ind)
        -- Otherwise, build a structure access
        [] ->
          do
            accs <- mapM (genVarRead builder valmap []) fields
            return (StructAcc accs)

-- | Given an access and a variable, do the work to write to it.  This
-- can involve a number of cases.
genVarWrite :: LLVM.BuilderRef -> ValMap -> Access -> [Index] -> Id ->
               IO ValMap
genVarWrite builder valmap acc indexes id =
  case (acc, getVarLocation id, indexes) of
    -- Straightforward, we've got a value and a binding, so update
    -- the map.
    (BindLoc _, []) -> return (Map.insert id (toValue acc) valmap)
    -- This case should never happen.
    (BindLoc _, _) -> error "Extra indexes in assignment to variable"
    -- We've got a value and a memory location.  Generate a GEP and store
    -- the value.
    (MemLoc volatile mem, indexes) ->
      do
        addr <- genGEP builder addr indexes
        store <- LLVM.buildStore builder (toValue acc) addr
        LLVM.setVolatile store volatile
        return valmap
    -- If we actuall have fields, descend one index into the
    -- destination and recurse.
    (StructLoc fields, FieldInd field : rest) ->
      genVarWrite valmap builder acc (fields ! field) rest
    -- Any other kind of index is an error condition.
    (StructLoc _, _ : _) ->
      error "Bad indexes in assignment to variable"
    -- When we run out of fields, we've reached an end case, and we
    -- need to look at the access.
    (StructLoc fields, []) ->
      case acc of
        -- We've got a value (which ought to have a structure type),
        -- and a local variable that's a structure.  Go through and
        -- generate writes into each field.
        DirectAcc val ->
          let
            foldfun valmap (field, id) =
              do
                val' <- LLVM.buildExtractValue builder val field ""
                genVarWrite valmap builder (DirectAcc val') id []
          in
            foldlM foldfun valmap (assocs fields)
        -- We've got a structure access and a structure location,
        -- which should match up.  Pair up the fields and recurse on
        -- each pair individually.
        StructAcc accfields ->
          let
            foldfun 
            fieldlist = zip (elems accfields) (elems fields)
          in
            foldlM foldfun valmap fieldlist

instance LLVMValue Index where
  toValue (FieldInd fname) = toValue fname
  toValue (ValueInd val) = val

instance LLVMValue Access where
  toValue (DirectAcc val) = val
  toValue (StructAcc arr) = LLVM.constStruct (elems arr) False

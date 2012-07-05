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

{-# OPTIONS_GHC -funbox-strict-fields #-}

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
       genWrite
       ) where

import Data.Array(Array)
import Data.Array.IArray
import Data.Array.Unboxed(UArray)
import Data.Foldable
import Data.Map(Map)
import Data.Maybe
import Data.Traversable
import Data.Word
import SimpleIR
import SimpleIR.LLVMGen.LLVMValue

import Prelude hiding (mapM_, mapM, foldr, foldl, sequence)

import qualified Data.Map as Map
import qualified LLVM.Core as LLVM

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
  | StructAcc (Array Fieldname Access)

-- | A map from Ids to locations, representing the state of the
-- program.
type ValMap = Map Word Location

-- | Generate a getelementptr instruction from the necessary information
genGEP :: LLVM.BuilderRef -> LLVM.ValueRef -> [Index] -> IO LLVM.ValueRef
genGEP builder val [] = return val
genGEP builder val indexes = LLVM.buildGEP builder val (map toValue indexes) ""

-- | Generate an extractvalue instruction from the necessary information
genExtractValue :: LLVM.BuilderRef -> Access -> [Index] -> IO Access
genExtractValue builder acc [] = return acc
genExtractValue builder (DirectAcc val) indexes =
  let
    genExtractValue' val (FieldInd (Fieldname fname) : indexes) =
      do
        inner <- genExtractValue' val indexes
        LLVM.buildExtractValue builder inner fname ""
  in do
    out <- genExtractValue' val indexes
    return (DirectAcc out)
genExtractValue builder (StructAcc fields) (FieldInd field : indexes) =
  genExtractValue builder (fields ! field) indexes

-- | Lookup a variable in a value map and return its location
getVarLocation :: ValMap -> Id -> Location
getVarLocation valmap (Id ind) =
  fromJust (Map.lookup ind valmap)

-- | Get the address of a variable, as well as whether or not it is 
genVarAddr :: LLVM.BuilderRef -> ValMap -> [Index] -> Id ->
              IO (LLVM.ValueRef, Bool)
genVarAddr builder valmap indexes id =
  case getVarLocation valmap id of
    MemLoc volatile addr ->
      do
        out <- genGEP builder addr indexes
        return (out, volatile)

-- | Generate an access to the given variable, with the given indexes.
genVarRead :: LLVM.BuilderRef -> ValMap -> [Index] -> Id -> IO Access
genVarRead builder valmap indexes id =
  case getVarLocation valmap id of
    -- Straightforward, it's a value.  Make sure we have no indexes
    -- and return the value.
    BindLoc val ->
      case indexes of
        [] -> return (DirectAcc val)
        _ -> error "Indexes in read of non-aggregate variable"
    -- For a memory location, generate a GEP, then load, then build a
    -- direct access.
    MemLoc volatile mem ->
      do
        addr <- genGEP builder mem indexes
        val <- LLVM.buildLoad builder addr ""
        LLVM.setVolatile val volatile
        return (DirectAcc val)
    -- For structures, we'll either recurse, or else build a structure
    -- access.
    StructLoc fields ->
      case indexes of
        -- If there's indexes, recurse
        (FieldInd ind : indexes) ->
          genVarRead builder valmap indexes (Id (fields ! ind))
        -- Otherwise, build a structure access
        [] ->
          do
            accs <- mapM (genVarRead builder valmap []) (map Id (elems fields))
            return (StructAcc (listArray (bounds fields) accs))

-- | This function handles writes to variables without indexes
genRawVarWrite :: LLVM.BuilderRef -> ValMap -> Access -> Id -> IO ValMap
genRawVarWrite builder valmap acc id @ (Id ind) =
  case getVarLocation valmap id of
    BindLoc _ -> return (Map.insert ind (BindLoc (toValue acc)) valmap)
    loc -> genRawWrite builder valmap acc loc

-- | This function handles writes to non-variables without indexes
genRawWrite :: LLVM.BuilderRef -> ValMap -> Access -> Location -> IO ValMap
-- We've got a value and a memory location.  Generate a store.
genRawWrite builder valmap acc (MemLoc volatile addr) =
  do
    store <- LLVM.buildStore builder (toValue acc) addr
    LLVM.setVolatile store volatile
    return valmap
-- For structures, we end up recursing.
genRawWrite builder valmap acc (StructLoc fields) =
  case acc of
    -- We've got a value (which ought to have a structure type),
    -- and a local variable that's a structure.  Go through and
    -- generate writes into each field.
    DirectAcc val ->
      let
        foldfun valmap (Fieldname fname, id) =
          do
            val' <- LLVM.buildExtractValue builder val fname ""
            genRawVarWrite builder valmap (DirectAcc val') (Id id)
      in do
        foldlM foldfun valmap (assocs fields)
    -- We've got a structure access and a structure location, which
    -- should match up.  Pair up the fields and recurse on each pair
    -- individually.
    StructAcc accfields ->
      let
        foldfun valmap (acc, id) = genRawVarWrite builder valmap acc (Id id)
        fieldlist = zip (elems accfields) (elems fields)
      in
        foldlM foldfun valmap fieldlist
genRawWrite _ _ _ (BindLoc _) = error "genRawWrite can't handle BindLocs"

-- | Take an access, a non-variable location, and a list of indexes, and
-- do the work to write to the location.  This involves many possible
-- cases.
genWrite :: LLVM.BuilderRef -> ValMap -> Access -> [Index] -> Location ->
            IO ValMap
-- This case should never happen
genWrite _ _ _ _ (BindLoc _) = error "genWrite can't handle BindLocs"
-- For no index cases, pass off to genRawWrite
genWrite builder valmap acc [] loc =
  genRawWrite builder valmap acc loc
-- We've got a value and a memory location.  Generate a GEP and store
-- the value.
genWrite builder valmap acc indexes (MemLoc volatile mem) =
  do
    addr <- LLVM.buildGEP builder mem (map toValue indexes) ""
    store <- LLVM.buildStore builder (toValue acc) addr
    LLVM.setVolatile store volatile
    return valmap
-- For structures, we recurse to strip away the fields
genWrite builder valmap acc (FieldInd field : indexes) (StructLoc fields) =
  genVarWrite builder valmap acc indexes (Id (fields ! field))
-- Any other kind of index is an error condition
genWrite _ _ _ _ (StructLoc _) = error "Bad indexes in assignment to variable"

-- | Take an access, a variable name, and a list of indexes and go the
-- work to write to the location.
genVarWrite :: LLVM.BuilderRef -> ValMap -> Access -> [Index] -> Id ->
               IO ValMap
genVarWrite builder valmap acc indexes id =
  case getVarLocation valmap id of
    BindLoc _ ->
      case indexes of
        [] -> genRawVarWrite builder valmap acc id
        _ -> error "Extra indexes in write to variable"
    loc -> genWrite builder valmap acc indexes loc

instance LLVMValue Index where
  toValue (FieldInd fname) = toValue fname
  toValue (ValueInd val) = val

instance LLVMValue Access where
  toValue (DirectAcc val) = val
  toValue (StructAcc arr) = LLVM.constStruct (map toValue (elems arr)) False

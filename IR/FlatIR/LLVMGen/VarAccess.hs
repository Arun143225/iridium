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
{-# OPTIONS_GHC -funbox-strict-fields -Wall #-}

-- | This module implements tools for compiling variable reads and
-- writes.
module IR.FlatIR.LLVMGen.VarAccess(
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

import Data.Array.IArray
import Data.Array.Unboxed(UArray)
import Data.Foldable
import Data.Map(Map)
import Data.Maybe
import Data.Traversable
import Data.Word
import IR.FlatIR.Syntax
import IR.FlatIR.LLVMGen.LLVMValue

import Prelude hiding (mapM_, mapM, foldr, foldl, sequence)

import qualified Data.Map as Map
import qualified LLVM.Core as LLVM

-- | Locations are stored in ValMaps to indicate how a given variable
-- is represented.
data Location =
  -- | A variable stored in an SSA binding
    BindLoc !LLVM.ValueRef
  -- | A variable stored in a memory location
  | MemLoc !Bool !LLVM.ValueRef
  -- | A structure, which refers to other local variables
  | StructLoc !(UArray Fieldname Word)

-- | This is a type used to store indexes for constructing
-- getelementptr and extractvalue instructions
data Index =
  -- | A field index.  We keep the field name, so we can index into
  -- structure types and locations.
    FieldInd !Fieldname
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
genGEP _ val [] = return val
genGEP builder val indexes = LLVM.buildGEP builder val (map toValue indexes) ""

-- | Generate an extractvalue instruction from the necessary information
genExtractValue :: LLVM.BuilderRef -> Access -> [Index] -> IO Access
genExtractValue _ acc [] = return acc
genExtractValue builder (DirectAcc val) indexes =
  let
    genExtractValue' val' (FieldInd (Fieldname fname) : indexes') =
      do
        inner' <- genExtractValue' val' indexes'
        LLVM.buildExtractValue builder inner' fname ""
    genExtractValue' _ (ValueInd _ : _) =
      error "Value index cannot occur in extractvalue"
    genExtractValue' val' [] = return val'
  in do
    out <- genExtractValue' val indexes
    return (DirectAcc out)
genExtractValue builder (StructAcc fields) (FieldInd field : indexes) =
  genExtractValue builder (fields ! field) indexes
genExtractValue _ acc ind =
  error ("Mismatched access " ++ show acc ++ " and index " ++ show ind)

-- | Lookup a variable in a value map and return its location
getVarLocation :: ValMap -> Id -> Location
getVarLocation valmap (Id ind) =
  fromJust (Map.lookup ind valmap)

-- | Get the address of a variable, as well as whether or not it is 
genVarAddr :: LLVM.BuilderRef -> ValMap -> [Index] -> Id ->
              IO (LLVM.ValueRef, Bool)
genVarAddr builder valmap indexes var =
  case getVarLocation valmap var of
    MemLoc volatile addr ->
      do
        out <- genGEP builder addr indexes
        return (out, volatile)
    _ -> error ("Location has no address")

-- | Generate an access to the given variable, with the given indexes.
genVarRead :: LLVM.BuilderRef -> ValMap -> [Index] -> Id -> IO Access
genVarRead builder valmap indexes var =
  case getVarLocation valmap var of
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
        (FieldInd ind : indexes') ->
          genVarRead builder valmap indexes' (Id (fields ! ind))
        -- Otherwise, build a structure access
        [] ->
          do
            accs <- mapM (genVarRead builder valmap []) (map Id (elems fields))
            return (StructAcc (listArray (bounds fields) accs))
        _ -> error "Wrong kind of index for a structure location"

-- | This function handles writes to variables without indexes
genRawVarWrite :: LLVM.BuilderRef -> ValMap -> Access -> Id -> IO ValMap
genRawVarWrite builder valmap acc var @ (Id name) =
  case getVarLocation valmap var of
    BindLoc _ -> return (Map.insert name (BindLoc (toValue acc)) valmap)
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
        foldfun valmap' (Fieldname fname, var) =
          do
            val' <- LLVM.buildExtractValue builder val fname ""
            genRawVarWrite builder valmap' (DirectAcc val') (Id var)
      in do
        foldlM foldfun valmap (assocs fields)
    -- We've got a structure access and a structure location, which
    -- should match up.  Pair up the fields and recurse on each pair
    -- individually.
    StructAcc accfields ->
      let
        foldfun valmap' (acc', var) =
          genRawVarWrite builder valmap' acc' (Id var)
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

-- | Take an access, a variable name, and a list of indexes and do the
-- work to write to the location.
genVarWrite :: LLVM.BuilderRef -> ValMap -> Access -> [Index] -> Id ->
               IO ValMap
genVarWrite builder valmap acc indexes var =
  case getVarLocation valmap var of
    BindLoc _ ->
      case indexes of
        [] -> genRawVarWrite builder valmap acc var
        _ -> error "Extra indexes in write to variable"
    loc -> genWrite builder valmap acc indexes loc

instance LLVMValue Index where
  toValue (FieldInd fname) = toValue fname
  toValue (ValueInd val) = val

instance LLVMValue Access where
  toValue (DirectAcc val) = val
  toValue (StructAcc arr) = LLVM.constStruct (map toValue (elems arr)) False

instance Show Index where
  show (FieldInd fname) = "field " ++ show fname
  show (ValueInd _) = "value"

instance Show Access where
  show (DirectAcc _) = "direct"
  show (StructAcc _) = "struct"
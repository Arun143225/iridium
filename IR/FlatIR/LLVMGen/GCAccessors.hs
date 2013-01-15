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

-- | This module contains code that generates declarations for
-- intrinsic functions that access and modify the fields of a GC
-- object.  This code is later generated (and very likely inlined) in
-- the GC backend.
module IR.FlatIR.LLVMGen.GCAccessors(
       genAccessors
       ) where

import Data.Array.Unboxed
import Data.Foldable
import Data.Graph.Inductive.Graph
import IR.FlatIR.Syntax
import Prelude hiding (mapM_)

import qualified IR.FlatIR.LLVMGen.Types as Types
import qualified IR.GC.Types as GC
import qualified LLVM.Core as LLVM

constant :: Bool -> GC.Mutability -> Bool
constant _ GC.Immutable = True
constant True _ = True
constant _ _ = False

-- | Generate the accessors and modifiers for the Module.  These
-- functions will be declared with only a prototype, with the
-- expectation that the GC implementation portion will define them
-- (and probably inline them)
genAccessors :: Graph gr =>
                Module gr
             -- ^ The FlatIR module being translated
             -> LLVM.ModuleRef
             -- ^ The LLVM Module being created
             -> LLVM.ContextRef
             -- ^ The LLVM Context handle
             -> UArray Typename LLVM.TypeRef
             -- ^ An array mapping Typenames to LLVM Type handles
             -> IO ()
genAccessors m @ (Module { modTypes = types }) llvmmod ctx typedefs =
  let
    toLLVMType = Types.toLLVMType m ctx typedefs

    genTypeAccessors :: (Typename, (String, Maybe Type)) -> IO ()
    genTypeAccessors (typename, (str, Just ty)) =
      let
        tyref = (typedefs ! typename)

        genDecls :: Bool -> Type -> String -> [LLVM.TypeRef] -> IO ()
        genDecls isconst declty name args =
          let
            readtype :: LLVM.TypeRef -> LLVM.TypeRef
            readtype resty = LLVM.functionType resty (reverse args) False

            writetype :: LLVM.TypeRef -> LLVM.TypeRef
            writetype resty =
              LLVM.functionType LLVM.voidType (reverse (resty : args)) False
          in do
            resty <- toLLVMType declty
            readfunc <- LLVM.addFunction llvmmod (name ++ ".read") 
                                         (readtype resty)
            LLVM.addFunctionAttr readfunc LLVM.NoUnwindAttribute
            LLVM.addFunctionAttr readfunc LLVM.ReadOnlyAttribute
            LLVM.addFunctionAttr readfunc LLVM.AlwaysInlineAttribute
            if not isconst
              then do
                writefunc <- LLVM.addFunction llvmmod (name ++ ".write")
                                              (writetype resty)
                LLVM.addFunctionAttr writefunc LLVM.NoUnwindAttribute
                LLVM.addFunctionAttr writefunc LLVM.AlwaysInlineAttribute
                return()
              else return ()

        genTypeAccessors' :: String -> Bool -> [LLVM.TypeRef] ->
                            (String, GC.Mutability, Type) -> IO ()
        genTypeAccessors' prefix isconst args (name, mut, StructType _ fields) =
          do
            mapM_ (genTypeAccessors' (prefix ++ "." ++ name)
                                     (constant isconst mut) args) fields
        genTypeAccessors' prefix isconst args (name, mut, ArrayType _ inner) =
          do
            genTypeAccessors' prefix isconst (LLVM.int32Type : args)
                                     (name, mut, inner)
        genTypeAccessors' prefix isconst args (name, mut, ty') =
          genDecls (constant isconst mut) ty' (prefix ++ "." ++ name) args
      in do
        genTypeAccessors' "core.types" False [tyref] (str, GC.Mutable, ty)
        return ()
    genTypeAccessors _ = return ()
  in
    mapM_ genTypeAccessors (assocs types)

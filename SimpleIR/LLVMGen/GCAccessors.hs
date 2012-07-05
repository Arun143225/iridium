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

-- | This module contains code that generates declarations for
-- intrinsic functions that access and modify the fields of a GC
-- object.  This code is later generated (and very likely inlined) in
-- the GC backend.
module SimpleIR.LLVMGen.GCAccessors(
       genAccessors
       ) where

import Data.Array.Unboxed
import Data.Foldable
import Data.Graph.Inductive.Graph
import Prelude hiding (mapM_)
import SimpleIR

import qualified LLVM.Core as LLVM
import qualified SimpleIR.LLVMGen.Types as Types

constant :: Bool -> Mutability -> Bool
constant _ Immutable = True
constant True _ = True
constant _ _ = False

-- | Generate the accessors and modifiers for the Module
genAccessors :: Graph gr => Module gr -> LLVM.ModuleRef -> LLVM.ContextRef ->
                            UArray Typename LLVM.TypeRef -> IO ()
genAccessors m @ (Module { modTypes = types }) mod ctx typedefs =
  let
    toLLVMType = Types.toLLVMType m ctx typedefs

    genTypeAccessors :: (Typename, (String, Maybe Type)) -> IO ()
    genTypeAccessors (typename, (str, Just ty)) =
      let
        tyref = (typedefs ! typename)

        genDecls :: Bool -> Type -> String -> [LLVM.TypeRef] -> IO ()
        genDecls const ty name args =
          let
            readtype :: LLVM.TypeRef -> LLVM.TypeRef
            readtype resty = LLVM.functionType resty (reverse args) False

            writetype :: LLVM.TypeRef -> LLVM.TypeRef
            writetype resty =
              LLVM.functionType LLVM.voidType (reverse (resty : args)) False
          in do
            resty <- toLLVMType ty
            readfunc <- LLVM.addFunction mod (name ++ ".read") (readtype resty)
            LLVM.addFunctionAttr readfunc LLVM.NoUnwindAttribute
            LLVM.addFunctionAttr readfunc LLVM.ReadOnlyAttribute
            LLVM.addFunctionAttr readfunc LLVM.AlwaysInlineAttribute
            if not const
              then do
                writefunc <- LLVM.addFunction mod (name ++ ".write")
                                                  (writetype resty)
                LLVM.addFunctionAttr writefunc LLVM.NoUnwindAttribute
                LLVM.addFunctionAttr writefunc LLVM.AlwaysInlineAttribute
                return()
              else return ()

        genTypeAccessors' :: String -> Bool -> [LLVM.TypeRef] ->
                            (String, Mutability, Type) -> IO ()
        genTypeAccessors' prefix const args (name, mut, StructType _ fields) =
          do
            mapM_ (genTypeAccessors' (prefix ++ "." ++ name)
                                     (constant const mut) args) fields
        genTypeAccessors' prefix const args (name, mut, ArrayType _ inner) =
          do
            genTypeAccessors' prefix const (LLVM.int32Type : args)
                                     (name, mut, inner)
        genTypeAccessors' prefix const args (name, mut, ty) =
          genDecls (constant const mut) ty (prefix ++ "." ++ name) args
      in do
        genTypeAccessors' "core.types" False [tyref] (str, Mutable, ty)
        return ()
    genTypeAccessors _ = return ()
  in
    mapM_ genTypeAccessors (assocs types)

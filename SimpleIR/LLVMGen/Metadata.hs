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

module SimpleIR.LLVMGen.Metadata(
       genMetadata
       ) where

import Data.Array(Array)
import Data.Array.IArray
import Data.Array.Unboxed(UArray)
import Data.Foldable
import Data.Graph.Inductive.Graph
import Prelude hiding (mapM_)
import SimpleIR

import qualified LLVM.Core as LLVM

mutabilityValue :: Mutability -> IO LLVM.ValueRef
mutabilityValue Immutable = LLVM.mdString "const"
mutabilityValue Mutable = LLVM.mdString "mutable"
mutabilityValue WriteOnce = LLVM.mdString "writeonce"
mutabilityValue (Custom str) = LLVM.mdString str

mobilityValue :: Mobility -> IO LLVM.ValueRef
mobilityValue Mobile = LLVM.mdString "mobile"
mobilityValue Immobile = LLVM.mdString "immobile"

ptrClassValue :: PtrClass -> IO LLVM.ValueRef
ptrClassValue StrongPtr = LLVM.mdString "strong"
ptrClassValue SoftPtr = LLVM.mdString "soft"
ptrClassValue WeakPtr = LLVM.mdString "weak"
ptrClassValue FinalPtr = LLVM.mdString "final"
ptrClassValue PhantomPtr = LLVM.mdString "phantom"

-- Generate the metadata descriptors for all of the generated GC types
genMetadata :: Graph gr => Module gr -> LLVM.ModuleRef -> LLVM.ContextRef ->
               IO ()
genMetadata (Module { modGCHeaders = gcheaders, modGenGCs = gengcs,
                      modTypes = types}) mod ctx =
  let
    genFieldNode :: (String, Mutability, Type) -> IO LLVM.ValueRef
    genFieldNode (str, mut, ty) =
      do
        namemd <- LLVM.mdString str
        mutmd <- mutabilityValue mut
        tymd <- genTypedesc ty
        LLVM.mdNode [ namemd, mutmd, tymd ]

    genTypedesc :: Type -> IO LLVM.ValueRef
    genTypedesc (StructType True fields) =
      do
        classmd <- LLVM.mdString "struct"
        packedmd <- LLVM.mdString "packed"
        fieldnodes <- mapM genFieldNode (elems fields)
        LLVM.mdNode (classmd : packedmd : fieldnodes)
    genTypedesc (StructType False fields) =
      do
        classmd <- LLVM.mdString "struct"
        packedmd <- LLVM.mdString "nonpacked"
        fieldnodes <- mapM genFieldNode (elems fields)
        LLVM.mdNode (classmd : packedmd : fieldnodes)
    genTypedesc (ArrayType (Just size) inner) =
      do
        classmd <- LLVM.mdString "array"
        innernode <- genTypedesc inner
        LLVM.mdNode [ classmd, LLVM.constInt LLVM.int64Type size False,
                      innernode ]
    genTypedesc (ArrayType Nothing inner) =
      do
        classmd <- LLVM.mdString "array"
        innernode <- genTypedesc inner
        LLVM.mdNode [ classmd,
                      LLVM.constInt LLVM.int64Type 0 False, innernode ]
    genTypedesc (PtrType (BasicObj inner)) =
      do
        classmd <- LLVM.mdString "nativeptr"
        innernode <- genTypedesc inner
        LLVM.mdNode [ classmd, innernode ]
    genTypedesc (PtrType (GCObj ptrclass header)) =
      let
        (tname, mob, innermut) = gcheaders ! header
        (_, Just inner) = types ! tname
      in do
        classmd <- LLVM.mdString "gcptr"
        mobmd <- mobilityValue mob
        ptrclassmd <- ptrClassValue ptrclass
        innernode <- genTypedesc inner
        LLVM.mdNode [ classmd, ptrclassmd, mobmd, innernode ]
    genTypedesc (IntType _ size) =
      do
        classmd <- LLVM.mdString "int"
        LLVM.mdNode [ classmd, LLVM.constInt LLVM.int32Type size False ]
    genTypedesc (IdType tname) =
      let
        (str, _) = types ! tname
      in do
        classmd <- LLVM.mdString "named"
        mdstr <- LLVM.mdString str
        LLVM.mdNode [ classmd, mdstr ]
    genTypedesc (FloatType 32) =
      do
        classmd <- LLVM.mdString "float"
        LLVM.mdNode [ classmd ]
    genTypedesc (FloatType 64) =
      do
        classmd <- LLVM.mdString "double"
        LLVM.mdNode [ classmd ]
    genTypedesc (FloatType 128) =
      do
        classmd <- LLVM.mdString "fp128"
        LLVM.mdNode [ classmd ]

    genHeaderMD :: GCHeader -> IO ()
    genHeaderMD header =
      let
        (tname, mob, mut) = gcheaders ! header
        (str, Just ty) = types ! tname
      in do
        typedesc <- genTypedesc ty
        mdstr <- LLVM.mdString str
        mobmd <- mobilityValue mob
        mutmd <- mutabilityValue mut
        mdnode <- LLVM.mdNode [ mdstr, mobmd, mutmd, typedesc ]
        LLVM.addNamedMetadataOperand mod "core.gc.typedesc.md" mdnode
  in
    mapM_ genHeaderMD gengcs

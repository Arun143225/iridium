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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}


module IR.PolyIR.Syntax(
       Type(..),
       ) where

import Data.Map(Map)
import Data.Position

data Type field bound free =
  -- | A function type
    Func {
      funcTyRetType :: Type field bound free,
      funcTyArgTys :: [Type field bound free],
      funcTyPos :: !Position
    }
  -- | A structure, representing both tuples and records
  | Struct {
      structPacked :: !Bool,
      structFields :: !Map (Fieldname field)
                           (Mutability, Type field bound free),
      structPos :: !Position
    }
  -- | An array.  Unlike LLVM arrays, these may be variable-sized.
  | Array {
      -- | Size of the array, if known.
      arraySize :: !(Maybe Word),
      -- | Array element type.
      arrayElemTy :: Type field bound free
      -- | Position in source from which this arises.
      arrayPos :: !Position
    }
  -- | Pointers, both native and GC
  | Ptr !ObjType
  -- | An integer, possibly signed, with a size
  | Int {
      intSigned :: !Bool,
      intWidth :: !Word,
      intPos :: !Position
    }
  -- | A defined type
  | Typevar {
      typevarName :: !Typename bound free,
      typevarPos :: !Position
    }
  -- | Floating point types
  | Float {
      floatWidth :: !Word,
      floatPos :: !Position
    }
  -- | The unit type, equivalent to SML unit and C/Java void
  | Unit !Position
    deriving Eq

data Scope = Scope {
    -- | A map from typenames to their proper names and possibly their
    -- definitions
    scopeTypes :: Array Typename (String, Maybe Type),
    -- | A map from GCHeaders to their definitions
    scopeGCHeaders :: Array GCHeader (Typename, Mobility, Mutability),
    -- | Generated GC types (this module will generate the signatures
    -- and accessors)
    scopeGenGCs :: [GCHeader],
    -- | A map from global names to the corresponding functions
    scopeGlobals :: Array Globalname (Global elem gr),
    -- | The position in source from which this arises.  This is here
    -- solely to record filenames in a unified way.
    scopePos :: !Pos
  }

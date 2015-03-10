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

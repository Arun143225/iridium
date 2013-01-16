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

data Type field bound free =
  -- | A function type
    Func (Type field bound free) [Type field bound free]
  -- | A structure, representing both tuples and records
  | Struct !Bool (Map (Fieldname field) (Mutability, Type field bound free))
  -- | An array.  Unlike LLVM arrays, these may be variable-sized
  | Array !(Maybe Word) (Type field bound free)
  -- | Pointers, both native and GC
  | Ptr !ObjType
  -- | An integer, possibly signed, with a size
  | Int !Bool !Word
  -- | A defined type
  | Typevar !(Typename bound free)
  -- | Floating point types
  | Float !Word
  -- | The unit type, equivalent to SML unit and C/Java void
  | Unit
    deriving Eq

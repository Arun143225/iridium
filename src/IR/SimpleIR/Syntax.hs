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

module IR.SimpleIR.Syntax(
       ) where

import Data.HashMap.Strict(HashMap)

import Data.HashMap.Strict as HashMap

-- | A basic block
data Block elem =
    Block {
      -- | The statements in the basic block
      blockBody :: ![elem],
      -- | The transfer for the basic block
      blockXfer :: !Transfer,
      -- | The position in source from which this arises.
      blockPos :: !Pos
    }

-- | The body of a function
data Body elem gr =
    Body {
      -- | The entry block
      bodyEntry :: !Label,
      -- | The CFG
      bodyCFG :: !(gr (Block elem) ())
    }

data Function elem gr = Function {
    -- | Return type
    funcRetTy :: !Type,
    -- | A map from identifiers for arguments and local variables to
    -- their types.
    funcArgTys :: !HashMap Id Type,
    -- | The function's body, if it has one
    funcBody :: !Maybe (Body elem gr),
    -- | The position in source from which this arises.
    funcPos :: !Pos
  }

-- | A module.  Represents a concept similar to an LLVM module.
data Module elem gr =
    Module {
      -- | Name of the module
      modName :: !String,
      -- | A map from typenames to their proper names and possibly their
      -- definitions
      modTypes :: !(Array Typename (String, Maybe Type)),
      -- | A map from GCHeaders to their definitions
      modGCHeaders :: Array GCHeader (Typename, Mobility, Mutability),
      -- | Generated GC types (this module will generate the signatures)
      modGenGCs :: ![GCHeader],
      -- | The top-level scope for this module.
      modScope :: !(Scope elem gr),
      -- | The position in source from which this arises.  This is here
      -- solely to record filenames in a unified way.
      modPos :: !Pos
    }

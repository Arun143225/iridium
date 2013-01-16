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

-- | This module defines type constructors that represent all GC
-- options.  This allows sharing of GC type metadata amongst the
-- various IR languages.
module IR.Common.GC(
       -- * GC Type Metadata
       Type(..),

       -- * Options
       Mobility(..),
       PtrClass(..),
       Mutability(..)
       ) where

import Data.Hash
import Text.Format

-- | The type of object pointed to by a pointer
data Type
       -- | The type of GC object type information.
       gctype
       -- | The type of native object type information.
       nativetype =
  -- | An object in GC space
    GC !PtrClass gctype
  -- | An object in non-GC space
  | Native nativetype
    deriving (Eq, Ord)

-- | Object mobility.  All native objects are immobile.  GC objects
-- can be mobile or immobile.  Immobile objects must be supported for
-- a sane FFI.
data Mobility =
  -- | The object's address may change during execution (specifically,
  -- due to garbage collection)
    Mobile
  -- | The object's address cannot change during execution.  Use to
  -- allocate buffers for IO, or objects for foreign calls.
  | Immobile
    deriving (Eq, Ord)

-- | Indicates the class of pointers.  This is relevant only to
-- pointers to grabage collected objects.
data PtrClass =
  -- | A strong GC pointer.  Acts as a "normal" pointer.
    Strong
  -- | A soft GC pointer.  Any object which is reachable from the root
  -- set only by soft pointers or weaker pointers may have all such
  -- pointers cleared in response to memory pressure.
  | Soft
  -- | A weak GC pointer.  Any object which is reachable only from the
  -- root set only by weak pointers will have all such pointer cleared
  -- during a collection cycle.
  | Weak
  -- | A finalizer GC pointer.  When an object is reachable only by
  -- finalizers, it will result in the finalizer threads becoming
  -- runnable.
  | Finalizer
  -- | A phantom GC pointer.  These should never be accessed by the
  -- program code, but will prevent an object's deletion during a
  -- collection cycle.
  | Phantom
    deriving (Eq, Ord)

-- | Mutability of fields and objects.  Mutability, and particular
-- variants thereof are of paramount importance during garbage
-- collection.
data Mutability =
  -- | The field is immutable
    Immutable
  -- | The field is mutable
  | Mutable
  -- | The field can only be updated once (ie. initialized)
  | WriteOnce
    deriving (Eq, Ord)

instance Hashable Mobility where
  hash Mobile = hashInt 1
  hash Immobile = hashInt 2

instance Hashable PtrClass where
  hash Strong = hashInt 1
  hash Soft = hashInt 2
  hash Weak = hashInt 3
  hash Finalizer = hashInt 4
  hash Phantom = hashInt 5

instance Hashable Mutability where
  hash Immutable = hashInt 1
  hash Mutable = hashInt 2
  hash WriteOnce = hashInt 3

instance (Hashable gctype, Hashable nativetype) =>
         Hashable (Type gctype nativetype) where
  hash (GC ptrclass ty) = hashInt 1 `combine` hash ptrclass `combine` hash ty
  hash (Native ty) = hashInt 2 `combine` hash ty

instance Show Mobility where
  show Mobile = "mobile"
  show Immobile = "immobile"

instance Show PtrClass where
  show Strong = "strong"
  show Soft = "soft"
  show Weak = "weak"
  show Finalizer = "finalizer"
  show Phantom = "phantom"

instance Show Mutability where
  show Immutable = "immutable"
  show Mutable = "mutable"
  show WriteOnce = "writeonce"

instance (Show gctype, Show nativetype) =>
         Show (Type gctype nativetype) where
  show (GC ptrclass ty) = "gc " ++ show ptrclass ++ " " ++ show ty
  show (Native ty) = "native " ++ show ty

instance Format Mobility where
  format = format . show

instance Format PtrClass where
  format = format . show

instance Format Mutability where
  format = format . show

instance (Format gctype, Format nativetype) =>
         Format (Type gctype nativetype) where
  format (GC ptrclass ty) = "gc" <+> show ptrclass <+> format ty
  format (Native ty) = "native" <+> format ty

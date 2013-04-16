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
module IR.Common.Ptr(
       -- * GC Type Metadata
       Ptr(..),

       -- * Options
       Mobility(..),
       PtrClass(..),
       Mutability(..),

       -- * Utility Functions
       mergeMutability
       ) where

import Data.Hash
import Data.Pos
import Text.Format

-- | The type of object pointed to by a pointer
data Ptr
       -- | The type of GC object type information.
       gctype
       -- | The type of native object type information.
       nativetype =
  -- | An object in GC space
    GC {
      -- | The pointer classification of this pointer.
      gcClass :: !PtrClass,
      -- | The mutability of the pointed-to data.
      gcMutability :: !Mutability,
      -- | The underlying element type.
      gcTy :: gctype
    }
  -- | An object in non-GC space
  | Native {
      -- | The mutability of the pointed-to data.
      nativeMutability :: !Mutability,
      -- | The underlying element type.      
      nativeTy :: nativetype
    }
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
  -- | The field is immutable.
    Immutable
  -- | The field can only be updated once (ie. initialized).  This is
  -- used only by the garbage collector; for runtime's sake it counts
  -- as mutable.
  | WriteOnce
  -- | The field is mutable.
  | Mutable
  -- | The field is volatile.
  | Volatile
  -- | Like WriteOnce, but also volatile.
  | VolatileOnce
    deriving (Eq, Ord)

-- | Given the mutability of a defining struct and a field, decide
-- what the actual mutability is.
mergeMutability :: Mutability
                -- ^ The struct's mutability.
                -> Mutability
                -- ^ The field's mutability.
                -> Mutability
                -- ^ The actual mutability.

-- Immutable overrides everything else.
mergeMutability Immutable _ = Immutable
mergeMutability _ Immutable = Immutable
-- VolatileOnce overrides everything else after immutability.
mergeMutability VolatileOnce _ = VolatileOnce
mergeMutability _ VolatileOnce = VolatileOnce
-- Volatile and WriteOnce combine into VolatileOnce
mergeMutability Volatile WriteOnce = VolatileOnce
mergeMutability WriteOnce Volatile = VolatileOnce
-- After immutability, volatility is strongest.
mergeMutability Volatile _ = Volatile
mergeMutability _ Volatile = Volatile
-- Writeonce is weaker than volatility.
mergeMutability WriteOnce _ = WriteOnce
mergeMutability _ WriteOnce = WriteOnce
-- Mutable carries no information
mergeMutability Mutable Mutable = Mutable

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
  hash Volatile = hashInt 4
  hash VolatileOnce = hashInt 5

instance (Hashable gctype, Hashable nativetype) =>
         Hashable (Ptr gctype nativetype) where
  hash (GC { gcClass = ptrclass, gcTy = ty, gcMutability = mut }) =
    hashInt 1 `combine` hash ptrclass `combine` hash ty `combine` hash mut
  hash (Native { nativeTy = ty, nativeMutability = mut }) =
    hashInt 2 `combine` hash ty `combine` hash mut

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
  show Volatile = "volatile"
  show VolatileOnce = "volatileonce"

instance (Show gctype, Show nativetype) =>
         Show (Ptr gctype nativetype) where
  show (GC { gcClass = ptrclass, gcTy = ty, gcMutability = mut }) =
    "gc " ++ show ptrclass ++ " " ++ show ty ++ " " ++ show mut
  show (Native { nativeTy = ty, nativeMutability = mut }) =
    "native " ++ show ty ++ " " ++ show mut

instance Format Mobility where
  format = format . show

instance Format PtrClass where
  format = format . show

instance Format Mutability where
  format = format . show

instance (Format gctype, Format nativetype) =>
         Format (Ptr gctype nativetype) where
  format (GC { gcClass = ptrclass, gcTy = ty, gcMutability = mut }) =
    "gc" <+> show ptrclass <+> format ty <+> format mut
  format (Native { nativeTy = ty, nativeMutability = mut }) =
    "native" <+> format ty <+> format mut

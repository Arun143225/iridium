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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

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
       ) where

import Data.Hashable
import Data.Monoid
import Data.Word
import Text.Format
import Text.XML.Expat.Pickle hiding (Node)
import Text.XML.Expat.Tree(NodeG)

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
    deriving (Eq, Ord, Enum)

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
  -- finalizers, it will result in the finalizer becoming active.
  | Finalizer
  -- | A phantom GC pointer.  These should never be accessed by the
  -- program code, but will prevent an object's deletion during a
  -- collection cycle.
  | Phantom
    deriving (Eq, Ord, Enum)

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
    deriving (Eq, Ord, Enum)

instance Monoid Mutability where
  mempty = Mutable

  -- Immutable overrides everything else.
  mappend Immutable _ = Immutable
  mappend _ Immutable = Immutable
  -- VolatileOnce overrides everything else after immutability.
  mappend VolatileOnce _ = VolatileOnce
  mappend _ VolatileOnce = VolatileOnce
  -- Volatile and WriteOnce combine into VolatileOnce
  mappend Volatile WriteOnce = VolatileOnce
  mappend WriteOnce Volatile = VolatileOnce
  -- After immutability, volatility is strongest.
  mappend Volatile _ = Volatile
  mappend _ Volatile = Volatile
  -- Writeonce is weaker than volatility.
  mappend WriteOnce _ = WriteOnce
  mappend _ WriteOnce = WriteOnce
  -- Mutable carries no information
  mappend Mutable Mutable = Mutable

instance Hashable Mobility where hashWithSalt s m = s `hashWithSalt` fromEnum m
instance Hashable PtrClass where hashWithSalt s p = s `hashWithSalt` fromEnum p
instance Hashable Mutability where hashWithSalt s m = s `hashWithSalt` fromEnum m

instance (Hashable gctype, Hashable nativetype) =>
         Hashable (Ptr gctype nativetype) where
  hashWithSalt s GC { gcClass = ptrclass, gcTy = ty, gcMutability = mut } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt`
    ptrclass `hashWithSalt`ty `hashWithSalt` mut
  hashWithSalt s Native { nativeTy = ty, nativeMutability = mut } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` ty `hashWithSalt` mut

instance Functor (Ptr gctype) where
  fmap f ptr @ Native { nativeTy = ty } = ptr { nativeTy = f ty }
  fmap _ ptr @ GC { gcClass = cls, gcMutability = mut, gcTy = ty } =
    ptr { gcClass = cls, gcMutability = mut, gcTy = ty }

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
  format = string . show

instance Format PtrClass where
  format = string . show

instance Format Mutability where
  format = string . show

instance (Format gctype, Format nativetype) =>
         Format (Ptr gctype nativetype) where
  format (GC { gcClass = ptrclass, gcTy = ty, gcMutability = mut }) =
    string "gc" <+> format ptrclass <+> format ty <+> format mut
  format (Native { nativeTy = ty, nativeMutability = mut }) =
    string "native" <+> format ty <+> format mut

gcPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text,
              XmlPickler [NodeG [] tag text] gctype) =>
             PU [NodeG [] tag text] (Ptr gctype nativetype)
gcPickler =
  let
    revfunc GC { gcClass = cls, gcMutability = mut, gcTy = ty } =
      ((cls, mut), ty)
    revfunc _ = error "can't convert"
  in
    xpWrap (\((cls, mut), ty) -> GC { gcClass = cls, gcTy = ty,
                                      gcMutability = mut }, revfunc)
           (xpElem (gxFromString "GC") (xpPair xpickle xpickle) xpickle)

nativePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text,
                  XmlPickler [NodeG [] tag text] nativetype) =>
                 PU [NodeG [] tag text] (Ptr gctype nativetype)
nativePickler =
  let
    revfunc Native { nativeMutability = mut, nativeTy = ty } = (mut, ty)
    revfunc _ = error "can't convert"
  in
    xpWrap (\(mut, ty) -> Native { nativeMutability = mut, nativeTy = ty },
            revfunc)
           (xpElem (gxFromString "Native") xpickle xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] gctype,
          XmlPickler [NodeG [] tag text] nativetype) =>
         XmlPickler [NodeG [] tag text] (Ptr gctype nativetype) where
  xpickle =
    let
      picker GC {} = 0
      picker Native {} = 1
    in
      xpAlt picker [gcPickler, nativePickler]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Mobility where
  xpickle = xpAlt fromEnum
                  [xpWrap (const Mobile, const ())
                          (xpAttrFixed (gxFromString "mobility")
                                       (gxFromString "strong")),
                   xpWrap (const Immobile, const ())
                          (xpAttrFixed (gxFromString "mobility")
                                       (gxFromString "weak"))]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) PtrClass where
  xpickle = xpAlt fromEnum
                  [xpWrap (const Strong, const ())
                          (xpAttrFixed (gxFromString "ptr-class")
                                       (gxFromString "strong")),
                   xpWrap (const Weak, const ())
                          (xpAttrFixed (gxFromString "ptr-class")
                                       (gxFromString "weak")),
                   xpWrap (const Soft, const ())
                          (xpAttrFixed (gxFromString "ptr-class")
                                       (gxFromString "soft")),
                   xpWrap (const Finalizer, const ())
                          (xpAttrFixed (gxFromString "ptr-class")
                                       (gxFromString "finalizer")),
                   xpWrap (const Phantom, const ())
                          (xpAttrFixed (gxFromString "ptr-class")
                                       (gxFromString "phantom"))]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Mutability where
  xpickle = xpAlt fromEnum
                  [xpWrap (const Immutable, const ())
                          (xpAttrFixed (gxFromString "mutability")
                                       (gxFromString "immutable")),
                   xpWrap (const WriteOnce, const ())
                          (xpAttrFixed (gxFromString "mutability")
                                       (gxFromString "writeonce")),
                   xpWrap (const Mutable, const ())
                          (xpAttrFixed (gxFromString "mutability")
                                       (gxFromString "mutable")),
                   xpWrap (const VolatileOnce, const ())
                          (xpAttrFixed (gxFromString "mutability")
                                       (gxFromString "volatileonce")),
                   xpWrap (const Volatile, const ())
                          (xpAttrFixed (gxFromString "mutability")
                                       (gxFromString "volatile"))]

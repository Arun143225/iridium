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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module IR.Common.Ptr(
       -- * Tagged Pointer Metadata
       Ptr(..),
       TagDesc(..),

       -- * Options
       Mutability(..),
       ) where

import Data.Hashable
import IR.Common.Names
import Text.Format
import Text.XML.Expat.Pickle hiding (Node)
import Text.XML.Expat.Tree(NodeG)

-- | The type of object pointed to by a pointer
data Ptr
       -- | The type of tagged object type information.
       tagdata
       -- | The type of native object type information.
       nativetype =
  -- | An tagged object
    Tagged {
      -- | The mutability of the pointed-to data.
      taggedMutability :: !Mutability,
      -- | The name of the tag descriptor.
      taggedTag :: !Tagname,
      -- | The tag header metadata.
      taggedData :: tagdata
    }
  -- | An object in non-tagged space
  | Native {
      -- | The mutability of the pointed-to data.
      nativeMutability :: !Mutability,
      -- | The underlying element type.
      nativeTy :: nativetype
    }
    deriving (Eq, Ord)

-- | A type descriptor pointed to by tag headers.
data TagDesc tagdata =
  TagDesc {
    -- | Mutability of the object described by this descriptor.
    tagDescMutability :: !Mutability,
    -- | Name of the underlying type described by this descriptor.
    tagDescTy :: !Typename,
    -- | Extra metadata contained in this descriptor.
    tagDescData :: tagdata
  }

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

instance Hashable Mutability where
  hashWithSalt s m = s `hashWithSalt` fromEnum m

instance (Hashable tagdata, Hashable nativetype) =>
         Hashable (Ptr tagdata nativetype) where
  hashWithSalt s Tagged { taggedTag = tag, taggedData = dat,
                          taggedMutability = mut } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt`
    tag `hashWithSalt` dat `hashWithSalt` mut
  hashWithSalt s Native { nativeTy = ty, nativeMutability = mut } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` ty `hashWithSalt` mut

instance Hashable tagdata => Hashable (TagDesc tagdata) where
  hashWithSalt s TagDesc { tagDescTy = ty, tagDescData = dat,
                           tagDescMutability = mut } =
    s `hashWithSalt` ty `hashWithSalt` dat `hashWithSalt` mut

instance Functor (Ptr tagdata) where
  fmap f ptr @ Native { nativeTy = ty } = ptr { nativeTy = f ty }
  fmap _ Tagged { taggedTag = tag, taggedData = dat, taggedMutability = mut } =
    Tagged { taggedTag = tag, taggedData = dat, taggedMutability = mut }

instance Show Mutability where
  show Immutable = "immutable"
  show Mutable = "mutable"
  show WriteOnce = "writeonce"
  show Volatile = "volatile"
  show VolatileOnce = "volatileonce"

instance Format Mutability where
  format = string . show

instance (Format tagdata, Format nativetype) =>
         Format (Ptr tagdata nativetype) where
  format Tagged { taggedTag = tag, taggedData = dat, taggedMutability = mut } =
    string "tagged" <+> format mut <+> format dat <+> format tag
  format Native { nativeTy = ty, nativeMutability = mut } =
    string "native" <+> format mut <+> format ty

instance Format tagdata => Format (TagDesc tagdata) where
  format TagDesc { tagDescTy = ty, tagDescData = dat,
                   tagDescMutability = mut } =
    string "typedesc" <+> format mut <+> format ty <+> format dat

taggedPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text,
                  XmlPickler [NodeG [] tag text] tagdata) =>
                 PU [NodeG [] tag text] (Ptr tagdata nativetype)
taggedPickler =
  let
    revfunc Tagged { taggedTag = tag, taggedData = dat,
                     taggedMutability = mut } = ((mut, tag), dat)
    revfunc _ = error "can't convert"
  in
    xpWrap (\((mut, tag), dat) -> Tagged { taggedData = dat, taggedTag = tag,
                                           taggedMutability = mut }, revfunc)
           (xpElem (gxFromString "tagged") (xpPair xpickle xpickle) xpickle)

nativePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text,
                  XmlPickler [NodeG [] tag text] nativetype) =>
                 PU [NodeG [] tag text] (Ptr tagdata nativetype)
nativePickler =
  let
    revfunc Native { nativeMutability = mut, nativeTy = ty } = (mut, ty)
    revfunc _ = error "can't convert"
  in
    xpWrap (\(mut, ty) -> Native { nativeMutability = mut, nativeTy = ty },
            revfunc)
           (xpElem (gxFromString "Native") xpickle xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] tagdata,
          XmlPickler [NodeG [] tag text] nativetype) =>
         XmlPickler [NodeG [] tag text] (Ptr tagdata nativetype) where
  xpickle =
    let
      picker Tagged {} = 0
      picker Native {} = 1
    in
      xpAlt picker [taggedPickler, nativePickler]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] tagdata) =>
         XmlPickler [NodeG [] tag text] (TagDesc tagdata) where
  xpickle =
    xpWrap (\((ty, mut), dat) -> TagDesc { tagDescTy = ty, tagDescData = dat,
                                           tagDescMutability = mut },
            \TagDesc { tagDescTy = ty, tagDescData = dat,
                       tagDescMutability = mut } -> ((ty, mut), dat))
           (xpElem (gxFromString "TagDesc") (xpPair xpickle xpickle) xpickle)

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

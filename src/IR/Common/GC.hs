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

-- | This module defines type constructors that represent all GC
-- options.  This allows sharing of GC type metadata amongst the
-- various IR languages.
module IR.Common.GC(
       -- * Tagged Pointer Metadata
       GCPtr(..),

       -- * Options
       Mobility(..),
       PtrClass(..),
       ) where

import Data.Hashable
import Text.Format
import Text.XML.Expat.Pickle hiding (Node)
import Text.XML.Expat.Tree(NodeG)

-- | GC Pointer information.
data GCPtr elemty =
  GCPtr {
    -- | The pointer classification of this pointer.
    gcPtrClass :: !PtrClass,
    -- | The element type of the pointer.
    gcPtrElem :: !elemty
  }

-- | Object mobility.  All native objects are immobile.  Tagged objects
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

instance Hashable Mobility where hashWithSalt s m = s `hashWithSalt` fromEnum m
instance Hashable PtrClass where hashWithSalt s p = s `hashWithSalt` fromEnum p

instance (Hashable gctype) => Hashable (GCPtr gctype) where
  hashWithSalt s GCPtr { gcPtrClass = ptrclass, gcPtrElem = elemty } =
    s `hashWithSalt` ptrclass `hashWithSalt` elemty

instance Show Mobility where
  show Mobile = "mobile"
  show Immobile = "immobile"

instance Show PtrClass where
  show Strong = "strong"
  show Soft = "soft"
  show Weak = "weak"
  show Finalizer = "finalizer"
  show Phantom = "phantom"

instance Format Mobility where
  format = string . show

instance Format PtrClass where
  format = string . show

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

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] gctype) =>
         XmlPickler [NodeG [] tag text] (GCPtr gctype) where
  xpickle =
    let
      revfunc GCPtr { gcPtrClass = cls, gcPtrElem = elemty } = (cls, elemty)
    in
      xpWrap (\(cls, elemty) -> GCPtr { gcPtrClass = cls,
                                        gcPtrElem = elemty }, revfunc)
             (xpElem (gxFromString "gcptr") xpickle xpickle)

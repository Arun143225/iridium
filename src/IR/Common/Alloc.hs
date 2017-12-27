-- Copyright (c) 2017 Eric McCorkle.  All rights reserved.
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
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module IR.Common.Alloc(
       Allocator(..),
       Allocation(..)
       ) where

import Data.Hashable
import IR.Common.Names
import IR.Common.Ptr
import IR.Common.Rename.Class
import IR.Common.RenameType.Class
import Text.Format
import Text.XML.Expat.Pickle hiding (Node)
import Text.XML.Expat.Tree(NodeG)

-- | A description of a source for allocated bytes, to be used in
-- 'Allocation's.
data Allocator =
    -- | Use alloca (stack allocator).
    Alloca
    -- | Call a function, passing in a size parameter.
  | Direct {
      directName :: !Globalname
    }
    -- | Make a call to an intrinsic.
  | Intrinsic {
      intrinsicName :: !Globalname
    }
    deriving (Eq, Ord)

-- | A description of object allocations.  For tagged data, this
-- includes the initialization of the tag data.
data Allocation tagty nativety expty =
  Allocation {
    -- | Allocator to use.
    allocSrc :: !Allocator,
    -- | Kind of object to allocate.
    allocType :: !(Ptr tagty nativety),
    -- | Possibly-empty list of allocation sizes.  Used to supply
    -- sizes to unsized arrays
    allocSizes :: ![expty]
  }
  deriving (Eq, Ord)

instance Hashable Allocator where
  hashWithSalt s Alloca = s `hashWithSalt` (0 :: Int)
  hashWithSalt s Direct { directName = name } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` name
  hashWithSalt s Intrinsic { intrinsicName = name } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` name

instance (Hashable expty, Hashable tagty, Hashable nativety) =>
         Hashable (Allocation tagty nativety expty) where
  hashWithSalt s Allocation { allocSrc = src, allocType = ty,
                              allocSizes = sizes } =
    s `hashWithSalt` src `hashWithSalt` ty `hashWithSalt` sizes

instance (Rename Id expty) =>
         Rename Id (Allocation tagty nativety expty) where
  rename f a @ Allocation { allocSizes = sizes } =
    a { allocSizes = map (rename f) sizes }

instance (RenameType Typename expty, RenameType Typename nativety) =>
         RenameType Typename (Allocation tagty nativety expty) where
  renameType f a @ Allocation { allocType = ty, allocSizes = sizes } =
    a { allocType = fmap (renameType f) ty,
        allocSizes = map (renameType f) sizes }

instance Format Allocator where
  format Alloca = string "alloca"
  format Direct { directName = name } = string "direct" <+> format name
  format Intrinsic { intrinsicName = name } =
    string "intrinsic" <+> format name

allocaPickler :: (GenericXMLString tag, Show tag,
                       GenericXMLString text, Show text) =>
                      PU [NodeG [] tag text] Allocator
allocaPickler = xpWrap (const Alloca, const ())
                       (xpElemNodes (gxFromString "Alloca") xpUnit)

directPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Allocator
directPickler =
  let
    revfunc Direct { directName = name } = name
    revfunc _ = error "cannot convert"
  in
    xpWrap (Direct, revfunc)
           (xpElemNodes (gxFromString "Direct") xpickle)

intrinsicPickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] Allocator
intrinsicPickler =
  let
    revfunc Intrinsic { intrinsicName = name } = name
    revfunc _ = error "cannot convert"
  in
    xpWrap (Intrinsic, revfunc)
           (xpElemNodes (gxFromString "Intrinsic") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Allocator where
  xpickle =
    let
      picker Alloca = 0
      picker Direct {} = 1
      picker Intrinsic {} = 1
    in
      xpAlt picker [allocaPickler, directPickler, intrinsicPickler]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] tagty,
          XmlPickler [NodeG [] tag text] nativety,
          XmlPickler [NodeG [] tag text] expty) =>
         XmlPickler [NodeG [] tag text] (Allocation tagty nativety expty) where
  xpickle =
    xpWrap (\(src, ty, sizes) -> Allocation { allocSrc = src, allocType = ty,
                                              allocSizes = sizes },
            \Allocation { allocSrc = src, allocType = ty,
                          allocSizes = sizes } ->(src, ty, sizes))
           (xpElemNodes (gxFromString "Allocation")
                        (xpTriple (xpElemNodes (gxFromString "src") xpickle)
                                  (xpElemNodes (gxFromString "type") xpickle)
                                  (xpElemNodes (gxFromString "sizes")
                                               (xpList xpickle))))

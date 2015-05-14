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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module IR.Common.TBAA(
       TBAA(..),
       TBAANode(..)
       ) where

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)

import qualified Data.ByteString as Strict

newtype TBAA ty = TBAA { tbaaNodes :: [TBAANode ty] }
  deriving (Eq, Ord)

data TBAANode ty =
  TBAANode {
      -- | The name of the type.
      tbaaName :: !Strict.ByteString,
      -- | Whether or not the type is constant.
      tbaaConst :: !Bool,
      -- | The node's children.
      tbaaChildren :: ![TBAANode ty],
      -- | The type corresponding to this node.
      tbaaType :: !ty
    }
    deriving (Eq, Ord)

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] ty) =>
         XmlPickler [NodeG [] tag text] (TBAANode ty) where
  xpickle =
    xpWrap (\((name, con), (children, ty)) ->
             TBAANode { tbaaName = gxToByteString name, tbaaType = ty,
                        tbaaConst = con, tbaaChildren = children },
            \TBAANode { tbaaName = name, tbaaConst = con,
                        tbaaChildren = children, tbaaType = ty } ->
             ((gxFromByteString name, con), (children, ty)))
           (xpElem (gxFromString "TBAANode")
                   (xpPair (xpAttr (gxFromString "name") xpText)
                           (xpAttr (gxFromString "const") xpPrim))
                   (xpPair (xpElemNodes (gxFromString "children")
                                        (xpList xpickle))
                           (xpElemNodes (gxFromString "type") xpickle)))

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] ty) =>
         XmlPickler [NodeG [] tag text] (TBAA ty) where
  xpickle = xpWrap (TBAA, tbaaNodes) (xpElemNodes (gxFromString "TBAA")
                                                  (xpList xpickle))

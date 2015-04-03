-- Copyright (c) 2015 Eric McCorkle.
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

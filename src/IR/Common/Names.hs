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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module IR.Common.Names(
       DeclNames(..),
       Label,
       Id,
       Fieldname,
       Variantname,
       Typename,
       Globalname,
       GCHeader
       ) where

import Data.Array
import Data.Graph.Inductive.Graph(Node)
import Data.Hashable
import Data.Word
import Text.Format
import Text.XML.Expat.Pickle hiding (Node)
import Text.XML.Expat.Tree(NodeG)

-- | A datatype encoding the various names of a global declaration.
data DeclNames =
  DeclNames {
    -- | The basic name of the declaration.
    basicName :: !String,
    -- | The linkage name of the declaration (often encodes type
    -- information, as in C++)
    linkageName :: !String,
    -- | The displayed name of the declaration (usually contains type
    -- information).
    displayName :: !String
  }

-- | A label, indexes blocks
newtype Label = Label { labelNode :: Node }
  deriving (Ord, Eq, Ix)

-- | An identifier, indexes variables
newtype Id = Id { idName :: Word }
  deriving (Ord, Eq, Ix)

-- | A field name, indexes fields
newtype Fieldname = Fieldname { fieldnameId :: Word }
  deriving (Ord, Eq, Ix)

-- | A variant name, indexes fields
newtype Variantname = Variantname { variantnameId :: Word }
  deriving (Ord, Eq, Ix)

-- | A type name, indexes types
newtype Typename = Typename { typenameId :: Word }
  deriving (Ord, Eq, Ix)

-- | A function name, indexes functions
newtype Globalname = Globalname { globalnameId :: Word }
  deriving (Ord, Eq, Ix)

-- | A header given to GCAlloc representing the type being allocated
newtype GCHeader = GCHeader { gcHeaderId :: Word }
  deriving (Ord, Eq, Ix)

instance Hashable Label where
  hashWithSalt s (Label node) = s `hashWithSalt` node

instance Hashable Id where
  hashWithSalt s (Id name) = s `hashWithSalt` name

instance Hashable Globalname where
  hashWithSalt s (Globalname name) = s `hashWithSalt` name

instance Hashable Fieldname where
  hashWithSalt s (Fieldname name) = s `hashWithSalt` name

instance Hashable Variantname where
  hashWithSalt s (Variantname name) = s `hashWithSalt` name

instance Hashable Typename where
  hashWithSalt s (Typename n) = s `hashWithSalt` n

instance Hashable GCHeader where
  hashWithSalt s (GCHeader n) = s `hashWithSalt` n

instance Format Label where
  format (Label l) = string "L" <> format l

instance Format Fieldname where
  format (Fieldname f) = string "f" <> format f

instance Format Variantname where
  format (Variantname v) = string "v" <> format v

instance Format Id where
  format (Id v) = string "%" <> format v

instance Format Globalname where
  format (Globalname g) = string "@" <> format g

instance Enum Label where
  toEnum = Label
  fromEnum = labelNode

instance Enum Id where
  toEnum = Id . toEnum
  fromEnum = fromEnum . idName

instance Enum Fieldname where
  toEnum = Fieldname . toEnum
  fromEnum = fromEnum . fieldnameId

instance Enum Variantname where
  toEnum = Variantname . toEnum
  fromEnum = fromEnum . variantnameId

instance Enum Globalname where
  toEnum = Globalname . toEnum
  fromEnum = fromEnum . globalnameId

instance Enum Typename where
  toEnum = Typename . toEnum
  fromEnum = fromEnum . typenameId

instance Enum GCHeader where
  toEnum = GCHeader . toEnum
  fromEnum = fromEnum . gcHeaderId

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Label where
  xpickle = xpWrap (Label, labelNode) (xpElemNodes (gxFromString "Label")
                                                   (xpContent xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Label where
  xpickle = xpWrap (Label, labelNode) (xpAttr (gxFromString "label") xpPrim)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Id where
  xpickle = xpWrap (Id, idName) (xpElemNodes (gxFromString "Id")
                                             (xpContent xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Id where
  xpickle = xpWrap (Id, idName) (xpAttr (gxFromString "id") xpPrim)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Fieldname where
  xpickle = xpWrap (Fieldname, fieldnameId)
                   (xpElemNodes (gxFromString "Fieldname")
                                (xpContent xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Fieldname where
  xpickle = xpWrap (Fieldname, fieldnameId)
                   (xpAttr (gxFromString "fieldname") xpPrim)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Variantname where
  xpickle = xpWrap (Variantname, variantnameId)
                   (xpElemNodes (gxFromString "Variantname")
                                (xpContent xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Variantname where
  xpickle = xpWrap (Variantname, variantnameId)
                   (xpAttr (gxFromString "variantname") xpPrim)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Typename where
  xpickle = xpWrap (Typename, typenameId)
                   (xpElemNodes (gxFromString "Typename")
                                (xpContent xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Typename where
  xpickle = xpWrap (Typename, typenameId)
                   (xpAttr (gxFromString "typename") xpPrim)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Globalname where
  xpickle = xpWrap (Globalname, globalnameId)
                   (xpElemNodes (gxFromString "Globalname")
                                (xpContent xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Globalname where
  xpickle = xpWrap (Globalname, globalnameId)
                   (xpAttr (gxFromString "globalname") xpPrim)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] GCHeader where
  xpickle = xpWrap (GCHeader, gcHeaderId)
                   (xpElemNodes (gxFromString "GCHeader")
                                (xpContent xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) GCHeader where
  xpickle = xpWrap (GCHeader, gcHeaderId)
                   (xpAttr (gxFromString "gc-header") xpPrim)

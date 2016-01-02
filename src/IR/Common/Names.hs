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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module IR.Common.Names(
       DeclNames(..),
       Label,
       Id,
       Fieldname,
       Variantname,
       Typename,
       Globalname,
       Tagname,
       typenameId
       ) where

import Data.Array
import Data.Graph.Inductive.Graph(Node)
import Data.Hashable
import Text.Format
import Text.XML.Expat.Pickle hiding (Node)
import Text.XML.Expat.Tree(NodeG)

import qualified Data.ByteString as Strict

-- | A datatype encoding the various names of a global declaration.
data DeclNames =
  DeclNames {
    -- | The basic name of the declaration.
    basicName :: !Strict.ByteString,
    -- | The linkage name of the declaration (often encodes type
    -- information, as in C++)
    linkageName :: !Strict.ByteString,
    -- | The displayed name of the declaration (usually contains type
    -- information).
    displayName :: !Strict.ByteString
  }
  deriving (Ord, Eq)

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

-- | A tag name, indexes tags
newtype Tagname = Tagname { tagId :: Word }
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

instance Hashable Tagname where
  hashWithSalt s (Tagname n) = s `hashWithSalt` n

instance Format DeclNames where
  format = bytestring . displayName

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

instance Format Typename where
  format (Typename t) = string "%t" <> format t

instance Format Tagname where
  format (Tagname t) = string "%tag" <> format t

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

instance Enum Tagname where
  toEnum = Tagname . toEnum
  fromEnum = fromEnum . tagId

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] DeclNames where
  xpickle = xpElemAttrs (gxFromString "DeclNames") xpickle

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) DeclNames where
  xpickle =
    xpWrap (\(bname, lname, dname) ->
             DeclNames { basicName = gxToByteString bname,
                         linkageName = gxToByteString lname,
                         displayName = gxToByteString dname },
            \DeclNames { basicName = bname, linkageName = lname,
                         displayName = dname } ->
            (gxFromByteString bname, gxFromByteString lname,
             gxFromByteString dname))
            (xpTriple (xpAttr (gxFromString "base-name") xpText)
                      (xpAttr (gxFromString "linkage-name") xpText)
                      (xpAttr (gxFromString "display-name") xpText))

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
         XmlPickler [NodeG [] tag text] Tagname where
  xpickle = xpWrap (Tagname, tagId)
                   (xpElemNodes (gxFromString "Tagname")
                                (xpContent xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Tagname where
  xpickle = xpWrap (Tagname, tagId)
                   (xpAttr (gxFromString "tagname") xpPrim)

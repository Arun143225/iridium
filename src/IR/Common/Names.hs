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

module IR.Common.Names(
       DeclNames(..),
       Label,
       Id,
       Fieldname,
       Variantname,
       Typename,
       Globalname,
       GCName
       ) where

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
newtype Label = Label Node
  deriving (Ord, Eq, Ix, Enum)

-- | An identifier, indexes variables
newtype Id = Id Word
  deriving (Ord, Eq, Ix, Enum)

-- | A field name, indexes fields
newtype Fieldname = Fieldname Word
  deriving (Ord, Eq, Ix, Enum)

-- | A variant name, indexes fields
newtype Variantname = Variantname Word
  deriving (Ord, Eq, Ix, Enum)

-- | A type name, indexes types
newtype Typename = Typename Word
  deriving (Ord, Eq, Ix, Enum)

-- | A function name, indexes functions
newtype Globalname = Globalname Word
  deriving (Ord, Eq, Ix, Enum)

-- | A header given to GCAlloc representing the type being allocated
newtype GCHeader = GCHeader Word
  deriving (Ord, Eq, Ix, Enum)

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
  format (Label l) = "L" <> l

instance Format Fieldname where
  format (Fieldname f) = "f" <> f

instance Format Variantname where
  format (Variantname v) = "v" <> v

instance Format Id where
  format (Id v) = "%" <> v

instance Format Globalname where
  format (Globalname g) = "@" <> g

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
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | This module defines a class for things that can have their types
-- alpha-renamed.
module IR.Common.RenameType.Class(
       RenameType(..)
       ) where

-- | Class of things that can have their types alpha-renamed.
class RenameType typename syntax where
  -- | Rename all typenames in the given syntax construct.
  renameType :: (typename -> typename)
             -- ^ A function which renames typenames.
             -> syntax
             -- ^ The syntax construct to rename.
             -> syntax
             -- ^ The input, with the renaming function applied to all
             -- typenames.

instance (RenameType typename syntax, Functor f) =>
         RenameType typename (f syntax) where
  renameType f t = fmap (renameType f) t

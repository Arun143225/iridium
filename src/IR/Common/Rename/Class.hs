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

-- | This module defines a class for things that can be alpha-renamed.
module IR.Common.Rename.Class(
       Rename(..)
       ) where

import Data.Array.IArray(IArray, Ix)

import qualified Data.Array.IArray as IArray

-- | Class of things that can be alpha-renamed.
class Rename id syntax where
  -- | Rename all ids in the given syntax construct.
  rename :: (id -> id)
         -- ^ A function which renames ids.
         -> syntax
         -- ^ The syntax construct to rename.
         -> syntax
         -- ^ The input, with the renaming function applied to all
         -- ids.

instance (Rename id syntax) => Rename id (Maybe syntax) where
  rename f (Just t) = Just (rename f t)
  rename _ Nothing = Nothing

instance (Rename id syntax) => Rename id [syntax] where
  rename f = map (rename f)

instance (Rename id syntax, Ix idx, IArray arr syntax) =>
         Rename id (arr idx syntax) where
  rename f = IArray.amap (rename f)

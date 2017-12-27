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
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | This module defines a class for things that can be alpha-renamed.
module IR.Common.Rename.Class(
       Rename(..),
       renameArray
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

instance Rename id () where
  rename = const $! const ()

instance (Rename id syntax) => Rename id (Maybe syntax) where
  rename f (Just t) = Just (rename f t)
  rename _ Nothing = Nothing

instance (Rename id syntax) => Rename id [syntax] where
  rename f = map (rename f)

renameArray :: (Rename id syntax, Ix idx, IArray arr syntax) =>
               (id -> id)
            -- ^ A function which renames ids.
            -> arr idx syntax
            -- ^ The syntax construct to rename.
            -> arr idx syntax
            -- ^ The input, with the renaming function applied to all
            -- ids.
renameArray f = IArray.amap (rename f)

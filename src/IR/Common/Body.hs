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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module IR.Common.Body(
       Body(..),
       Block(..),
       Stm(..),
       Phi(..),
       Bind(..),
       SSAElems,
       StmElems
       ) where

import Data.Hashable
import Data.Hashable.Extras
import Data.Map(Map)
import Data.Position
import IR.Common.LValue
import IR.Common.Names
import IR.Common.Transfer
import Prelude.Extras
import Text.Format
import Text.FormatM

import qualified Data.Map as Map

-- | A statement.  Represents an effectful action for the statement
-- form of the IR.
data Stm exp =
  -- | Update the given lvalue
    Move {
      -- | The destination LValue.
      moveDst :: !(LValue exp),
      -- | The source expression.
      moveSrc :: !exp,
      -- | The position in source from which this originates.
      movePos :: !Position
    }
  -- | Execute an expression
  | Do !exp

-- | A binding.  Represents an SSA binding in the SSA form of the
-- language.
data Phi =
  -- | A Phi-node.
  Phi {
    -- | The name being bound.
    phiName :: !Id,
    -- | A map from inbound edges to values
    phiVals :: !(Map Label Id),
    -- | The position in source from which this arises.
    phiPos :: !Position
  }

-- | A binding.  Represents an SSA binding in the SSA form of the
-- language.
data Bind exp =
    Bind {
      -- | The name being bound.
      bindName :: !Id,
      -- | The value beind bound.
      bindVal :: !exp,
      -- | The position in source from which this originates.
      bindPos :: !Position
    }
    -- | Execute an expression for its effect only.  Analogous to Do
    -- in the statement language.
  | Effect !exp

-- | A basic block
data Block exp elems =
    Block {
      -- | The statements in the basic block
      blockBody :: !elems,
      -- | The transfer for the basic block
      blockXfer :: !(Transfer exp),
      -- | The position in source from which this arises.
      blockPos :: !Position
    }

-- There is no straightforward ordering, equality, or hashing on Body.

-- | The body of a function
data Body exp elems gr =
    Body {
      -- | The entry block
      bodyEntry :: !Label,
      -- | The CFG
      bodyCFG :: !(gr (Block exp elems) ())
    }

type SSAElems exp = ([Phi], [Bind exp])
type StmElems exp = [Stm exp]

instance Eq1 Stm where
  Move { moveSrc = src1, moveDst = dst1 } ==#
    Move { moveSrc = src2, moveDst = dst2 } = src1 == src2 && dst1 == dst2
  (Do exp1) ==# (Do exp2) = exp1 == exp2
  _ ==# _ = False

instance Eq Phi where
  Phi { phiName = name1, phiVals = vals1 } ==
    Phi { phiName = name2, phiVals = vals2 } =
    name1 == name2 && vals1 == vals2

instance Eq1 Bind where
  Bind { bindName = name1, bindVal = val1 } ==#
    Bind { bindName = name2, bindVal = val2 } =
    name1 == name2 && val1 == val2
  (Effect e1) ==# (Effect e2) = e1 == e2
  _ ==# _ = False

instance Eq2 Block where
  Block { blockBody = body1, blockXfer = xfer1 } ==##
    Block { blockBody = body2, blockXfer = xfer2 } =
    xfer1 == xfer2 && (body1 == body2)

instance Eq exp => Eq (Bind exp) where (==) = (==#)
instance Eq exp => Eq (Stm exp) where (==) = (==#)
instance (Eq elems, Eq exp) => Eq (Block elems exp) where (==) = (==##)

instance Ord1 Stm where
  compare1 Move { moveSrc = src1, moveDst = dst1 }
          Move { moveSrc = src2, moveDst = dst2 } =
    case compare src1 src2 of
      EQ -> compare dst1 dst2
      out -> out
  compare1 (Move {}) _ = LT
  compare1 _ (Move {}) = GT
  compare1 (Do exp1) (Do exp2) = compare exp1 exp2

instance Ord Phi where
  compare Phi { phiName = name1, phiVals = vals1 }
          Phi { phiName = name2, phiVals = vals2 } =
    case compare name1 name2 of
      EQ -> compare vals1 vals2
      out -> out

instance Ord1 Bind where
  compare1 Bind { bindName = name1, bindVal = val1 }
          Bind { bindName = name2, bindVal = val2 } =
    case compare name1 name2 of
      EQ -> compare val1 val2
      out -> out
  compare1 Bind {} _ = LT
  compare1 _ Bind {} = GT
  compare1 (Effect e1) (Effect e2) = compare e1 e2

instance Ord2 Block where
  compare2 Block { blockBody = body1, blockXfer = xfer1 }
           Block { blockBody = body2, blockXfer = xfer2 } =
    case compare xfer1 xfer2 of
      EQ -> compare body1 body2
      out -> out

instance Ord exp => Ord (Bind exp) where compare = compare1
instance Ord exp => Ord (Stm exp) where compare = compare1
instance (Ord exp, Ord elem) => Ord (Block elem exp) where compare = compare2

instance Hashable1 Stm where
  hashWithSalt1 s Move { moveSrc = src, moveDst = dst } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` src `hashWithSalt` dst
  hashWithSalt1 s (Do val) = s `hashWithSalt` (2 :: Int) `hashWithSalt` val

instance Hashable Phi where
  hashWithSalt s Phi { phiName = name, phiVals = vals } =
    s `hashWithSalt` name `hashWithSalt` (Map.toList vals)

instance Hashable1 Bind where
  hashWithSalt1 s Bind { bindName = name1, bindVal = val1 } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` name1 `hashWithSalt` val1
  hashWithSalt1 s (Effect e1) =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` e1

instance Hashable2 Block where
  hashWithSalt2 s Block { blockBody = body, blockXfer = xfer } =
    s `hashWithSalt` xfer `hashWithSalt` body

instance Hashable exp => Hashable (Bind exp) where
  hashWithSalt = hashWithSalt1
instance Hashable exp => Hashable (Stm exp) where
  hashWithSalt = hashWithSalt1
instance (Hashable exp, Hashable elem) => Hashable (Block elem exp) where
  hashWithSalt = hashWithSalt2

instance Format exp => Format (Stm exp) where
  format Move { moveDst = dst, moveSrc = src } =
    format dst <+> string "<-" <+> format src
  format (Do e) = format e

instance Format Phi where
  format Phi { phiName = name, phiVals = vals } =
    let
      mapfun (l, v) = format l <> colon <+> format v
      choicedocs = map mapfun (Map.toList vals)
    in
      format name <+> string "<- phi" <+> list choicedocs

instance Format exp => Format (Bind exp) where
  format Bind { bindName = dst, bindVal = src } =
    format dst <+> string "<-" <+> format src
  format (Effect e) = format e

instance Format exp => Format (Block exp ([Phi], [Bind exp])) where
  format Block { blockBody = (phis, binds), blockXfer = xfer } =
    vcat (map format phis ++ map format binds ++ [format xfer])

instance Format exp => Format (Block exp [Stm exp]) where
  format Block { blockBody = stms, blockXfer = xfer } =
    vcat (map format stms ++ [format xfer])

instance FormatM m exp => FormatM m (Stm exp) where
  formatM Move { moveDst = dst, moveSrc = src } =
    do
      dstdoc <- formatM dst
      srcdoc <- formatM src
      return $! dstdoc <+> string "<-" <+> srcdoc
  formatM (Do e) = formatM e

instance FormatM m exp => FormatM m (Bind exp) where
  formatM Bind { bindName = dst, bindVal = src } =
    do
      srcdoc <- formatM src
      return $! format dst <+> string "<-" <+> srcdoc
  formatM (Effect e) = formatM e

instance FormatM m exp => FormatM m (Block exp ([Phi], [Bind exp])) where
  formatM Block { blockBody = (phis, binds), blockXfer = xfer } =
    do
      binddocs <- mapM formatM binds
      xferdoc <- formatM xfer
      return $! vcat (map format phis ++ binddocs ++ [xferdoc])

instance FormatM m exp => FormatM m (Block exp [Stm exp]) where
  formatM Block { blockBody = stms, blockXfer = xfer } =
    do
      stmdocs <- mapM formatM stms
      xferdoc <- formatM xfer
      return $! vcat (stmdocs ++ [xferdoc])

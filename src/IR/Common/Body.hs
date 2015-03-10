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
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Common.Body(
       Body(..),
       Block(..),
       Stm(..),
       Bind(..),
       SSAElems,
       StmElems
       ) where

import IR.Common.LValue
import IR.Common.Names

-- | A statement.  Represents an effectful action for the statement
-- form of the IR.
data Stm exp =
  -- | Update the given lvalue
    Move {
      -- | The destination LValue.
      moveDst :: !LValue,
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

instance Eq1 Phi where
  Phi { phiName = name1, phiVals = vals1 } ==#
    Phi { phiName = name2, phiVals = vals2 } =
    name1 == name2 && vals1 == vals2

instance Eq1 Bind where
  Bind { bindName = name1, bindVal = val1 } ==#
    Bind { bindName = name2, bindVal = val2 } =
    name1 == name2 && val1 == val2
  (Effect e1) ==# (Effect e2) = e1 == e2
  _ ==# _ = False

instance Eq1 Block where
  Block { blockBody = body1, blockXfer = xfer1 } ==#
    Block { blockBody = body2, blockXfer = xfer2 } =
    xfer1 == xfer2 && (body1 == body2)

instance Eq exp => Eq (Phi exp) where (==) = (==#)
instance Eq exp => Eq (Bind exp) where (==) = (==#)
instance Eq exp => Eq (Stm exp) where (==) = (==#)
instance Eq elem => Eq (Block elem) where (==) = (==#)

instance Ord1 Stm where
  compare1 Move { moveSrc = src1, moveDst = dst1 }
          Move { moveSrc = src2, moveDst = dst2 } =
    case compare src1 src2 of
      EQ -> compare dst1 dst2
      out -> out
  compare1 (Move {}) _ = LT
  compare1 _ (Move {}) = GT
  compare1 (Do exp1) (Do exp2) = compare exp1 exp2

instance Ord1 Phi where
  compare1 Phi { phiName = name1, phiVals = vals1 }
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

instance Ord1 Block where
  compare1 Block { blockBody = body1, blockXfer = xfer1 }
           Block { blockBody = body2, blockXfer = xfer2 } =
    case compare xfer1 xfer2 of
      EQ -> compare1 body1 body2
      out -> out

instance Ord exp => Ord (Phi exp) where compare = compare1
instance Ord exp => Ord (Bind exp) where compare = compare1
instance Ord exp => Ord (Stm exp) where compare = compare1
instance Ord elem => Ord (Block elem) where compare = compare1

instance Hashable1 Stm where
  hashWithSalt1 s Move { moveSrc = src, moveDst = dst } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` src `hashWithSalt` dst
  hashWithSalt1 s (Do val) = s `hashWithSalt` (2 :: Word) `hashWithSalt` val

instance Hashable1 Phi where
  hashWithSalt1 s Phi { phiName = name1, phiVals = vals1 } =
    s `hashWithSalt` name1 `hashWithSalt` vals1

instance Hashable1 Bind where
  hashWithSalt1 s Bind { bindName = name1, bindVal = val1 } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` name1 `hashWithSalt` val1
  hashWithSalt1 s (Effect e1) =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` e1

instance Hashable1 Block where
  hashWithSalt1 s Block { blockBody = body, blockXfer = xfer } =
    (s `hashWithSalt` xfer) `hashWithSalt1` body

instance Hashable exp => Hashable (Phi exp) where
  hashWithSalt = hashWithSalt1
instance Hashable exp => Hashable (Bind exp) where
  hashWithSalt = hashWithSalt1
instance Hashable exp => Hashable (Stm exp) where
  hashWithSalt = hashWithSalt1
instance Hashable elem => Hashable (Block elem) where
  hashWithSalt = hashWithSalt1

instance Format exp => Format (Stm exp) where
  format Move { moveDst = dst, moveSrc = src } =
    format dst <+> "<-" <+> format src
  format (Do e) = format e

instance Format exp => Format (Phi exp) where
  format Phi { phiName = name, phiVals = vals } =
    let
      choicedocs = map (\(l, v) -> l <> colon <+> v) (Map.toList vals)
    in
      format name <+> "<- phi" <+> list choicedocs

instance Format exp => Format (Bind exp) where
  format Bind { bindDst = dst, bindSrc = src } =
    format dst <+> "<-" <+> format src
  format (Effect e) = format e

instance Format exp => Format (Block exp ([Phi], [Bind exp])) where
  format Block { blockBody = (phis, binds), blockXfer = xfer } =
    return $! vcat (map format phis ++ map format binds ++ [format xfer])

instance Format exp => Format (Block exp [Stm exp]) where
  format Block { blockBody = stms, blockXfer = xfer } =
    return $! vcat (map format stm ++ [format xfer])

instance FormatM exp => FormatM (Stm exp) where
  formatM Move { moveDst = dst, moveSrc = src } =
    do
      dstdoc <- formatM dst
      srcdoc <- formatM src
      return $! dstdoc <+> "<-" <+> srcdoc
  formatM (Do e) = formatM e

instance FormatM exp => FormatM (Bind exp) where
  formatM Bind { bindDist = dst, bindSrc = src } =
    do
      srcdoc <- formatM src
      return $! format dst <+> "<-" <+> srcdoc
  formatM (Effect e) = formatM e

instance FormatM exp => FormatM (Block exp ([Phi], [Bind exp])) where
  formatM Block { blockBody = (phis, binds), blockXfer = xfer } =
    do
      binddocs <- mapM formatM binds
      xferdoc <- formatM xfer
      return $! vcat (map format phis ++ binddocs ++ [xferdoc])

instance FormatM exp => FormatM (Block exp [Stm exp]) where
  formatM Block { blockBody = stms, blockXfer = xfer } =
    do
      stmdocs <- mapM formatM stms
      xferdoc <- formatM xfer
      return $! vcat (stmdocs ++ [xferdoc])

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

module IR.Common.Transfer(
       Transfer(..)
       ) where

import Data.Hashable
import Data.Hashable.Extras
import Data.Position
import IR.Common.Names
import IR.Common.Rename
import IR.Common.RenameType
import Prelude.Extras
import Text.Format
import Text.FormatM

-- | Transfers.  These represent methods of leaving a basic block.
-- All basic blocks end in a transfer.
data Transfer exp =
  -- | A direct jump
    Goto {
      -- | The jump target.
      gotoLabel :: !Label,
      -- | The position in source from which this arises.
      gotoPos :: !Position
    }
  -- | A (integer) case expression
  | Case {
      -- | The value being decided upon.  Must be an integer value.
      caseVal :: !exp,
      -- | The cases.  There must be at least one case.
      caseCases :: ![(Integer, Label)],
      -- | The default case.
      caseDefault :: !Label,
      -- | The position in source from which this arises.
      casePos :: !Position
    }
  -- | A return
  | Ret {
      -- | The return value, if one exists.
      retVal :: !(Maybe exp),
      -- | The position in source from which this arises.
      retPos :: !Position
    }
  -- | An unreachable instruction, usually following a call with no
  -- return
  | Unreachable !Position

instance Eq1 Transfer where
  Goto { gotoLabel = label1 } ==# Goto { gotoLabel = label2 } = label1 == label2
  Case { caseVal = val1, caseCases = cases1, caseDefault = def1 } ==#
    Case { caseVal = val2, caseCases = cases2, caseDefault = def2 } =
    val1 == val2 && cases1 == cases2 && def1 == def2
  Ret { retVal = ret1 } ==# Ret { retVal = ret2 } = ret1 == ret2
  Unreachable _ ==# Unreachable _ = True
  _ ==# _ = False

instance Eq exp => Eq (Transfer exp) where
  (==) = (==#)

instance Ord1 Transfer where
  compare1 Goto { gotoLabel = label1 } Goto { gotoLabel = label2 } =
    compare label1 label2
  compare1 Goto {} _ = LT
  compare1 _ Goto {} = GT
  compare1 Case { caseVal = val1, caseCases = cases1, caseDefault = def1 }
          Case { caseVal = val2, caseCases = cases2, caseDefault = def2 } =
    case compare val1 val2 of
      EQ -> case compare def1 def2 of
        EQ -> compare cases1 cases2
        out -> out
      out -> out
  compare1 Case {} _ = LT
  compare1 _ Case {} = GT
  compare1 Ret { retVal = ret1 } Ret { retVal = ret2 } = compare ret1 ret2
  compare1 Ret {} _ = LT
  compare1 _ Ret {} = GT
  compare1 (Unreachable _) (Unreachable _) = EQ

instance Ord exp => Ord (Transfer exp) where
  compare = compare1

instance Hashable1 Transfer where
  hashWithSalt1 s Goto { gotoLabel = label } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` label
  hashWithSalt1 s Case { caseVal = val, caseCases = cases, caseDefault = def } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt`
      val `hashWithSalt` cases `hashWithSalt` def
  hashWithSalt1 s Ret { retVal = val } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` val
  hashWithSalt1 s (Unreachable _) = s `hashWithSalt` (4 :: Int)

instance Hashable elem => Hashable (Transfer elem) where
  hashWithSalt = hashWithSalt1

instance RenameType Typename exp => RenameType Typename (Transfer exp) where
  renameType f tr @ Case { caseVal = val } = tr { caseVal = renameType f val  }
  renameType f tr @ Ret { retVal = val } = tr { retVal = renameType f val }
  renameType _ tr = tr

instance Rename Id exp => Rename Id (Transfer exp) where
  rename f tr @ Case { caseVal = val } = tr { caseVal = rename f val  }
  rename f tr @ Ret { retVal = val } = tr { retVal = rename f val }
  rename _ tr = tr

instance Format exp => Format (Transfer exp) where
  format Goto { gotoLabel = l } = string "goto" <+> format l
  format Case { caseVal = e, caseCases = cases, caseDefault = def } =
    let
      mapfun (i, l) = format i <> colon <+> format l
      casesdoc = list ((string "default" <> colon <+> format def) :
                       map mapfun cases)
    in
      string "case" <+> align (format e </> casesdoc)
  format Ret { retVal = Just e } = string "ret" <+> format e
  format Ret { retVal = Nothing } = string "ret"
  format (Unreachable _) = string "Unreachable"

instance FormatM m exp => FormatM m (Transfer exp) where
  formatM Goto { gotoLabel = l } = return $! string "goto" <+> format l
  formatM Case { caseVal = e, caseCases = cases, caseDefault = def } =
    let
      mapfun (i, l) = format i <> colon <+> format l
      casesdoc = list ((string "default" <> colon <+> format def) :
                       map mapfun cases)
    in do
      edoc <- formatM e
      return $! string "case" <> align (softbreak <> edoc </> casesdoc)
  formatM Ret { retVal = Just e } =
    do
      edoc <- formatM e
      return $! string "ret" <+> edoc
  formatM Ret { retVal = Nothing } = return $! string "ret"
  formatM (Unreachable _) = return $! string "Unreachable"

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

-- | Module defining operators common to all IR languages.
module IR.Common.Operator(
       Binop(..),
       Unop(..)
       ) where

import Data.Hashable
import Text.Format

-- | Binary operators.  The Arithmetic operators exist for matched
-- integer and floating point types.  Subtraction always produces a
-- signed integer type.
data Binop =
    Add | AddNW | Sub | SubNW | Mul | MulNW | Div | Mod
  | And | Or | Xor | Shl | Shr | Eq | Neq | Ge | Le | Gt | Lt
  | FOEq | FONeq | FOGt | FOGe | FOLt | FOLe
  | FUEq | FUNeq | FUGt | FUGe | FULt | FULe
    deriving (Eq, Ord, Enum)

-- | Unary operators
data Unop = Neg | NegNW | Not
    deriving (Eq, Ord, Enum)

instance Hashable Unop where
  hashWithSalt s u = s `hashWithSalt` fromEnum u

instance Hashable Binop where
  hashWithSalt s b = s `hashWithSalt` fromEnum b

instance Show Unop where
  show Neg = "neg"
  show NegNW = "negnw"
  show Not = "not"

instance Show Binop where
  show Add = "add"
  show AddNW = "addnw"
  show Sub = "sub"
  show SubNW = "subnw"
  show Mul = "mul"
  show MulNW = "mulnw"
  show Div = "div"
  show Mod = "mod"
  show Shl = "shl"
  show Shr = "shr"
  show And = "and"
  show Or = "or"
  show Xor = "xor"
  show Eq = "eq"
  show Neq = "ne"
  show Ge = "ge"
  show Le = "le"
  show Gt = "gt"
  show Lt = "lt"
  show FOEq = "foeq"
  show FONeq = "foneq"
  show FOGe = "foge"
  show FOLe = "fole"
  show FOGt = "fogt"
  show FOLt = "folt"
  show FUEq = "fueq"
  show FUNeq = "funeq"
  show FUGe = "fuge"
  show FULe = "fule"
  show FUGt = "fugt"
  show FULt = "fult"

instance Format Unop where
  format = format . show

instance Format Binop where
  format = format . show
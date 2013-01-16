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
module IR.Common.Operators(
       Binop(..),
       Unop(..)
       ) where

import Data.Hash
import Text.Format

-- | Binary operators.  The Arithmetic operators exist for matched
-- integer and floating point types.  Subtraction always produces a
-- signed integer type.
data Binop =
    Add | AddNW | Sub | SubNW | Mul | MulNW | Div | Mod
  | And | Or | Xor | Shl | Shr | Eq | Neq | Ge | Le | Gt | Lt
  | FOEq | FONeq | FOGt | FOGe | FOLt | FOLe
  | FUEq | FUNeq | FUGt | FUGe | FULt | FULe

-- | Unary operators
data Unop = Neg | NegNW | Not

instance Hashable Unop where
  hash Neg = hashInt 1
  hash NegNW = hashInt 2
  hash Not = hashInt 3

instance Hashable Binop where
  hash Add = hashInt 1
  hash AddNW = hashInt 2
  hash Sub = hashInt 3
  hash SubNW = hashInt 4
  hash Mul = hashInt 5
  hash MulNW = hashInt 6
  hash Div = hashInt 7
  hash Mod = hashInt 8
  hash Shl = hashInt 9
  hash Shr = hashInt 10
  hash And = hashInt 11
  hash Or = hashInt 12
  hash Xor = hashInt 13
  hash Eq = hashInt 14
  hash Neq = hashInt 15
  hash Ge = hashInt 16
  hash Le = hashInt 17
  hash Gt = hashInt 18
  hash Lt = hashInt 19
  hash FOEq = hashInt 20
  hash FONeq = hashInt 21
  hash FOGe = hashInt 22
  hash FOLe = hashInt 23
  hash FOGt = hashInt 24
  hash FOLt = hashInt 25
  hash FUEq = hashInt 26
  hash FUNeq = hashInt 27
  hash FUGe = hashInt 28
  hash FULe = hashInt 29
  hash FUGt = hashInt 30
  hash FULt = hashInt 31

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
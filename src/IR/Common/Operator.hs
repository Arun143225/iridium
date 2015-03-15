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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Module defining operators common to all IR languages.
module IR.Common.Operator(
       Binop(..),
       Unop(..)
       ) where

import Data.Hashable
import Text.Format
import Text.XML.Expat.Pickle

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
  format = string . show

instance Format Binop where
  format = string . show

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Binop where
  xpickle = xpAlt fromEnum
                  [xpWrap (const Add, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "add")),
                   xpWrap (const AddNW, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "addnw")),
                   xpWrap (const Sub, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "sub")),
                   xpWrap (const SubNW, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "subnw")),
                   xpWrap (const Mul, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "mul")),
                   xpWrap (const MulNW, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "mulnw")),
                   xpWrap (const Div, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "div")),
                   xpWrap (const Mod, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "mod")),
                   xpWrap (const Shl, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "shl")),
                   xpWrap (const Shr, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "shr")),
                   xpWrap (const And, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "And")),
                   xpWrap (const Or, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "or")),
                   xpWrap (const Xor, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "xor")),
                   xpWrap (const Eq, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "eq")),
                   xpWrap (const Neq, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "neq")),
                   xpWrap (const Ge, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "ge")),
                   xpWrap (const Le, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "le")),
                   xpWrap (const Gt, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "gt")),
                   xpWrap (const Lt, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "lt")),
                   xpWrap (const FOEq, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "foeq")),
                   xpWrap (const FONeq, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "foneq")),
                   xpWrap (const FOGe, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "foge")),
                   xpWrap (const FOLe, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "fole")),
                   xpWrap (const FOGt, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "fogt")),
                   xpWrap (const FOLt, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "folt")),
                   xpWrap (const FUEq, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "fueq")),
                   xpWrap (const FUNeq, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "funeq")),
                   xpWrap (const FUGe, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "fuge")),
                   xpWrap (const FULe, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "fule")),
                   xpWrap (const FUGt, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "fugt")),
                   xpWrap (const FULt, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "fult"))]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Unop where
  xpickle = xpAlt fromEnum
                  [xpWrap (const Neg, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "neg")),
                   xpWrap (const NegNW, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "negnw")),
                   xpWrap (const Not, const ())
                          (xpAttrFixed (gxFromString "op")
                                       (gxFromString "not"))]

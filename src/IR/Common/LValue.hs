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
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module IR.Common.LValue(
       LValue(..)
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
import Text.XML.Expat.Pickle hiding (Node)
import Text.XML.Expat.Tree(NodeG)

-- | An assignable value.
data LValue exp =
  -- | An array (or pointer) index
    Index {
      -- | The indexed value.  Must be an array.
      idxVal :: exp,
      -- | The index value.  Must be an integer type.
      idxIndex :: exp,
      -- | The position in source from which this arises.
      idxPos :: !Position
    }
  -- | A field in a structure
  | Field {
      -- | The value whose field is being accessed.  Must be a
      -- structure type.
      fieldVal :: exp,
      -- | The name of the field being accessed.
      fieldName :: !Fieldname,
      -- | The position in source from which this arises.
      fieldPos :: !Position
    }
  -- | A form of a variant
  | Form {
      -- | The value whose field is being accessed.  Must be a
      -- structure type.
      formVal :: exp,
      -- | The name of the field being accessed.
      formName :: !Variantname,
      -- | The position in source from which this arises.
      formPos :: !Position
    }
  -- | Dereference a pointer
  | Deref {
      -- | The value being dereferenced.  Must be a pointer type.
      derefVal :: exp,
      -- | The position in source from which this arises.
      derefPos :: !Position
    }
  -- | A local value (local variable or argument)
  | Var {
      -- | The name of the local value.
      varName :: !Id,
      -- | The position in source from which this arises.
      varPos :: !Position
    }
  -- | A global value (global variable or function)
  | Global {
      -- | The name of the global value.
      globalName :: !Globalname,
      -- | The position in source from which this arises.
      globalPos :: !Position
    }

instance Eq1 LValue where
  Index { idxVal = val1, idxIndex = idx1 } ==#
    Index { idxVal = val2, idxIndex = idx2 } = val1 == val2 && idx1 == idx2
  Field { fieldVal = val1, fieldName = name1 } ==#
    Field { fieldVal = val2, fieldName = name2 } =
      val1 == val2 && name1 == name2
  Form { formVal = val1, formName = name1 } ==#
    Form { formVal = val2, formName = name2 } =
    val1 == val2 && name1 == name2
  Deref { derefVal = val1 } ==# Deref { derefVal = val2 } = val1 == val2
  Var { varName = name1 } ==# Var { varName = name2 } = name1 == name2
  Global { globalName = name1 } ==# Global { globalName = name2 } =
    name1 == name2
  _ ==# _ = False

instance Eq elem => Eq (LValue elem) where (==) = (==#)

instance Ord1 LValue where
  compare1 Index { idxVal = val1, idxIndex = idx1 }
          Index { idxVal = val2, idxIndex = idx2 } =
    case compare idx1 idx2 of
      EQ -> compare val1 val2
      out -> out
  compare1 Index {} _ = LT
  compare1 _ Index {} = GT
  compare1 Field { fieldVal = val1, fieldName = name1 }
          Field { fieldVal = val2, fieldName = name2 } =
    case compare name1 name2 of
      EQ -> compare val1 val2
      out -> out
  compare1 Field {} _ = LT
  compare1 _ Field {} = GT
  compare1 Form { formVal = val1, formName = name1 }
          Form { formVal = val2, formName = name2 } =
    case compare name1 name2 of
      EQ -> compare val1 val2
      out -> out
  compare1 Form {} _ = LT
  compare1 _ Form {} = GT
  compare1 Deref { derefVal = val1 } Deref { derefVal = val2 } =
    compare val1 val2
  compare1 Deref {} _ = LT
  compare1 _ Deref {} = GT
  compare1 Var { varName = name1 } Var { varName = name2 } = compare name1 name2
  compare1 Var {} _ = LT
  compare1 _ Var {} = GT
  compare1 Global { globalName = name1 } Global { globalName = name2 } =
    compare name1 name2

instance Ord elem => Ord (LValue elem) where compare = compare1

instance Hashable1 LValue where
  hashWithSalt1 s Index { idxVal = val, idxIndex = idx } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` val `hashWithSalt` idx
  hashWithSalt1 s Field { fieldVal = val, fieldName = name } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` val `hashWithSalt` name
  hashWithSalt1 s Form { formVal = val, formName = name } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` val `hashWithSalt` name
  hashWithSalt1 s Deref { derefVal = val } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt`val
  hashWithSalt1 s Var { varName = name } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` name
  hashWithSalt1 s Global { globalName = name } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt` name

instance Hashable elem => Hashable (LValue elem) where
  hashWithSalt = hashWithSalt1

instance RenameType Typename exp => RenameType Typename (LValue exp) where
  renameType f lval @ Index { idxVal = inner } =
    lval { idxVal = renameType f inner }
  renameType f lval @ Field { fieldVal = inner } =
    lval { fieldVal = renameType f inner }
  renameType f lval @ Form { formVal = inner } =
    lval { formVal = renameType f inner }
  renameType f lval @ Deref { derefVal = inner } =
    lval { derefVal = renameType f inner }
  renameType _ lval = lval

instance Rename Id exp => Rename Id (LValue exp) where
  rename f lval @ Index { idxVal = inner } = lval { idxVal = rename f inner }
  rename f lval @ Field { fieldVal = inner } =
    lval { fieldVal = rename f inner }
  rename f lval @ Form { formVal = inner } =
    lval { formVal = rename f inner }
  rename f lval @ Deref { derefVal = inner } =
    lval { derefVal = rename f inner }
  rename f lval @ Var { varName = name } = lval { varName = f name }
  rename _ lval = lval

instance Format exp => Format (LValue exp) where
  format Index { idxVal = e, idxIndex = i } = format e <+> brackets (format i)
  format Form { formVal = e, formName = form } =
    format e <> char '#' <> format form
  format Field { fieldVal = e, fieldName = field } =
    format e <> dot <> format field
  format Deref { derefVal = e } = string "*" <+> format e
  format Global { globalName = g } = format g
  format Var { varName = v } = format v

instance FormatM m exp => FormatM m (LValue exp) where
  formatM Index { idxVal = e, idxIndex = i } =
    do
      edoc <- formatM e
      idoc <- formatM i
      return $! edoc <+> brackets idoc
  formatM Field { fieldVal = e, fieldName = field } =
    do
      edoc <- formatM e
      return $! edoc <> dot <> format field
  formatM Form { formVal = e, formName = form } =
    do
      edoc <- formatM e
      return $! edoc <> char '#' <> format form
  formatM Deref { derefVal = e } =
    do
      edoc <- formatM e
      return $! string "*" <+> edoc
  formatM Global { globalName = g } = return $! format g
  formatM Var { varName = v } = return $! format v

indexPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [NodeG [] tag text] exp) =>
                PU [NodeG [] tag text] (LValue exp)
indexPickler =
  let
    revfunc Index { idxVal = val, idxIndex = idx, idxPos = pos } =
      (pos, (val, idx))
    revfunc _ = error "can't convert"
  in
    xpWrap (\(pos, (val, idx)) -> Index { idxVal = val, idxIndex = idx,
                                          idxPos = pos }, revfunc)
           (xpElem (gxFromString "Index") xpickle (xpPair xpickle xpickle))

fieldPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [NodeG [] tag text] exp) =>
                PU [NodeG [] tag text] (LValue exp)
fieldPickler =
  let
    revfunc Field { fieldVal = val, fieldName = name, fieldPos = pos } =
      ((name, pos), val)
    revfunc _ = error "can't convert"
  in
    xpWrap (\((name, pos), val) -> Field { fieldVal = val, fieldName = name,
                                           fieldPos = pos }, revfunc)
           (xpElem (gxFromString "Index") (xpPair xpickle xpickle) xpickle)

formPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [NodeG [] tag text] exp) =>
                PU [NodeG [] tag text] (LValue exp)
formPickler =
  let
    revfunc Form { formVal = val, formName = name, formPos = pos } =
      ((name, pos), val)
    revfunc _ = error "can't convert"
  in
    xpWrap (\((name, pos), val) -> Form { formVal = val, formName = name,
                                          formPos = pos }, revfunc)
           (xpElem (gxFromString "Index") (xpPair xpickle xpickle) xpickle)

derefPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text,
                 XmlPickler [NodeG [] tag text] exp) =>
                PU [NodeG [] tag text] (LValue exp)
derefPickler =
  let
    revfunc Deref { derefVal = val, derefPos = pos } = (pos, val)
    revfunc _ = error "can't convert"
  in
    xpWrap (\(pos, val) -> Deref { derefVal = val, derefPos = pos }, revfunc)
           (xpElem (gxFromString "Deref") xpickle xpickle)

varPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] (LValue exp)
varPickler =
  let
    revfunc Var { varName = name, varPos = pos } = (name, pos)
    revfunc _ = error "can't convert"
  in
    xpWrap (\(name, pos) -> Var { varName = name, varPos = pos }, revfunc)
           (xpElemAttrs (gxFromString "Var") (xpPair xpickle xpickle))

globalPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] (LValue exp)
globalPickler =
  let
    revfunc Global { globalName = name, globalPos = pos } = (name, pos)
    revfunc _ = error "can't convert"
  in
    xpWrap (\(name, pos) -> Global { globalName = name, globalPos = pos },
            revfunc)
           (xpElemAttrs (gxFromString "Global") (xpPair xpickle xpickle))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] exp) =>
         XmlPickler [NodeG [] tag text] (LValue exp) where
  xpickle =
    let
      picker Index {} = 0
      picker Field {} = 1
      picker Form {} = 2
      picker Deref {} = 3
      picker Var {} = 4
      picker Global {} = 5
    in
      xpAlt picker [indexPickler, fieldPickler, formPickler,
                    derefPickler, varPickler, globalPickler]

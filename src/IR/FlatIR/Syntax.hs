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

-- | This module defines the FlatIR language.
--
-- FlatIR is a simply-typed flat-scoped intermediate language.  It
-- is designed to be reasonably close to LLVM, with instructions
-- similar to LLVM's, but without some of the difficulties of LLVM.
--
-- At the moment, the FlatIR language is under revision, and will
-- probably change quite a bit.
--
-- Things that need to be done:
--   * Add notions of vtables and lookups to the language
--   * Add variant types
--   * Redesign/modify certain instructions (Deref, Call, Cast, Alloc)
--   * Add exception handling
module IR.FlatIR.Syntax(
       -- * Indexes
       GCHeader(..),
       Id(..),
       Label(..),
       Fieldname(..),
       Typename(..),
       Globalname(..),

       -- * Operators and options
       Binop(..),
       Unop(..),

       -- * Core language

       -- ** Types
       Type(..),
       Ptr(..),
       Mutability(..),
       Mobility(..),
       PtrClass(..),

       -- ** Execution
       Exp(..),
       LValue(..),
       Stm(..),
       Bind(..),
       Transfer(..),

       -- ** Definitions
       DeclNames(..),
       Block(..),
       Body(..),
       Global(..),
       Module(..),
       StmModule,
       SSAModule,

       -- ** Utilities
       renameType
       ) where

import Data.Array
import Data.Graph.Inductive.Graph(Node)
--import Data.Graph.Inductive.Query.DFS
import Data.Functor
import Data.Hashable
import Data.Hashable.Extras
import Data.Hash.ExtraInstances()
import Data.Map(Map)
import Data.Maybe
import Data.Interval(Intervals)
import Data.Pos
import Data.Word
import IR.Common.Names
import IR.Common.Ptr
import IR.Common.Operator
import IR.Common.Rename
import IR.Common.RenameType
import Prelude hiding (head)
import Prelude.Extras
import Text.Format

-- FlatIR is a simply-typed IR intended to be close to LLVM, but not
-- in SSA form.  It is intended primarily as a jumping-off point for
-- other languages targeting LLVM.

-- Programs in FlatIR are equipped with very detailed information
-- about garbage collection.  This is passed through to LLVM in the
-- form of metadata.

-- FlatIR also contains a virtual call abstraction, which allows
-- polymorphic languages to compile to FlatIR without having to
-- monomorphise everything. XXX IMPLEMENT THIS

-- FlatIR will eventually contain transaction information.

-- In general, any optimization pass that would be written for
-- FlatIR should instead be written for LLVM, unless there is a very
-- compelling reason for it.  Examples would be optimizations that
-- deal with GC or virtual calls (or eventually transactions).

-- | Types.  Types are monomorphic, and correspond roughly with LLVM
-- types. XXX add a variant type
data Type =
  -- | A function type
    FuncType {
      -- | The return type of the function.
      funcTyRetTy :: Type,
      -- | The types of the arguments.
      funcTyArgTys :: [Type],
      -- | The position in source from which this arises.
      funcTyPos :: !Pos
    }
  -- | A structure, representing both tuples and records
  | StructType {
      -- | Whether or not the layout is strict.
      structPacked :: !Bool,
      -- | The fields of the struct.
      structFields :: Array Fieldname (String, Mutability, Type),
      -- | The position in source from which this arises.
      structPos :: !Pos
    }
  -- | A variant, representing both tuples and records
  | VariantType {
      -- | The fields of the struct.
      variantForms :: Array Variantname (String, Mutability, Type),
      -- | The position in source from which this arises.
      variantPos :: !Pos
    }
  -- | An array.  Unlike LLVM arrays, these may be variable-sized
  | ArrayType {
      -- | The length of the array, if known.
      arrayLen :: !(Maybe Word),
      -- | The type of array elements.
      arrayElemTy :: Type,
      -- | The position in source from which this arises.
      arrayPos :: !Pos
    }
  -- | Pointers, both native and GC
  | PtrType {
      -- | The pointer information
      ptrTy :: !(Ptr GCHeader Type),
      -- | The position in source from which this arises.
      ptrPos :: !Pos
    }
  -- | An integer, possibly signed, with a size.
  | IntType {
      -- | Whether or not the int is signed.
      intSigned :: !Bool,
      -- | The size of the int in bits.
      intSize :: !Word,
      -- | The possible-value intervals for the integer.
      intIntervals :: Intervals Integer,
      -- | The position in source from which this arises.
      intPos :: !Pos
    }
  -- | A defined type
  | IdType {
      -- | The name for this type.
      idName :: !Typename,
      -- | The position in source from which this arises.
      idPos :: !Pos
    }
  -- | Floating point types
  | FloatType {
      -- | The size of the float in bits.
      floatSize :: !Word,
      -- | The position in source from which this arises.
      floatPos :: !Pos
    }
  -- | The unit type, equivalent to SML unit and C/Java void
  | UnitType !Pos

-- | An expression
data Exp =
  -- | Allocate an object whose type is described by the given header.
  -- XXX probably replace this with a general Alloc instruction,
  -- represeting GC allocation, malloc, and alloca.
    GCAlloc !GCHeader (Maybe Exp) (Maybe Exp)
  -- | A binary operation
  | Binop {
      -- | The operator.
      binopOp :: !Binop,
      -- | The left hand side.
      binopLeft :: Exp,
      -- | The right hand side.
      binopRight :: Exp,
      -- | The position in source from which this arises.
      binopPos :: !Pos
    }
  -- | Call a function.
  | Call {
      -- | The function being called.  Must be a function value.
      callFunc :: Exp,
      -- | The arguments to the function.
      callArgs :: [Exp],
      -- | The position in source from which this arises.
      callPos :: !Pos
    }
  -- | A unary operation
  | Unop {
      -- | The operator.
      unopOp :: !Unop,
      -- | The operand.
      unopVal ::  Exp,
      -- | The position in source from which this arises.
      unopPos :: !Pos
    }
  -- | A conversion from one type to another.
  | Conv {
      -- | The type to which the value is being converted.
      convTy :: Type,
      -- | The value being converted.
      convVal :: Exp,
      -- | The position in source from which this arises.
      convPos :: !Pos
    }
  -- | Treat an expression as if it were the given type regardless of
  -- its actual type.
  | Cast {
      -- | The type to which the value is being cast.
      castTy :: Type,
      -- | The value being cast.
      castVal :: Exp,
      -- | The position in source from which this arises.
      castPos :: !Pos
    }
  -- | Address of an LValue
  | AddrOf {
      -- | The value having its address taken.
      addrofVal :: LValue,
      -- | The position in source from which this arises.
      addrofPos :: !Pos
    }
  -- | A structure literal
  | StructLit {
      -- | The literal's type, must be a struct type.
      structLitTy :: Type,
      -- | The constant's field values
      structLitFields :: Array Fieldname Exp,
      -- | The position in source from which this arises.
      structLitPos :: !Pos
    }
  -- | A variant literal
  | VariantLit {
      -- | The literal's type, must be a variant type.
      variantLitTy :: Type,
      -- | The literal's form.
      variantLitForm :: !Variantname,
      -- | The literal's inner value.
      variantLitVal :: Exp,
      -- | The position in source from which this arises.
      variantLitPos :: !Pos
    }
  -- | An array literal
  | ArrayLit {
      -- | The constant's type, must be an array type.
      arrayLitTy :: Type,
      -- | The constant's values
      arrayLitVals :: [Exp],
      -- | The position in source from which this arises.
      arrayLitPos :: !Pos
    }
  -- | A numerical constant with a given size and signedness XXX add a
  -- floating point constant.
  | NumLit {
      -- | The constant's type, must be an integer or float type.
      numLitTy :: Type,
      -- | The constant's value
      numLitVal :: !Integer,
      -- | The position in source from which this arises.
      numLitPos :: !Pos
    }
  -- | An LValue
  | LValue !LValue

-- | A global value.  Represents a global variable or a function.
data Global elem gr =
  -- | A function
    Function {
      -- | Name of the function
      funcName :: !DeclNames,
      -- | Return type
      funcRetTy :: Type,
      -- | A map from identifiers for arguments and local variables to
      -- their types.
      funcValTys :: Array Id Type,
      -- | A list of the identifiers representing arguments
      funcParams :: [Id],
      -- | The function's body, if it has one
      funcBody :: Maybe (Body elem gr),
      -- | The position in source from which this arises.
      funcPos :: !Pos
    }
  -- | A global variable
  | GlobalVar {
      -- | The name of the variable.
      gvarName :: !DeclNames,
      -- | The type of the variable.
      gvarTy :: Type,
      -- | The initializer.
      gvarInit :: Maybe Exp,
      -- | The variable's mutability.
      gvarMutability :: Mutability,
      -- | The position in source from which this arises.
      gvarPos :: !Pos
    }

-- | A module.  Represents a concept similar to an LLVM module.
data Module elem gr =
    Module {
      -- | Name of the module
      modName :: !String,
      -- | A map from typenames to their proper names and possibly their
      -- definitions
      modTypes :: Array Typename (String, Maybe Type),
      -- | A map from GCHeaders to their definitions
      modGCHeaders :: Array GCHeader (Typename, Mobility, Mutability),
      -- | Generated GC types (this module will generate the signatures
      -- and accessors)
      modGenGCs :: [GCHeader],
      -- | A map from global names to the corresponding definitions
      modGlobals :: Array Globalname (Global elem gr),
      -- | The position in source from which this arises.  This is here
      -- solely to record filenames in a unified way.
      modPos :: !Pos
    }

type StmModule = Module Stm
type SSAModule = Module Bind

instance Eq Type where
  FuncType { funcTyRetTy = retty1, funcTyArgTys = params1 } ==
    FuncType { funcTyRetTy = retty2, funcTyArgTys = params2 } =
    retty1 == retty2 && params1 == params2
  StructType { structPacked = packed1, structFields = fields1 } ==
    StructType { structPacked = packed2, structFields = fields2 } =
    packed1 == packed2 && fields1 == fields2
  VariantType { variantForms = forms1 } ==
    VariantType { variantForms = forms2 } =
    forms1 == forms2
  ArrayType { arrayLen = len1, arrayElemTy = inner1 } ==
    ArrayType { arrayLen = len2, arrayElemTy = inner2 } =
    len1 == len2 && inner1 == inner2
  PtrType { ptrTy = objtype1 } == PtrType { ptrTy = objtype2 } =
    objtype1 == objtype2
  IntType { intSigned = signed1, intIntervals = intervals1, intSize = size1 } ==
    IntType { intSigned = signed2, intIntervals = intervals2, intSize = size2 } =
    signed1 == signed2 && size1 == size2 && intervals1 == intervals2
  IdType { idName = name1 } == IdType { idName = name2 } = name1 == name2
  FloatType { floatSize = size1 } == FloatType { floatSize = size2 } =
    size1 == size2
  (UnitType _) == (UnitType _) = True
  _ == _ = False

instance Eq Exp where
  Binop { binopOp = op1, binopLeft = left1, binopRight = right1 } ==
    Binop { binopOp = op2, binopLeft = left2, binopRight = right2 } =
    op1 == op2 && left1 == left2 && right1 == right2
  Call { callFunc = func1, callArgs = args1 } ==
    Call { callFunc = func2, callArgs = args2 } =
    func1 == func2 && args1 == args2
  Unop { unopOp = op1, unopVal = val1 } ==
    Unop { unopOp = op2, unopVal = val2 } =
    op1 == op2 && val1 == val2
  Conv { convTy = ty1, convVal = val1 } ==
    Conv { convTy = ty2, convVal = val2 } =
    ty1 == ty2 && val1 == val2
  Cast { castTy = ty1, castVal = val1 } ==
    Cast { castTy = ty2, castVal = val2 } =
    ty1 == ty2 && val1 == val2
  AddrOf { addrofVal = val1 } == AddrOf { addrofVal = val2 } = val1 == val2
  StructLit { structLitTy = ty1, structLitFields = fields1 } ==
    StructLit { structLitTy = ty2, structLitFields = fields2 } =
    ty1 == ty2 && fields1 == fields2
  VariantLit { variantLitTy = ty1, variantLitForm = form1,
               variantLitVal = val1 } ==
    VariantLit { variantLitTy = ty2, variantLitForm = form2,
                 variantLitVal = val2 } =
    form1 == form2 && ty1 == ty2 && val1 == val2
  ArrayLit { arrayLitTy = ty1, arrayLitVals = vals1 } ==
    ArrayLit { arrayLitTy = ty2, arrayLitVals = vals2 } =
    ty1 == ty2 && vals1 == vals2
  NumLit { numLitTy = ty1, numLitVal = val1 } ==
    NumLit { numLitTy = ty2, numLitVal = val2 } =
    ty1 == ty2 && val1 == val2
  (LValue lval1) == (LValue lval2) = lval1 == lval2
  _ == _ = False

instance Ord Type where
  compare FuncType { funcTyRetTy = retty1, funcTyArgTys = params1 }
          FuncType { funcTyRetTy = retty2, funcTyArgTys = params2 } =
    case compare retty1 retty2 of
      EQ -> compare params1 params2
      out -> out
  compare FuncType {} _ = LT
  compare _ FuncType {} = GT
  compare StructType { structPacked = packed1, structFields = fields1 }
          StructType { structPacked = packed2, structFields = fields2 } =
    case compare packed1 packed2 of
      EQ -> compare fields1 fields2
      out -> out
  compare StructType {} _ = LT
  compare _ StructType {} = GT
  compare VariantType { variantForms = forms1 }
          VariantType { variantForms = forms2 } =
    compare forms1 forms2
  compare VariantType {} _ = LT
  compare _ VariantType {} = GT
  compare ArrayType { arrayLen = len1, arrayElemTy = inner1 }
          ArrayType { arrayLen = len2, arrayElemTy = inner2 } =
    case compare len1 len2 of
      EQ -> compare inner1 inner2
      out -> out
  compare ArrayType {} _ = LT
  compare _ ArrayType {} = GT
  compare PtrType { ptrTy = objtype1 } PtrType { ptrTy = objtype2 } =
    compare objtype1 objtype2
  compare PtrType {} _ = LT
  compare _ PtrType {} = GT
  compare IntType { intSigned = signed1, intSize = size1,
                    intIntervals = intervals1 }
          IntType { intSigned = signed2, intSize = size2,
                    intIntervals = intervals2 } =
    case compare signed1 signed2 of
      EQ -> case compare size1 size2 of
        EQ -> compare intervals1 intervals2
        out -> out
      out -> out
  compare IntType {} _ = LT
  compare _ IntType {} = GT
  compare IdType { idName = name1 } IdType { idName = name2 } =
    compare name1 name2
  compare IdType {} _ = LT
  compare _ IdType {} = GT
  compare FloatType { floatSize = size1 } FloatType { floatSize = size2 } =
    compare size1 size2
  compare FloatType {} _ = LT
  compare _ FloatType {} = GT
  compare (UnitType _) (UnitType _) = EQ

instance Ord Exp where
  compare (GCAlloc _ _ _) _ = error "GCAlloc is going away"
  compare _ (GCAlloc _ _ _) = error "GCAlloc is going away"
  compare Binop { binopOp = op1, binopLeft = left1, binopRight = right1 }
          Binop { binopOp = op2, binopLeft = left2, binopRight = right2 } =
    case compare op1 op2 of
      EQ -> case compare left1 left2 of
        EQ ->  compare right1 right2
        out -> out
      out -> out
  compare Binop {} _ = LT
  compare _ Binop {} = GT
  compare Call { callFunc = func1, callArgs = args1 }
          Call { callFunc = func2, callArgs = args2 } =
    case compare func1 func2 of
      EQ -> compare args1 args2
      out -> out
  compare Call {} _ = LT
  compare _ Call {} = GT
  compare Unop { unopOp = op1, unopVal = val1 }
          Unop { unopOp = op2, unopVal = val2 } =
    case compare op1 op2 of
      EQ -> compare val1 val2
      out -> out
  compare Unop {} _ = LT
  compare _ Unop {} = GT
  compare Conv { convTy = ty1, convVal = val1 }
          Conv { convTy = ty2, convVal = val2 } =
    case compare ty1 ty2 of
      EQ -> compare val1 val2
      out -> out
  compare Conv {} _ = LT
  compare _ Conv {} = GT
  compare Cast { castTy = ty1, castVal = val1 }
          Cast { castTy = ty2, castVal = val2 } =
    case compare ty1 ty2 of
      EQ -> compare val1 val2
      out -> out
  compare Cast {} _ = LT
  compare _ Cast {} = GT
  compare AddrOf { addrofVal = val1 } AddrOf { addrofVal = val2 } =
    compare val1 val2
  compare AddrOf {} _ = LT
  compare _ AddrOf {} = GT
  compare StructLit { structLitTy = ty1, structLitFields = fields1 }
          StructLit { structLitTy = ty2, structLitFields = fields2 } =
    case compare ty1 ty2 of
      EQ -> compare fields1 fields2
      out -> out
  compare StructLit {} _ = LT
  compare _ StructLit {} = GT
  compare VariantLit { variantLitTy = ty1, variantLitForm = form1,
                       variantLitVal = val1 }
          VariantLit { variantLitTy = ty2, variantLitForm = form2,
                       variantLitVal = val2 } =
    case compare form1 form2 of
      EQ -> case compare ty1 ty2 of
        EQ -> compare val1 val2
        out -> out
      out -> out
  compare VariantLit {} _ = LT
  compare _ VariantLit {} = GT
  compare ArrayLit { arrayLitTy = ty1, arrayLitVals = vals1 }
          ArrayLit { arrayLitTy = ty2, arrayLitVals = vals2 } =
    case compare ty1 ty2 of
      EQ -> compare vals1 vals2
      out -> out
  compare ArrayLit {} _ = LT
  compare _ ArrayLit {} = GT
  compare NumLit { numLitTy = ty1, numLitVal = val1 }
          NumLit { numLitTy = ty2, numLitVal = val2 } =
    case compare ty1 ty2 of
      EQ -> compare val1 val2
      out -> out
  compare NumLit {} _ = LT
  compare _ NumLit {} = GT
  compare (LValue lval1) (LValue lval2) = compare lval1 lval2

instance Hashable Type where
  hashWithSalt s FuncType { funcTyRetTy = retty, funcTyArgTys = params } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt` retty `hashWithSalt` params
  hashWithSalt s StructType { structPacked = packed, structFields = fields } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt`
      packed `hashWithSalt` (elems fields)
  hashWithSalt s VariantType { variantForms = forms } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` (elems forms)
  hashWithSalt s ArrayType { arrayLen = Nothing, arrayElemTy = inner } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` (0 :: Word) `hashWithSalt` inner
  hashWithSalt s ArrayType { arrayLen = Just size, arrayElemTy = inner } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` size `hashWithSalt` inner
  hashWithSalt s PtrType { ptrTy = objtype } =
    s `hashWithSalt` (4 :: Word) `hashWithSalt` objtype
  hashWithSalt s IntType { intSigned = signed, intIntervals = intervals,
                           intSize = size } =
    s `hashWithSalt` (5 :: Word) `hashWithSalt` signed `hashWithSalt`
      intervals `hashWithSalt` size
  hashWithSalt s IdType { idName = Typename str } =
    s `hashWithSalt` (6 :: Word) `hashWithSalt` str
  hashWithSalt s FloatType { floatSize = size } =
    s `hashWithSalt` (7 :: Word) `hashWithSalt` size
  hashWithSalt s UnitType {} = s `hashWithSalt` (7 :: Word)

instance Hashable Exp where
  hashWithSalt s Binop { binopOp = op, binopLeft = left, binopRight = right } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt`
    op `hashWithSalt` left `hashWithSalt` right
  hashWithSalt s Call { callFunc = func, callArgs = args } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` func `hashWithSalt` args
  hashWithSalt s Unop { unopOp = op, unopVal = val } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` op `hashWithSalt` val
  hashWithSalt s Conv { convTy = ty, convVal = val } =
    s `hashWithSalt` (4 :: Word) `hashWithSalt` ty `hashWithSalt` val
  hashWithSalt s Cast { castTy = ty, castVal = val } =
    s `hashWithSalt` (5 :: Word) `hashWithSalt` ty `hashWithSalt` val
  hashWithSalt s AddrOf { addrofVal = val } =
    s `hashWithSalt` (6 :: Word) `hashWithSalt` val
  hashWithSalt s StructLit { structLitTy = ty, structLitFields = fields } =
    s `hashWithSalt` (7 :: Word) `hashWithSalt` ty `hashWithSalt` (elems fields)
  hashWithSalt s VariantLit { variantLitTy = ty, variantLitForm = form,
                              variantLitVal = val } =
    s `hashWithSalt` (8 :: Word) `hashWithSalt`
    form `hashWithSalt` ty `hashWithSalt` val
  hashWithSalt s ArrayLit { arrayLitTy = ty, arrayLitVals = vals } =
    s `hashWithSalt` (9 :: Word) `hashWithSalt` ty `hashWithSalt` vals
  hashWithSalt s NumLit { numLitTy = ty, numLitVal = val } =
    s `hashWithSalt` (10 :: Word) `hashWithSalt` ty `hashWithSalt` val
  hashWithSalt s (LValue lval) =
    s `hashWithSalt` (11 :: Word) `hashWithSalt` lval
  hashWithSalt _ (GCAlloc _ _ _) = error "GCAlloc is going away"

instance RenameType Typename Type where
  renameType f ty @ FuncType { funcTyRetTy = retty, funcTyArgTys = argtys } =
    ty { funcTyArgTys = renameType f argtys, funcTyRetTy = renameType f retty }
  renameType f ty @ StructType { structFields = fields } =
    ty { structFields = fmap (\(n, m, t) -> (n, m, renameType f t)) fields }
  renameType f ty @ VariantType { variantForms = forms } =
    ty { variantForms = fmap (\(n, m, t) -> (n, m, renameType f t)) forms }
  renameType f ty @ ArrayType { arrayElemTy = elemty } =
    ty { arrayElemTy = renameType f elemty }
  renameType f ty @ PtrType { ptrTy = inner } =
    ty { ptrTy = renameType f inner }
  renameType f ty @ IdType { idName = name } = ty { idName = f name }
  renameType _ ty = ty

instance RenameType Typename Exp where
  renameType f e @ Binop { binopLeft = left, binopRight = right } =
    e { binopLeft = renameType f left, binopRight = renameType f right }
  renameType f e @ Call { callFunc = func, callArgs = args } =
    e { callFunc = renameType f func, callArgs = renameType f args }
  renameType f e @ Conv { convTy = ty, convVal = val } =
    e { convTy = renameType f ty, convVal = renameType f val }
  renameType f e @ Cast { castTy = ty, castVal = val } =
    e { castTy = renameType f ty, castVal = renameType f val }
  renameType f e @ Unop { unopVal = val } = e { unopVal = renameType f val }
  renameType f e @ AddrOf { addrofVal = val } =
    e { addrofVal = renameType f val }
  renameType f e @ StructLit { structLitFields = fields, structLitTy = ty } =
    e { structLitFields = renameType f fields, structLitTy = renameType f ty }
  renameType f e @ VariantLit { variantLitVal = val, variantLitTy = ty } =
    e { variantLitVal = renameType f val, variantLitTy = renameType f ty }
  renameType f e @ ArrayLit { arrayLitVals = vals, arrayLitTy = ty } =
    e { arrayLitVals = renameType f vals, arrayLitTy = renameType f ty }
  renameType f e @ NumLit { numLitTy = ty } =
    e { numLitTy = renameType f ty }
  renameType f (LValue l) = LValue (renameType f l)
  renameType _ e = e

instance Rename Id Exp where
  rename f e @ Binop { binopLeft = left, binopRight = right } =
    e { binopLeft = rename f left, binopRight = rename f right }
  rename f e @ Call { callFunc = func, callArgs = args } =
    e { callFunc = rename f func, callArgs = rename f args }
  rename f e @ Conv { convVal = val } = e { convVal = rename f val }
  rename f e @ Cast { castVal = val } = e { castVal = rename f val }
  rename f e @ Unop { unopVal = val } = e { unopVal = rename f val }
  rename f e @ AddrOf { addrofVal = val } = e { addrofVal = rename f val }
  rename f e @ StructLit { structLitFields = fields } =
    e { structLitFields = rename f fields }
  rename f e @ VariantLit { variantLitVal = val } =
    e { variantLitVal = rename f val }
  rename f e @ ArrayLit { arrayLitVals = vals } =
    e { arrayLitVals = rename f vals }
  rename f (LValue l) = LValue (rename f l)
  rename _ e = e

{-
-- This mess is a good example of what I mean about format and a
-- naming function.

instance Graph gr => Format (Module gr) where
  format (Module { modName = name, modTypes = types, modGlobals = globals,
                   modGCHeaders = gcheaders, modGenGCs = gcgen }) =
    let
      -- These functions here cause a heap of trouble.  We want to
      -- look up actual names, so they have to get moved inside the
      -- format module call.  This propogates downward and winds up
      -- dragging almost everything inside.
      formatTypename :: Typename -> Doc
      formatTypename ty = "%" <> fst (types ! ty)

      formatGCHeader :: GCHeader -> Doc
      formatGCHeader hdr =
        let
          (ty, mut, mob) = gcheaders ! hdr
        in
          mut <+> mob <+> fst (types ! ty)

      formatGlobalname :: Globalname -> Doc
      formatGlobalname fname = "@" <>
        (case globals ! fname of
          Function { funcName = funcname } -> funcname
          GlobalVar { gvarName = gvarname } -> gvarname)

      formatType :: Type -> Doc
      formatType (FuncType retty params) =
        parenList (formatType retty) (map formatType params)
      formatType (StructType packed fields) =
        let
          mapfun (str, mut, ty) =
            mut <+> str <+> colon <+> formatType ty
          fielddocs =
            nest 2 (sep (punctuate comma (map mapfun (elems fields))))
        in
          if packed
            then sep [format "<{", fielddocs, format "}>"]
            else sep [lbrace, fielddocs, rbrace ]
      formatType (PtrType (Native inner)) = formatType inner <> "*"
      formatType (PtrType (GC ptrclass hdr)) =
        ptrclass <+> formatGCHeader hdr
      formatType (ArrayType (Just size) inner) =
        formatType inner <> brackets size
      formatType (ArrayType Nothing inner) = formatType inner <> "[]"
      formatType (IntType True size) = "i" <> size
      formatType (IntType False size) = "ui" <> size
      formatType (IdType i) = formatTypename i
      formatType (FloatType size) = format "f" <> size
      formatType UnitType = format "unit"

      formatExp (Call f args) =
        parenList (formatExp f) (map formatExp args)
      formatExp (GCAlloc header Nothing Nothing) =
        "gcalloc" <+> formatGCHeader header
      formatExp (GCAlloc header (Just size) Nothing) =
        "gcalloc" <+> formatGCHeader header <+> brackets (formatExp size)
      formatExp (GCAlloc header Nothing (Just gen)) =
        "gcalloc" <+> formatGCHeader header <+> "gen" <+> formatExp gen
      formatExp (GCAlloc header (Just size) (Just gen)) =
        "gcalloc" <+> formatGCHeader header <+>
          brackets (formatExp size) <+> "gen" <+> formatExp gen
      formatExp (Binop op l r) =
        parens (sep [ format op,  formatExp l <> comma, formatExp r ])
      formatExp (Unop op e) = parens (hang (format op) 2 (formatExp e))
      formatExp (Conv ty inner) =
        parens (sep [ format "conv", formatExp inner,
                      format "to", formatType ty ])
      formatExp (Cast ty inner) =
        parens (sep [ format "cast", formatExp inner,
                      format "to", formatType ty ])
      formatExp (AddrOf l) = "addrof" <+> formatLValue l
      formatExp (LValue l) = formatLValue l
      formatExp (StructLit ty fields) =
        let
          headerdoc = "const" <+> formatType ty
        in
          braceBlock headerdoc (punctuate comma (map formatExp (elems fields)))
      formatExp (ArrayLit ty inits) =
        let
          headerdoc = "const" <+> formatType ty
        in
          braceBlock headerdoc (punctuate comma (map formatExp inits))
      formatExp (NumLit ty n) = hang (formatType ty) 2 (format n)

      formatLValue :: LValue -> Doc
      formatLValue (Deref e) = "*" <+> formatExp e
      formatLValue (Index e i) = formatExp e <+> brackets (formatExp i)
      formatLValue (Field (LValue (Deref e)) field) =
        formatExp e <> "->" <> field
      formatLValue (Field e field) = formatExp e <> "." <> field
      formatLValue (Global g) = formatGlobalname g
      formatLValue (Var v) = format v

      formatStm :: Stm -> Doc
      formatStm (Move dst src) =
        formatLValue dst <+> "<-" <+> formatExp src
      formatStm (Do e) = formatExp e

      formatTransfer :: Transfer -> Doc
      formatTransfer (Goto l) = "goto" <+> l
      formatTransfer (Case e cases def) =
        let
          mapfun (i, l) = i <> colon <+> l
        in
          braceBlock ("case" <+> formatExp e)
                     (("default" <> colon <+> def) : map mapfun cases)
      formatTransfer (Ret (Just e)) = "ret" <+> formatExp e
      formatTransfer (Ret Nothing) = format "ret"
      formatTransfer Unreachable = format "unreachable"

      formatBlock (Block stms trans) =
        vcat ((map formatStm stms) ++ [formatTransfer trans])

      formatGlobal :: Graph gr => Global gr -> Doc
      formatGlobal (Function { funcName = fname, funcRetTy = retty,
                               funcParams = argnames, funcValTys = vartypes,
                               funcBody = body }) =
        let
          argfun i = i <+> colon <+> formatType (vartypes ! i)
          varfun (i, ty) = i <+> colon <+> formatType ty
          header = parenList ("function" <+> fname) (map argfun argnames)
          vardocs = map varfun (assocs vartypes)

          fcontent =
            case body of
              Just (Body (Label entry) graph) ->
                let
                  getnode = fromJust . lab graph
                  blockfun node =
                    ("L" <> node <> colon) $$
                    nest 2 (formatBlock (getnode node))
                in
                  vardocs ++ (map blockfun (dfs [entry] graph))
              Nothing -> vardocs
        in
          braceBlock (header <+> colon <+> formatType retty) fcontent
      formatGlobal (GlobalVar { gvarName = gname, gvarTy = ty,
                                gvarInit = Just body }) =
          hang (hang ("global" <+> formatType ty) 2 gname) 2 (formatExp body)
      formatGlobal (GlobalVar { gvarName = gname, gvarTy = ty,
                                gvarInit = Nothing }) =
          hang ("global" <+> formatType ty) 2 gname

      typefunc (tyname, Just ty) =
        hang ("type" <+> tyname <+> equals) 2 (formatType ty)
      typefunc (tyname, Nothing) = "type" <+> tyname

      gcheaderfunc (GCHeader ind, (ty, mob, mut)) =
        "gc_header_" <> ind <+> equals <+> mut <+> mob <+> fst (types ! ty)

      gcgenfunc hdr = "gen" <+> formatGCHeader hdr

      typesdocs = map typefunc (elems types)
      gchdrdocs = map gcheaderfunc (assocs gcheaders)
      gcgendocs = map gcgenfunc gcgen
      globalsdocs = map formatGlobal (elems globals)
      content = typesdocs ++ (space : gchdrdocs) ++
                (space : gcgendocs) ++ (space : globalsdocs)
    in
      braceBlock ("module" <+> name) content
-}
instance Show Label where
  show (Label l) = "L" ++ show l

instance Show Fieldname where
  show (Fieldname f) = "f" ++ show f

instance Show Variantname where
  show (Variantname v) = "v" ++ show v

instance Show Id where
  show (Id v) = "%" ++ show v

instance Show Globalname where
  show (Globalname g) = "@" ++ show g
{-
instance Graph gr => Show (Module gr) where
  show = show . format
-}

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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

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
       GCHeader,
       Id,
       Label,
       Fieldname,
       Typename,
       Globalname,

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

       -- ** Utilities
       renameType
       ) where

import Data.Array
import Data.Graph.Inductive.Graph(Graph)
import Data.Functor
import Data.Hashable
import Data.Maybe
import Data.Intervals(Intervals)
import Data.Position.DWARFPosition(DWARFPosition)
import Data.Word
import IR.Common.Body
import IR.Common.LValue
import IR.Common.Names
import IR.Common.Ptr
import IR.Common.Operator
import IR.Common.Rename
import IR.Common.RenameType
import IR.Common.Transfer
import Prelude hiding (head, init)
--import Prelude.Extras
--import Text.Format
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)

import qualified Data.ByteString as Strict

-- FlatIR is a simply-typed IR intended to be close to LLVM.  It is
-- intended primarily as a jumping-off point for other languages
-- targeting LLVM.

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
-- types.
data Type =
  -- | A function type
    FuncType {
      -- | The return type of the function.
      funcTyRetTy :: Type,
      -- | The types of the arguments.
      funcTyArgTys :: [Type],
      -- | The position in source from which this arises.
      funcTyPos :: !DWARFPosition
    }
  -- | A structure, representing both tuples and records
  | StructType {
      -- | Whether or not the layout is strict.
      structPacked :: !Bool,
      -- | The fields of the struct.
      structFields :: Array Fieldname (Strict.ByteString, Mutability, Type),
      -- | The position in source from which this arises.
      structPos :: !DWARFPosition
    }
  -- | A variant, representing both tuples and records
  | VariantType {
      -- | The fields of the struct.
      variantForms :: Array Variantname (Strict.ByteString, Mutability, Type),
      -- | The position in source from which this arises.
      variantPos :: !DWARFPosition
    }
  -- | An array.  Unlike LLVM arrays, these may be variable-sized
  | ArrayType {
      -- | The length of the array, if known.
      arrayLen :: !(Maybe Word),
      -- | The type of array elements.
      arrayElemTy :: Type,
      -- | The position in source from which this arises.
      arrayPos :: !DWARFPosition
    }
  -- | Pointers, both native and GC
  | PtrType {
      -- | The pointer information
      ptrTy :: !(Ptr GCHeader Type),
      -- | The position in source from which this arises.
      ptrPos :: !DWARFPosition
    }
  -- | An integer, possibly signed, with a size.
  | IntType {
      -- | Whether or not the int is signed.
      intSigned :: !Bool,
      -- | The size of the int in bits.
      intSize :: !Word,
      -- | The possible-value intervals for the integer.
      intIntervals :: !(Intervals Integer),
      -- | The position in source from which this arises.
      intPos :: !DWARFPosition
    }
  -- | Floating point types
  | FloatType {
      -- | The size of the float in bits.
      floatSize :: !Word,
      -- | The position in source from which this arises.
      floatPos :: !DWARFPosition
    }
  -- | A defined type
  | IdType {
      -- | The name for this type.
      idName :: !Typename,
      -- | The position in source from which this arises.
      idPos :: !DWARFPosition
    }
  -- | The unit type, equivalent to SML unit and C/Java void
  | UnitType !DWARFPosition

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
      binopPos :: !DWARFPosition
    }
  -- | Call a function.
  | Call {
      -- | The function being called.  Must be a function value.
      callFunc :: Exp,
      -- | The arguments to the function.
      callArgs :: [Exp],
      -- | The position in source from which this arises.
      callPos :: !DWARFPosition
    }
  -- | A unary operation
  | Unop {
      -- | The operator.
      unopOp :: !Unop,
      -- | The operand.
      unopVal ::  Exp,
      -- | The position in source from which this arises.
      unopPos :: !DWARFPosition
    }
  -- | A conversion from one type to another.
  | Conv {
      -- | The type to which the value is being converted.
      convTy :: Type,
      -- | The value being converted.
      convVal :: Exp,
      -- | The position in source from which this arises.
      convPos :: !DWARFPosition
    }
  -- | Treat an expression as if it were the given type regardless of
  -- its actual type.
  | Cast {
      -- | The type to which the value is being cast.
      castTy :: Type,
      -- | The value being cast.
      castVal :: Exp,
      -- | The position in source from which this arises.
      castPos :: !DWARFPosition
    }
  -- | Address of an LValue
  | AddrOf {
      -- | The value having its address taken.
      addrofVal :: LValue Exp,
      -- | The position in source from which this arises.
      addrofPos :: !DWARFPosition
    }
  -- | A structure literal
  | StructLit {
      -- | The literal's type, must be a struct type.
      structLitTy :: Type,
      -- | The constant's field values
      structLitFields :: Array Fieldname Exp,
      -- | The position in source from which this arises.
      structLitPos :: !DWARFPosition
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
      variantLitPos :: !DWARFPosition
    }
  -- | An array literal
  | ArrayLit {
      -- | The constant's type, must be an array type.
      arrayLitTy :: Type,
      -- | The constant's values
      arrayLitVals :: [Exp],
      -- | The position in source from which this arises.
      arrayLitPos :: !DWARFPosition
    }
  -- | A numerical constant with a given size and signedness XXX add a
  -- floating point constant.
  | IntLit {
      -- | The constant's type, must be an integer or float type.
      intLitTy :: Type,
      -- | The constant's value
      intLitVal :: !Integer,
      -- | The position in source from which this arises.
      intLitPos :: !DWARFPosition
    }
  -- | An LValue
  | LValue !(LValue Exp)

-- | A global value.  Represents a global variable or a function.
data Global gr =
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
      funcBody :: Maybe (Body Exp (StmElems Exp) gr),
      -- | The position in source from which this arises.
      funcPos :: !DWARFPosition
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
      gvarPos :: !DWARFPosition
    }

-- | A module.  Represents a concept similar to an LLVM module.
data Module gr =
    Module {
      -- | Name of the module
      modName :: !Strict.ByteString,
      -- | A map from typenames to their proper names and possibly their
      -- definitions
      modTypes :: Array Typename (Strict.ByteString, Maybe Type),
      -- | A map from GCHeaders to their definitions
      modGCHeaders :: Array GCHeader (Typename, Mobility, Mutability),
      -- | Generated GC types (this module will generate the signatures
      -- and accessors)
      modGenGCs :: [GCHeader],
      -- | A map from global names to the corresponding definitions
      modGlobals :: Array Globalname (Global gr),
      -- | The position in source from which this arises.  This is here
      -- solely to record filenames in a unified way.
      modPos :: !DWARFPosition
    }

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
  IntType { intSigned = signed1, intIntervals = intervals1,
            intSize = size1 } ==
    IntType { intSigned = signed2, intIntervals = intervals2,
              intSize = size2 } =
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
  IntLit { intLitTy = ty1, intLitVal = val1 } ==
    IntLit { intLitTy = ty2, intLitVal = val2 } =
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
  compare IntType { intSigned = signed1, intIntervals = intervals1,
                    intSize = size1 }
          IntType { intSigned = signed2, intIntervals = intervals2,
                    intSize = size2 } =
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
  compare IntLit { intLitTy = ty1, intLitVal = val1 }
          IntLit { intLitTy = ty2, intLitVal = val2 } =
    case compare ty1 ty2 of
      EQ -> compare val1 val2
      out -> out
  compare IntLit {} _ = LT
  compare _ IntLit {} = GT
  compare (LValue lval1) (LValue lval2) = compare lval1 lval2

instance Hashable Type where
  hashWithSalt s FuncType { funcTyRetTy = retty, funcTyArgTys = params } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` retty `hashWithSalt` params
  hashWithSalt s StructType { structPacked = packed, structFields = fields } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt`
      packed `hashWithSalt` elems fields
  hashWithSalt s VariantType { variantForms = forms } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` elems forms
  hashWithSalt s ArrayType { arrayLen = Nothing, arrayElemTy = inner } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` (0 :: Int) `hashWithSalt` inner
  hashWithSalt s ArrayType { arrayLen = Just size, arrayElemTy = inner } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` size `hashWithSalt` inner
  hashWithSalt s PtrType { ptrTy = objtype } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` objtype
  hashWithSalt s IntType { intSigned = signed, intIntervals = intervals,
                           intSize = size } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` signed `hashWithSalt`
      intervals `hashWithSalt` size
  hashWithSalt s IdType { idName = name } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt` name
  hashWithSalt s FloatType { floatSize = size } =
    s `hashWithSalt` (7 :: Int) `hashWithSalt` size
  hashWithSalt s UnitType {} = s `hashWithSalt` (7 :: Int)

instance Hashable Exp where
  hashWithSalt s Binop { binopOp = op, binopLeft = left, binopRight = right } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt`
    op `hashWithSalt` left `hashWithSalt` right
  hashWithSalt s Call { callFunc = func, callArgs = args } =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` func `hashWithSalt` args
  hashWithSalt s Unop { unopOp = op, unopVal = val } =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` op `hashWithSalt` val
  hashWithSalt s Conv { convTy = ty, convVal = val } =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` ty `hashWithSalt` val
  hashWithSalt s Cast { castTy = ty, castVal = val } =
    s `hashWithSalt` (5 :: Int) `hashWithSalt` ty `hashWithSalt` val
  hashWithSalt s AddrOf { addrofVal = val } =
    s `hashWithSalt` (6 :: Int) `hashWithSalt` val
  hashWithSalt s StructLit { structLitTy = ty, structLitFields = fields } =
    s `hashWithSalt` (7 :: Int) `hashWithSalt` ty `hashWithSalt` elems fields
  hashWithSalt s VariantLit { variantLitTy = ty, variantLitForm = form,
                              variantLitVal = val } =
    s `hashWithSalt` (8 :: Int) `hashWithSalt`
    form `hashWithSalt` ty `hashWithSalt` val
  hashWithSalt s ArrayLit { arrayLitTy = ty, arrayLitVals = vals } =
    s `hashWithSalt` (9 :: Int) `hashWithSalt` ty `hashWithSalt` vals
  hashWithSalt s IntLit { intLitTy = ty, intLitVal = val } =
    s `hashWithSalt` (10 :: Int) `hashWithSalt` ty `hashWithSalt` val
  hashWithSalt s (LValue lval) =
    s `hashWithSalt` (11 :: Int) `hashWithSalt` lval
  hashWithSalt _ (GCAlloc {}) = error "GCAlloc is going away"

instance RenameType Typename Type where
  renameType f ty @ FuncType { funcTyRetTy = retty, funcTyArgTys = argtys } =
    ty { funcTyArgTys = renameType f argtys, funcTyRetTy = renameType f retty }
  renameType f ty @ StructType { structFields = fields } =
    ty { structFields = fmap (\(n, m, t) -> (n, m, renameType f t)) fields }
  renameType f ty @ VariantType { variantForms = forms } =
    ty { variantForms = fmap (\(n, m, t) -> (n, m, renameType f t)) forms }
  renameType f ty @ ArrayType { arrayElemTy = elemty } =
    ty { arrayElemTy = renameType f elemty }
--  renameType f ty @ PtrType { ptrTy = inner } =
--    ty { ptrTy = renameType f inner }
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
  renameType f e @ IntLit { intLitTy = ty } =
    e { intLitTy = renameType f ty }
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

funcTypePickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text) =>
                   PU [NodeG [] tag text] Type
funcTypePickler =
  let
    revfunc FuncType { funcTyRetTy = retty, funcTyArgTys = argtys,
                       funcTyPos = pos } = (argtys, retty, pos)
    revfunc _ = error "Can't convert to FuncType"
  in
    xpWrap (\(argtys, retty, pos) -> FuncType { funcTyRetTy = retty,
                                                funcTyArgTys = argtys,
                                                funcTyPos = pos },
            revfunc)
           (xpElemNodes (gxFromString "FuncType")
                        (xpTriple (xpElemNodes (gxFromString "args")
                                               (xpList xpickle))
                                  (xpElemNodes (gxFromString "ret") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

fieldPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text]
                   (Fieldname, (Strict.ByteString, Mutability, Type))
fieldPickler =
  xpWrap (\((idx, fname, mut), ty) -> (idx, (gxToByteString fname, mut, ty)),
          \(idx, (fname, mut, ty)) -> ((idx, gxFromByteString fname, mut), ty))
         (xpElem (gxFromString "field")
                 (xpTriple xpickle (xpAttr (gxFromString "name") xpText)
                           xpickle)
                 xpickle)

fieldsPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text]
                    (Array Fieldname (Strict.ByteString, Mutability, Type))
fieldsPickler =
  xpWrap (\l -> array (toEnum 0, toEnum (length l)) l, assocs)
         (xpElemNodes (gxFromString "fields") (xpList fieldPickler))

structTypePickler :: (GenericXMLString tag, Show tag,
                      GenericXMLString text, Show text) =>
                     PU [NodeG [] tag text] Type
structTypePickler =
  let
    revfunc StructType { structPacked = packed, structFields = fields,
                         structPos = pos } = (packed, (fields, pos))
    revfunc _ = error "Can't convert to StructType"
  in
    xpWrap (\(packed, (fields, pos)) -> StructType { structPacked = packed,
                                                     structFields = fields,
                                                     structPos = pos },
            revfunc)
           (xpElem (gxFromString "StructType")
                   (xpAttr (gxFromString "packed") xpPrim)
                   (xpPair (xpElemNodes (gxFromString "fields") fieldsPickler)
                           (xpElemNodes (gxFromString "pos") xpickle)))

formPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text]
                   (Variantname, (Strict.ByteString, Mutability, Type))
formPickler =
  xpWrap (\((idx, fname, mut), ty) -> (idx, (gxToByteString fname, mut, ty)),
          \(idx, (fname, mut, ty)) -> ((idx, gxFromByteString fname, mut), ty))
         (xpElem (gxFromString "form")
                 (xpTriple xpickle (xpAttr (gxFromString "name") xpText)
                           xpickle)
                 xpickle)

formsPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text]
                    (Array Variantname (Strict.ByteString, Mutability, Type))
formsPickler =
  xpWrap (\l -> array (toEnum 0, toEnum (length l)) l, assocs)
         (xpElemNodes (gxFromString "forms") (xpList formPickler))

variantTypePickler :: (GenericXMLString tag, Show tag,
                       GenericXMLString text, Show text) =>
                      PU [NodeG [] tag text] Type
variantTypePickler =
  let
    revfunc VariantType { variantForms = forms, variantPos = pos } =
      (forms, pos)
    revfunc _ = error "Can't convert to VariantType"
  in
    xpWrap (\(forms, pos) -> VariantType { variantForms = forms,
                                           variantPos = pos },
            revfunc)
           (xpElemNodes (gxFromString "VariantType")
                        (xpPair (xpElemNodes (gxFromString "forms")
                                             formsPickler)
                                (xpElemNodes (gxFromString "pos") xpickle)))

arrayTypePickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] Type
arrayTypePickler =
  let
    revfunc ArrayType { arrayElemTy = elemty, arrayLen = len,
                        arrayPos = pos } = (len, (elemty, pos))
    revfunc _ = error "Can't convert to ArrayType"
  in
    xpWrap (\(len, (elemty, pos)) -> ArrayType { arrayElemTy = elemty,
                                                 arrayLen = len,
                                                 arrayPos = pos },
            revfunc)
           (xpElem (gxFromString "ArrayType")
                   (xpOption (xpAttr (gxFromString "len") xpPrim))
                   (xpPair (xpElemNodes (gxFromString "elem") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

ptrTypePickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Type
ptrTypePickler =
  let
    revfunc PtrType { ptrTy = ptrty, ptrPos = pos } = (ptrty, pos)
    revfunc _ = error "Can't convert to PtrType"
  in
    xpWrap (\(ptrty, pos) -> PtrType { ptrTy = ptrty, ptrPos = pos },
            revfunc)
           (xpElemNodes (gxFromString "PtrType")
                        (xpPair (xpElemNodes (gxFromString "inner") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

intTypePickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] Type
intTypePickler =
  let
    revfunc IntType { intSize = size, intSigned = signed,
                      intIntervals = intervals, intPos = pos } =
      ((signed, size), (intervals, pos))
    revfunc _ = error "Can't convert to IntType"
  in
    xpWrap (\((signed, size), (intervals, pos)) ->
             IntType { intSize = size, intSigned = signed,
                       intIntervals = intervals, intPos = pos }, revfunc)
           (xpElem (gxFromString "IntType")
                   (xpPair (xpAttr (gxFromString "signed") xpPrim)
                           (xpAttr (gxFromString "size") xpPrim))
                   (xpPair (xpElemNodes (gxFromString "intervals") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

floatTypePickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] Type
floatTypePickler =
  let
    revfunc FloatType { floatSize = size, floatPos = pos } = (size, pos)
    revfunc _ = error "Can't convert to FloatType"
  in
    xpWrap (\(size, pos) -> FloatType { floatSize = size, floatPos = pos },
            revfunc)
           (xpElem (gxFromString "FloatType")
                   (xpAttr (gxFromString "size") xpPrim)
                   (xpElemNodes (gxFromString "pos") xpickle))

idTypePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Type
idTypePickler =
  let
    revfunc IdType { idName = tyname, idPos = pos } = (tyname, pos)
    revfunc _ = error "Can't convert to IdType"
  in
    xpWrap (\(tyname, pos) -> IdType { idName = tyname, idPos = pos }, revfunc)
           (xpElem (gxFromString "IdType") xpickle
                   (xpElemNodes (gxFromString "pos") xpickle))
unitTypePickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text) =>
                   PU [NodeG [] tag text] Type
unitTypePickler =
  let
    revfunc (UnitType pos) = pos
    revfunc _ = error "Can't convert to UnitType"
  in
    xpWrap (UnitType, revfunc)
           (xpElemNodes (gxFromString "UnitType")
                        (xpElemNodes (gxFromString "pos") xpickle))

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Type where
  xpickle =
    let
      picker FuncType {} = 0
      picker StructType {} = 1
      picker VariantType {} = 2
      picker ArrayType {} = 3
      picker PtrType {} = 4
      picker IntType {} = 5
      picker FloatType {} = 6
      picker IdType {} = 7
      picker UnitType {} = 8
    in
      xpAlt picker [funcTypePickler, structTypePickler, variantTypePickler,
                    arrayTypePickler, ptrTypePickler, intTypePickler,
                    floatTypePickler, idTypePickler, unitTypePickler ]

binopPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] Exp
binopPickler =
  let
    revfunc Binop { binopOp = op, binopLeft = left,
                    binopRight = right, binopPos = pos } =
      (op, (left, right, pos))
    revfunc _ = error "Can't convert to Binop"
  in
    xpWrap (\(op, (left, right, pos)) ->
             Binop { binopOp = op, binopLeft = left,
                     binopRight = right, binopPos = pos }, revfunc)
           (xpElem (gxFromString "Binop") xpickle
                   (xpTriple (xpElemNodes (gxFromString "left") xpickle)
                             (xpElemNodes (gxFromString "right") xpickle)
                             (xpElemNodes (gxFromString "pos") xpickle)))

callPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Exp
callPickler =
  let
    revfunc Call { callFunc = func, callArgs = args, callPos = pos } =
      (func, args, pos)
    revfunc _ = error "Can't convert to Call"
  in
    xpWrap (\(func, args, pos) -> Call { callFunc = func, callArgs = args,
                                         callPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Call")
                        (xpTriple (xpElemNodes (gxFromString "func") xpickle)
                                  (xpElemNodes (gxFromString "args")
                                               (xpList xpickle))
                                  (xpElemNodes (gxFromString "pos") xpickle)))

unopPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Exp
unopPickler =
  let
    revfunc Unop { unopOp = op, unopVal = val, unopPos = pos } =
      (op, (val, pos))
    revfunc _ = error "Can't convert to Unop"
  in
    xpWrap (\(op, (val, pos)) -> Unop { unopOp = op, unopVal = val,
                                        unopPos = pos }, revfunc)
           (xpElem (gxFromString "Unop") xpickle
                   (xpPair (xpElemNodes (gxFromString "val") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

convPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Exp
convPickler =
  let
    revfunc Conv { convVal = val, convTy = ty, convPos = pos } = (val, ty, pos)
    revfunc _ = error "Can't convert to Conv"
  in
    xpWrap (\(val, ty, pos) -> Conv { convVal = val, convTy = ty,
                                      convPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Conv")
                        (xpTriple (xpElemNodes (gxFromString "val") xpickle)
                                  (xpElemNodes (gxFromString "type") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

castPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] Exp
castPickler =
  let
    revfunc Cast { castVal = val, castTy = ty, castPos = pos } = (val, ty, pos)
    revfunc _ = error "Can't convert to Cast"
  in
    xpWrap (\(val, ty, pos) -> Cast { castVal = val, castTy = ty,
                                      castPos = pos }, revfunc)
           (xpElemNodes (gxFromString "Cast")
                        (xpTriple (xpElemNodes (gxFromString "val") xpickle)
                                  (xpElemNodes (gxFromString "type") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

addrofPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Exp
addrofPickler =
  let
    revfunc AddrOf { addrofVal = val, addrofPos = pos } = (val, pos)
    revfunc _ = error "Can't convert to AddrOf"
  in
    xpWrap (\(val, pos) -> AddrOf { addrofVal = val, addrofPos = pos },
            revfunc)
           (xpElemNodes (gxFromString "AddrOf")
                        (xpPair (xpElemNodes (gxFromString "val") xpickle)
                                (xpElemNodes (gxFromString "pos") xpickle)))

structLitPickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] Exp
structLitPickler =
  let
    revfunc StructLit { structLitTy = ty, structLitFields = fields,
                        structLitPos = pos } = (ty, fields, pos)
    revfunc _ = error "Can't convert to StructLit"

    fieldValsPickler :: (GenericXMLString tag, Show tag,
                         GenericXMLString text, Show text) =>
                        PU [NodeG [] tag text] (Array Fieldname Exp)
    fieldValsPickler =
      xpWrap (\l -> array (toEnum 0, toEnum (length l)) l, assocs)
             (xpList (xpElem (gxFromString "field") xpickle xpickle))
  in
    xpWrap (\(ty, fields, pos) ->
             StructLit { structLitTy = ty, structLitFields = fields,
                         structLitPos = pos }, revfunc)
           (xpElemNodes (gxFromString "StructLit")
                        (xpTriple (xpElemNodes (gxFromString "ty") xpickle)
                                  (xpElemNodes (gxFromString "fields")
                                               fieldValsPickler)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

variantLitPickler :: (GenericXMLString tag, Show tag,
                      GenericXMLString text, Show text) =>
                     PU [NodeG [] tag text] Exp
variantLitPickler =
  let
    revfunc VariantLit { variantLitTy = ty, variantLitVal = val,
                         variantLitForm = form, variantLitPos = pos } =
      (form, (ty, val, pos))
    revfunc _ = error "Can't convert to VariantLit"
  in
    xpWrap (\(form, (ty, val, pos)) ->
             VariantLit { variantLitTy = ty, variantLitVal = val,
                          variantLitForm = form, variantLitPos = pos }, revfunc)
           (xpElem (gxFromString "VariantLit") xpickle
                   (xpTriple (xpElemNodes (gxFromString "ty") xpickle)
                             (xpElemNodes (gxFromString "val") xpickle)
                             (xpElemNodes (gxFromString "pos") xpickle)))

arrayLitPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text) =>
                   PU [NodeG [] tag text] Exp
arrayLitPickler =
  let
    revfunc ArrayLit { arrayLitTy = ty, arrayLitVals = vals,
                       arrayLitPos = pos } = (ty, vals, pos)
    revfunc _ = error "Can't convert to ArrayLit"
  in
    xpWrap (\(ty, vals, pos) -> ArrayLit { arrayLitTy = ty, arrayLitVals = vals,
                                           arrayLitPos = pos }, revfunc)
           (xpElemNodes (gxFromString "ArrayLit")
                        (xpTriple (xpElemNodes (gxFromString "ty") xpickle)
                                  (xpElemNodes (gxFromString "vals")
                                               (xpList xpickle))
                                  (xpElemNodes (gxFromString "pos") xpickle)))

intLitPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                 PU [NodeG [] tag text] Exp
intLitPickler =
  let
    revfunc IntLit { intLitTy = ty, intLitVal = val, intLitPos = pos } =
      (val, (ty, pos))
    revfunc _ = error "Can't convert to IntType"
  in
    xpWrap (\(val, (ty, pos)) -> IntLit { intLitTy = ty, intLitVal = val,
                                          intLitPos = pos }, revfunc)
           (xpElem (gxFromString "IntType")
                   (xpAttr (gxFromString "size") xpPrim)
                   (xpPair (xpElemNodes (gxFromString "type") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

lvaluePickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text) =>
                   PU [NodeG [] tag text] Exp
lvaluePickler =
  let
    revfunc (LValue lval) = lval
    revfunc _ = error "Can't convert to LValue"
  in
    xpWrap (LValue, revfunc) (xpElemNodes (gxFromString "LValue") xpickle)

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Exp where
  xpickle =
    let
      picker GCAlloc {} = 0
      picker Binop {} = 1
      picker Call {} = 1
      picker Unop {} = 2
      picker Conv {} = 3
      picker Cast {} = 4
      picker AddrOf {} = 5
      picker StructLit {} = 6
      picker VariantLit {} = 7
      picker ArrayLit {} = 8
      picker IntLit {} = 9
      picker LValue {} = 10
    in
      xpAlt picker [undefined, binopPickler, callPickler, unopPickler,
                    convPickler, castPickler, addrofPickler,
                    structLitPickler, variantLitPickler, arrayLitPickler,
                    intLitPickler, lvaluePickler ]

functionPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text,
                    Graph gr) =>
                   PU [NodeG [] tag text] (Global gr)
functionPickler =
  let
    revfunc Function { funcName = fname, funcRetTy = retty, funcValTys = valtys,
                       funcParams = params, funcBody = body, funcPos = pos } =
      (fname, (retty, valtys, params, body, pos))
    revfunc _ = error "Can't convert to Function"

    valtysPickler :: (GenericXMLString tag, Show tag,
                      GenericXMLString text, Show text) =>
                     PU [NodeG [] tag text] (Array Id Type)
    valtysPickler =
      xpWrap (\l -> array (toEnum 0, toEnum (length l)) l, assocs)
             (xpList (xpElem (gxFromString "valty") xpickle xpickle))
  in
    xpWrap (\(fname, (retty, valtys, params, body, pos)) ->
             Function { funcName = fname, funcRetTy = retty,
                        funcValTys = valtys, funcParams = params,
                        funcBody = body, funcPos = pos }, revfunc)
           (xpElem (gxFromString "Function") xpickle
                   (xp5Tuple (xpElemNodes (gxFromString "retty") xpickle)
                             (xpElemNodes (gxFromString "valtys") valtysPickler)
                             (xpElemNodes (gxFromString "params")
                                          (xpList xpickle))
                             (xpOption (xpElemNodes (gxFromString "body")
                                                    xpickle))
                             (xpElemNodes (gxFromString "pos") xpickle)))

globalvarPickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] (Global gr)
globalvarPickler =
  let
    revfunc GlobalVar { gvarName = gname, gvarTy = ty, gvarInit = init,
                        gvarMutability = mut, gvarPos = pos } =
      ((gname, mut), (ty, init, pos))
    revfunc _ = error "Can't convert to GlobalVar"
  in
    xpWrap (\((gname, mut), (ty, init, pos)) ->
             GlobalVar { gvarName = gname, gvarTy = ty, gvarInit = init,
                         gvarMutability = mut, gvarPos = pos }, revfunc)
           (xpElem (gxFromString "Function") (xpPair xpickle xpickle)
                   (xpTriple (xpElemNodes (gxFromString "type") xpickle)
                             (xpOption (xpElemNodes (gxFromString "init")
                                                    xpickle))
                             (xpElemNodes (gxFromString "pos") xpickle)))

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text,
          Graph gr) =>
         XmlPickler [NodeG [] tag text] (Global gr) where
  xpickle =
    let
      picker Function {} = 0
      picker GlobalVar {} = 1
    in
      xpAlt picker [functionPickler, globalvarPickler]


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
      formatExp (IntLit ty n) = hang (formatType ty) 2 (format n)

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

instance Graph gr => Show (Module gr) where
  show = show . format
-}

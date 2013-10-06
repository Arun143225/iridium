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
import IR.Common.Ptr
import IR.Common.Operator
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

-- | A label, indexes blocks
newtype Label = Label Node
  deriving (Ord, Eq, Ix)

-- | An identifier, indexes variables
newtype Id = Id Word
  deriving (Ord, Eq, Ix)

-- | A field name, indexes fields
newtype Fieldname = Fieldname Word
  deriving (Ord, Eq, Ix)

-- | A type name, indexes types
newtype Typename = Typename Word
  deriving (Ord, Eq, Ix)

-- | A function name, indexes functions
newtype Globalname = Globalname Word
  deriving (Ord, Eq, Ix)

-- | A header given to GCAlloc representing the type being allocated
newtype GCHeader = GCHeader Word
  deriving (Ord, Eq, Ix)

-- | Transfers.  These represent methods of leaving a basic block.
-- All basic blocks end in a transfer.
data Transfer =
  -- | A direct jump
    Goto {
      -- | The jump target.
      gotoLabel :: !Label,
      -- | The position in source from which this arises.
      gotoPos :: !Pos
    }
  -- | A (integer) case expression
  | Case {
      -- | The value being decided upon.  Must be an integer value.
      caseVal :: Exp,
      -- | The cases.  There must be at least one case.
      caseCases :: [(Integer, Label)],
      -- | The default case.
      caseDefault :: !Label,
      -- | The position in source from which this arises.
      casePos :: !Pos
    }
  -- | A return
  | Ret { 
      -- | The return value, if one exists.
      retVal :: Maybe Exp,
      -- | The position in source from which this arises.
      retPos :: !Pos
    }
  -- | An unreachable instruction, usually following a call with no
  -- return
  | Unreachable !Pos

-- | An assignable value
data LValue =
  -- | An array (or pointer) index
    Index {
      -- | The indexed value.  Must be an array.
      idxVal :: Exp,
      -- | The index value.  Must be an integer type.
      idxIndex :: Exp,
      -- | The position in source from which this arises.
      idxPos :: !Pos
    }
  -- | A field in a structure
  | Field {
      -- | The value whose field is being accessed.  Must be a
      -- structure type.
      fieldVal :: Exp,
      -- | The name of the field being accessed.
      fieldName :: !Fieldname,
      -- | The position in source from which this arises.
      fieldPos :: !Pos
    }
  -- | Dereference a pointer
  | Deref {
      -- | The value being dereferenced.  Must be a pointer type.
      derefVal :: Exp,
      -- | The position in source from which this arises.
      derefPos :: !Pos
    }
  -- | A local value (local variable or argument)
  | Var {
      -- | The name of the local value.
      varName :: !Id,
      -- | The position in source from which this arises.
      varPos :: !Pos
    }
  -- | A global value (global variable or function)
  | Global {
      -- | The name of the global value.
      globalName :: !Globalname,
      -- | The position in source from which this arises.
      globalPos :: !Pos
    }

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
  -- | A structure constant
  | StructConst {
      -- | The constant's type, must be a struct type.
      structConstTy :: Type,
      -- | The constant's field values
      structConstFields :: Array Fieldname Exp,
      -- | The position in source from which this arises.
      structConstPos :: !Pos
    }
  -- | An array constant
  | ArrayConst {
      -- | The constant's type, must be an array type.
      arrayConstTy :: Type,
      -- | The constant's values
      arrayConstVals :: [Exp],
      -- | The position in source from which this arises.
      arrayConstPos :: !Pos
    }
  -- | A numerical constant with a given size and signedness XXX add a
  -- floating point constant.
  | NumConst {
      -- | The constant's type, must be an integer or float type.
      numConstTy :: Type,
      -- | The constant's value
      numConstVal :: !Integer,
      -- | The position in source from which this arises.
      numConstPos :: !Pos
    }
  -- | An LValue
  | LValue !LValue

-- | A statement.  Represents an effectful action for the statement
-- form of the IR.
data Stm =
  -- | Update the given lvalue
    Move {
      -- | The destination LValue.
      moveDst :: LValue,
      -- | The source expression.
      moveSrc :: Exp,
      -- | The position in source from which this originates.
      movePos :: !Pos
    }
  -- | Execute an expression
  | Do !Exp

-- | A binding.  Represents an SSA binding in the SSA form of the
-- language.
data Bind =
    -- | A Phi-node.
    Phi {
      -- | The name being bound.
      phiName :: !Id,
      -- | A map from inbound edges to values
      phiVals :: Map Label Exp,
      -- | The position in source from which this arises.
      phiPos :: !Pos
    }
    -- | A regular binding
  | Bind {
      -- | The name being bound.
      bindName :: !Id,
      -- | The value beind bound.
      bindVal :: Exp,
      -- | The position in source from which this originates.
      bindPos :: !Pos
    }
    -- | Execute an expression for its effect only.  Analogous to Do
    -- in the statement language.
  | Effect !Exp

-- | A basic block
data Block elem =
    Block {
      -- | The statements in the basic block
      blockBody :: [elem],
      -- | The transfer for the basic block
      blockXfer :: Transfer,
      -- | The position in source from which this arises.
      blockPos :: !Pos
    }

-- There is no straightforward ordering, equality, or hashing on the
-- remaining types.

-- | The body of a function
data Body elem gr =
    Body {
      -- | The entry block
      bodyEntry :: !Label,
      -- | The CFG
      bodyCFG :: (gr (Block elem) ())
    }

-- | A datatype encoding the various names of a global declaration.
data DeclNames =
  DeclNames {
    -- | The basic name of the declaration.
    basicName :: !String,
    -- | The linkage name of the declaration (often encodes type
    -- information, as in C++)
    linkageName :: !String,
    -- | The displayed name of the declaration (usually contains type
    -- information).
    displayName :: !String
  }

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
      -- | A map from global names to the corresponding functions
      modGlobals :: Array Globalname (Global elem gr),
      -- | The position in source from which this arises.  This is here
      -- solely to record filenames in a unified way.
      modPos :: !Pos
    }

type StmModule = Module Stm
type SSAModule = Module Bind

instance RenameType Typename Type where
  renameType f ty @ FuncType { funcTyRetTy = retty, funcTyArgTys = argtys } =
    ty { funcTyArgTys = renameType f argtys, funcTyRetTy = renameType f retty }
  renameType f ty @ StructType { structFields = fields } =
    ty { structFields = fmap (\(n, m, t) -> (n, m, renameType f t)) fields }
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
  renameType f e @ StructConst { structConstFields = fields,
                                 structConstTy = ty } =
    e { structConstFields = renameType f fields,
        structConstTy = renameType f ty }
  renameType f e @ ArrayConst { arrayConstVals = vals, arrayConstTy = ty } =
    e { arrayConstVals = renameType f vals, arrayConstTy = renameType f ty }
  renameType f e @ NumConst { numConstTy = ty } =
    e { numConstTy = renameType f ty }
  renameType f (LValue l) = LValue (renameType f l)
  renameType _ e = e

instance RenameType Typename LValue where
  renameType f lval @ Index { idxVal = inner } =
    lval { idxVal = renameType f inner }
  renameType f lval @ Field { fieldVal = inner } =
    lval { fieldVal = renameType f inner }
  renameType f lval @ Deref { derefVal = inner } =
    lval { derefVal = renameType f inner }
  renameType _ lval = lval

instance Eq Type where
  FuncType { funcTyRetTy = retty1, funcTyArgTys = params1 } ==
    FuncType { funcTyRetTy = retty2, funcTyArgTys = params2 } =
    retty1 == retty2 && params1 == params2
  StructType { structPacked = packed1, structFields = fields1 } ==
    StructType { structPacked = packed2, structFields = fields2 } =
    packed1 == packed2 && fields1 == fields2
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

instance Eq LValue where
  Index { idxVal = val1, idxIndex = idx1 } ==
    Index { idxVal = val2, idxIndex = idx2 } = val1 == val2 && idx1 == idx2
  Field { fieldVal = val1, fieldName = name1 } ==
    Field { fieldVal = val2, fieldName = name2 } = val1 == val2 && name1 == name2
  Deref { derefVal = val1 } == Deref { derefVal = val2 } = val1 == val2
  Var { varName = name1 } == Var { varName = name2 } = name1 == name2
  Global { globalName = name1 } == Global { globalName = name2 } =
    name1 == name2
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
  StructConst { structConstTy = ty1, structConstFields = fields1 } ==
    StructConst { structConstTy = ty2, structConstFields = fields2 } =
    ty1 == ty2 && fields1 == fields2
  ArrayConst { arrayConstTy = ty1, arrayConstVals = vals1 } ==
    ArrayConst { arrayConstTy = ty2, arrayConstVals = vals2 } =
    ty1 == ty2 && vals1 == vals2
  NumConst { numConstTy = ty1, numConstVal = val1 } ==
    NumConst { numConstTy = ty2, numConstVal = val2 } =
    ty1 == ty2 && val1 == val2
  (LValue lval1) == (LValue lval2) = lval1 == lval2
  _ == _ = False

instance Eq Stm where
  Move { moveSrc = src1, moveDst = dst1 } ==
    Move { moveSrc = src2, moveDst = dst2 } = src1 == src2 && dst1 == dst2
  (Do exp1) == (Do exp2) = exp1 == exp2
  _ == _ = False

instance Eq Bind where
  Phi { phiName = name1, phiVals = vals1 } ==
    Phi { phiName = name2, phiVals = vals2 } =
    name1 == name2 && vals1 == vals2
  Bind { bindName = name1, bindVal = val1 } ==
    Bind { bindName = name2, bindVal = val2 } =
    name1 == name2 && val1 == val2
  (Effect e1) == (Effect e2) = e1 == e2
  _ == _ = False

instance Eq Transfer where
  Goto { gotoLabel = label1 } == Goto { gotoLabel = label2 } = label1 == label2
  Case { caseVal = val1, caseCases = cases1, caseDefault = def1 } ==
    Case { caseVal = val2, caseCases = cases2, caseDefault = def2 } =
    val1 == val2 && cases1 == cases2 && def1 == def2
  Ret { retVal = ret1 } == Ret { retVal = ret2 } = ret1 == ret2
  Unreachable _ == Unreachable _ = True
  _ == _ = False

instance Eq1 Block where
  Block { blockBody = body1, blockXfer = xfer1 } ==#
    Block { blockBody = body2, blockXfer = xfer2 } =
    xfer1 == xfer2 && (body1 == body2)

instance Eq elem => Eq (Block elem) where (==) = (==#)

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

instance Ord LValue where
  compare Index { idxVal = val1, idxIndex = idx1 }
          Index { idxVal = val2, idxIndex = idx2 } =
    case compare idx1 idx2 of
      EQ -> compare val1 val2
      out -> out
  compare Index {} _ = LT
  compare _ Index {} = GT
  compare Field { fieldVal = val1, fieldName = name1 }
          Field { fieldVal = val2, fieldName = name2 } =
    case compare name1 name2 of
      EQ -> compare val1 val2
      out -> out
  compare Field {} _ = LT
  compare _ Field {} = GT
  compare Deref { derefVal = val1 } Deref { derefVal = val2 } =
    compare val1 val2
  compare Deref {} _ = LT
  compare _ Deref {} = GT
  compare Var { varName = name1 } Var { varName = name2 } = compare name1 name2
  compare Var {} _ = LT
  compare _ Var {} = GT
  compare Global { globalName = name1 } Global { globalName = name2 } =
    compare name1 name2

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
  compare StructConst { structConstTy = ty1, structConstFields = fields1 }
          StructConst { structConstTy = ty2, structConstFields = fields2 } =
    case compare ty1 ty2 of
      EQ -> compare fields1 fields2
      out -> out
  compare StructConst {} _ = LT
  compare _ StructConst {} = GT
  compare ArrayConst { arrayConstTy = ty1, arrayConstVals = vals1 }
          ArrayConst { arrayConstTy = ty2, arrayConstVals = vals2 } =
    case compare ty1 ty2 of
      EQ -> compare vals1 vals2
      out -> out
  compare ArrayConst {} _ = LT
  compare _ ArrayConst {} = GT
  compare NumConst { numConstTy = ty1, numConstVal = val1 }
          NumConst { numConstTy = ty2, numConstVal = val2 } =
    case compare ty1 ty2 of
      EQ -> compare val1 val2
      out -> out
  compare NumConst {} _ = LT
  compare _ NumConst {} = GT
  compare (LValue lval1) (LValue lval2) = compare lval1 lval2

instance Ord Stm where
  compare Move { moveSrc = src1, moveDst = dst1 }
          Move { moveSrc = src2, moveDst = dst2 } =
    case compare src1 src2 of
      EQ -> compare dst1 dst2
      out -> out
  compare (Move {}) _ = LT
  compare _ (Move {}) = GT
  compare (Do exp1) (Do exp2) = compare exp1 exp2

instance Ord Bind where
  compare Phi { phiName = name1, phiVals = vals1 }
          Phi { phiName = name2, phiVals = vals2 } =
    case compare name1 name2 of
      EQ -> compare vals1 vals2
      out -> out
  compare Phi {} _ = LT
  compare _ Phi {} = GT
  compare Bind { bindName = name1, bindVal = val1 }
          Bind { bindName = name2, bindVal = val2 } =
    case compare name1 name2 of
      EQ -> compare val1 val2
      out -> out
  compare Bind {} _ = LT
  compare _ Bind {} = GT
  compare (Effect e1) (Effect e2) = compare e1 e2

instance Ord Transfer where
  compare Goto { gotoLabel = label1 } Goto { gotoLabel = label2 } =
    compare label1 label2
  compare Goto {} _ = LT
  compare _ Goto {} = GT
  compare Case { caseVal = val1, caseCases = cases1, caseDefault = def1 }
          Case { caseVal = val2, caseCases = cases2, caseDefault = def2 } =
    case compare val1 val2 of
      EQ -> case compare def1 def2 of
        EQ -> compare cases1 cases2
        out -> out
      out -> out
  compare Case {} _ = LT
  compare _ Case {} = GT
  compare Ret { retVal = ret1 } Ret { retVal = ret2 } = compare ret1 ret2
  compare Ret {} _ = LT
  compare _ Ret {} = GT
  compare (Unreachable _) (Unreachable _) = EQ

instance Ord1 Block where
  compare1 Block { blockBody = body1, blockXfer = xfer1 }
           Block { blockBody = body2, blockXfer = xfer2 } =
    case compare xfer1 xfer2 of
      EQ -> compare1 body1 body2
      out -> out

instance Ord elem => Ord (Block elem) where compare = compare1

instance Position Type where
  pos FuncType { funcTyPos = p } = p
  pos StructType { structPos = p } = p
  pos ArrayType { arrayPos = p } = p
  pos PtrType { ptrPos = p } = p
  pos IntType { intPos = p } = p
  pos IdType { idPos = p } = p
  pos FloatType { floatPos = p } = p
  pos (UnitType p) = p

instance Position Exp where
  pos Binop { binopPos = p } = p
  pos Call { callPos = p } = p
  pos Conv { convPos = p } = p
  pos Cast { castPos = p } = p
  pos Unop { unopPos = p } = p
  pos AddrOf { addrofPos = p } = p
  pos StructConst { structConstPos = p } = p
  pos ArrayConst { arrayConstPos = p } = p
  pos NumConst { numConstPos = p } = p
  pos (LValue l) = pos l
  pos (GCAlloc _ _ _) = error "GCAlloc is going away"

instance Position LValue where
  pos Index { idxPos = p } = p
  pos Field { fieldPos = p } = p
  pos Deref { derefPos = p } = p
  pos Var { varPos = p } = p
  pos Global { globalPos = p } = p

instance Position Transfer where
  pos Goto { gotoPos = p } = p
  pos Case { casePos = p } = p
  pos Ret { retPos = p } = p
  pos (Unreachable p) = p

instance Position Stm where
  pos Move { movePos = p } = p
  pos (Do expr) = pos expr

instance Position Bind where
  pos Phi { phiPos = p } = p
  pos Bind { bindPos = p } = p
  pos (Effect e) = pos e

instance Position (Block elem) where
  pos Block { blockPos = p } = p

instance Position (Global elem gr) where
  pos Function { funcPos = p } = p
  pos GlobalVar { gvarPos = p } = p

instance Position (Module elem gr) where
  pos Module { modPos = p } = p

instance Hashable Typename where
  hashWithSalt s (Typename n) = s `hashWithSalt` n

instance Hashable GCHeader where
  hashWithSalt s (GCHeader n) = s `hashWithSalt` n

instance Hashable Type where
  hashWithSalt s FuncType { funcTyRetTy = retty, funcTyArgTys = params } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt` retty `hashWithSalt` params
  hashWithSalt s StructType { structPacked = packed, structFields = fields } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt`
      packed `hashWithSalt` (elems fields)
  hashWithSalt s ArrayType { arrayLen = Nothing, arrayElemTy = inner } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` (0 :: Word) `hashWithSalt` inner
  hashWithSalt s ArrayType { arrayLen = Just size, arrayElemTy = inner } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` size `hashWithSalt` inner
  hashWithSalt s PtrType { ptrTy = objtype } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` objtype
  hashWithSalt s IntType { intSigned = signed, intIntervals = intervals,
                           intSize = size } =
    s `hashWithSalt` (4 :: Word) `hashWithSalt` signed `hashWithSalt`
      intervals `hashWithSalt` size
  hashWithSalt s IdType { idName = Typename str } =
    s `hashWithSalt` (5 :: Word) `hashWithSalt` str
  hashWithSalt s FloatType { floatSize = size } =
    s `hashWithSalt` (6 :: Word) `hashWithSalt` size
  hashWithSalt s UnitType {} = s `hashWithSalt` (7 :: Word)

instance Hashable Transfer where
  hashWithSalt s Goto { gotoLabel = label } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` label
  hashWithSalt s Case { caseVal = val, caseCases = cases, caseDefault = def } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt`
      val `hashWithSalt` cases `hashWithSalt` def
  hashWithSalt s Ret { retVal = val } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` val
  hashWithSalt s (Unreachable _) = s `hashWithSalt` (4 :: Word)

instance Hashable LValue where
  hashWithSalt s Index { idxVal = val, idxIndex = idx } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` val `hashWithSalt` idx
  hashWithSalt s Field { fieldVal = val, fieldName = name } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` val `hashWithSalt` name
  hashWithSalt s Deref { derefVal = val } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt`val
  hashWithSalt s Var { varName = name } =
    s `hashWithSalt` (4 :: Word) `hashWithSalt` name
  hashWithSalt s Global { globalName = name } =
    s `hashWithSalt` (5 :: Word) `hashWithSalt` name

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
  hashWithSalt s StructConst { structConstTy = ty, structConstFields = fields } =
    s `hashWithSalt` (7 :: Word) `hashWithSalt` ty `hashWithSalt` (elems fields)
  hashWithSalt s ArrayConst { arrayConstTy = ty, arrayConstVals = vals } =
    s `hashWithSalt` (8 :: Word) `hashWithSalt` ty `hashWithSalt` vals
  hashWithSalt s NumConst { numConstTy = ty, numConstVal = val } =
    s `hashWithSalt` (9 :: Word) `hashWithSalt` ty `hashWithSalt` val
  hashWithSalt s (LValue lval) =
    s `hashWithSalt` (10 :: Word) `hashWithSalt` lval
  hashWithSalt _ (GCAlloc _ _ _) = error "GCAlloc is going away"

instance Hashable Stm where
  hashWithSalt s Move { moveSrc = src, moveDst = dst } = 
    s `hashWithSalt` (1 :: Word) `hashWithSalt` src `hashWithSalt` dst
  hashWithSalt s (Do val) = s `hashWithSalt` (2 :: Word) `hashWithSalt` val

instance Hashable Bind where
  hashWithSalt s Phi { phiName = name1, phiVals = vals1 } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` name1 `hashWithSalt` vals1
  hashWithSalt s Bind { bindName = name1, bindVal = val1 } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` name1 `hashWithSalt` val1
  hashWithSalt s (Effect e1) =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` e1

instance Hashable1 Block where
  hashWithSalt1 s Block { blockBody = body, blockXfer = xfer } =
    (s `hashWithSalt` xfer) `hashWithSalt1` body

instance Hashable elem => Hashable (Block elem) where
  hashWithSalt = hashWithSalt1

instance Hashable Label where
  hashWithSalt s (Label node) = s `hashWithSalt` node

instance Hashable Id where
  hashWithSalt s (Id name) = s `hashWithSalt` name

instance Hashable Globalname where
  hashWithSalt s (Globalname name) = s `hashWithSalt` name

instance Hashable Fieldname where
  hashWithSalt s (Fieldname name) = s `hashWithSalt` name

instance Format Label where
  format (Label l) = "L" <> l

instance Format Fieldname where
  format (Fieldname f) = "f" <> f

instance Format Id where
  format (Id v) = "%" <> v

instance Format Globalname where
  format (Globalname g) = "@" <> g 
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
      formatExp (StructConst ty fields) =
        let
          headerdoc = "const" <+> formatType ty
        in
          braceBlock headerdoc (punctuate comma (map formatExp (elems fields)))
      formatExp (ArrayConst ty inits) =
        let
          headerdoc = "const" <+> formatType ty
        in
          braceBlock headerdoc (punctuate comma (map formatExp inits))
      formatExp (NumConst ty n) = hang (formatType ty) 2 (format n)

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

instance Show Id where
  show (Id v) = "%" ++ show v

instance Show Globalname where
  show (Globalname g) = "@" ++ show g 
{-
instance Graph gr => Show (Module gr) where
  show = show . format
-}

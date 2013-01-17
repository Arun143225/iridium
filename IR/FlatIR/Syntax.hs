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
--   * Redesign the language so that compilation relies more on types
--   * Add notions of vtables and lookups to the language
--   * Add variant types
--   * Redesign/modify certain instructions (Deref, Call, Cast, Alloc)
--   * Add exception handling
--   * Add support for static linking
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
       Transfer(..),

       -- ** Definitions
       Block(..),
       Body(..),
       Global(..),
       Module(..)
       ) where

import Data.Array
import Data.Graph.Inductive.Graph
--import Data.Graph.Inductive.Query.DFS
import Data.Hash
import Data.Maybe
import Data.Pos
import Data.Word
import IR.Common.Ptr
import IR.Common.Operator
import Prelude hiding (head)
--import Prelude.Extras(Eq1, Ord1)
--import Text.Format

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
  -- | An integer, possibly signed, with a size
  | IntType {
      -- | Whether or not the int is signed
      intSigned :: !Bool,
      -- | The size of the int in bits
      intSize ::  !Word,
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
    deriving (Ord, Eq)

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
    deriving (Ord, Eq)

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
    deriving (Ord, Eq)

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
    deriving (Ord, Eq)

-- | A statement.  Represents an effectful action.
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
    deriving (Ord, Eq)

-- | A basic block
data Block =
    Block {
      -- | The statements in the basic block
      blockStms :: [Stm],
      -- | The transfer for the basic block
      blockXfer :: Transfer,
      -- | The position in source from which this arises.
      blockPos :: !Pos
    }
    deriving (Ord, Eq)

-- There is no straightforward ordering, equality, or hashing on the
-- remaining types.

-- | The body of a function
data Body gr =
    Body {
      -- | The entry block
      bodyEntry :: Label,
      -- | The CFG
      bodyCFG :: (gr Block ())
    }

-- | A global value.  Represents a global variable or a function.
data Global gr =
  -- | A function
    Function {
      -- | Name of the function
      funcName :: !String,
      -- | Return type
      funcRetTy :: Type,
      -- | A map from identifiers for arguments and local variables to
      -- their types
      funcValTys :: Array Id Type,
      -- | A list of the identifiers representing arguments
      funcParams :: [Id],
      -- | The function's body, if it has one
      funcBody :: Maybe (Body gr),
      -- | The position in source from which this arises.
      funcPos :: !Pos
    }
  -- | A global variable
  | GlobalVar {
      -- | The name of the variable.
      gvarName :: !String,
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
data Module gr =
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
      modGlobals :: Array Globalname (Global gr),
      -- | The position in source from which this arises.  This is here
      -- solely to record filenames in a unified way.
      modPos :: !Pos
    }

instance Position Type where
  pos (FuncType { funcTyPos = p }) = p
  pos (StructType { structPos = p }) = p
  pos (ArrayType { arrayPos = p }) = p
  pos (PtrType { ptrPos = p }) = p
  pos (IntType { intPos = p }) = p
  pos (IdType { idPos = p }) = p
  pos (FloatType { floatPos = p }) = p
  pos (UnitType p) = p

instance Position Exp where
  pos (Binop { binopPos = p }) = p
  pos (Call { callPos = p }) = p
  pos (Conv { convPos = p }) = p
  pos (Cast { castPos = p }) = p
  pos (Unop { unopPos = p }) = p
  pos (AddrOf { addrofPos = p }) = p
  pos (StructConst { structConstPos = p }) = p
  pos (ArrayConst { arrayConstPos = p }) = p
  pos (NumConst { numConstPos = p }) = p
  pos (LValue l) = pos l
  pos (GCAlloc _ _ _) = error "GCAlloc is going away"

instance Position LValue where
  pos (Index { idxPos = p }) = p
  pos (Field { fieldPos = p }) = p
  pos (Deref { derefPos = p }) = p
  pos (Var { varPos = p }) = p
  pos (Global { globalPos = p }) = p

instance Position Transfer where
  pos (Goto { gotoPos = p }) = p
  pos (Case { casePos = p }) = p
  pos (Ret { retPos = p }) = p
  pos (Unreachable p) = p

instance Position Stm where
  pos (Move { movePos = p }) = p
  pos (Do expr) = pos expr

instance Position Block where
  pos (Block { blockPos = p }) = p

instance Position (Global gr) where
  pos (Function { funcPos = p }) = p
  pos (GlobalVar { gvarPos = p }) = p

instance Position (Module gr) where
  pos (Module { modPos = p }) = p

instance Hashable Typename where
  hash (Typename n) = hash n

instance Hashable GCHeader where
  hash (GCHeader n) = hash n

instance Hashable Type where
  hash (FuncType { funcTyRetTy = retty, funcTyArgTys = params }) =
    hashInt 0 `combine` hash retty `combine` hash params
  hash (StructType { structPacked = packed, structFields = fields }) =
    hashInt 1 `combine` hash packed `combine` hashFoldable fields
  hash (ArrayType { arrayLen = Nothing, arrayElemTy = inner }) =
    hashInt 2 `combine` hashInt 0 `combine` hash inner
  hash (ArrayType { arrayLen = Just size, arrayElemTy = inner }) =
    hashInt 2 `combine` hash size `combine` hash inner
  hash (PtrType { ptrTy = objtype }) = hashInt 3 `combine` hash objtype
  hash (IntType { intSigned = signed, intSize = size }) =
    hashInt 4 `combine` hash signed `combine` hash size
  hash (IdType { idName = Typename str }) = hash str
  hash (FloatType { floatSize = size }) = hashInt 5 `combine` hash size
  hash (UnitType {}) = hashInt 6

instance Hashable Transfer where
  hash (Goto { gotoLabel = label }) = hashInt 1 `combine` hash label
  hash (Case { caseVal = val, caseCases = cases, caseDefault = def }) =
    hashInt 2 `combine` hash val `combine` hash cases `combine` hash def
  hash (Ret { retVal = val }) = hashInt 3 `combine` hash val
  hash (Unreachable _) = hashInt 4

instance Hashable LValue where
  hash (Index { idxVal = val, idxIndex = idx }) =
    hashInt 1 `combine` hash val `combine` hash idx
  hash (Field { fieldVal = val, fieldName = name }) =
    hashInt 2 `combine` hash val `combine` hash name
  hash (Deref { derefVal = val }) = hashInt 3 `combine` hash val
  hash (Var { varName = name }) = hashInt 4 `combine` hash name
  hash (Global { globalName = name }) = hashInt 5 `combine` hash name

instance Hashable Exp where
  hash (Binop { binopOp = op, binopLeft = left, binopRight = right }) =
    hashInt 1 `combine` hash op `combine` hash left `combine` hash right
  hash (Call { callFunc = func, callArgs = args }) =
    hashInt 2 `combine` hash func `combine` hash args
  hash (Unop { unopOp = op, unopVal = val }) =
    hashInt 3 `combine` hash op `combine` hash val
  hash (Conv { convTy = ty, convVal = val }) =
    hashInt 4 `combine` hash ty `combine` hash val
  hash (Cast { castTy = ty, castVal = val }) =
    hashInt 5 `combine` hash ty `combine` hash val
  hash (AddrOf { addrofVal = val }) = hashInt 6 `combine` hash val
  hash (StructConst { structConstTy = ty, structConstFields = fields }) =
    foldr combine (hashInt 7 `combine` hash ty) (map hash (elems fields))
  hash (ArrayConst { arrayConstTy = ty, arrayConstVals = vals }) =
    hashInt 8 `combine` hash ty `combine` hash vals
  hash (NumConst { numConstTy = ty, numConstVal = val }) =
    hashInt 9 `combine` hash ty `combine` hash val
  hash (LValue lval) = hashInt 10 `combine` hash lval
  hash (GCAlloc _ _ _) = error "GCAlloc is going away"

instance Hashable Stm where
  hash (Move { moveSrc = src, moveDst = dst }) = 
    hashInt 1 `combine` hash src `combine` hash dst
  hash (Do val) = hashInt 2 `combine` hash val

instance Hashable Block where
  hash (Block { blockStms = stms, blockXfer = xfer }) =
    hash stms `combine` hash xfer

instance Hashable Label where
  hash (Label node) = hash node

instance Hashable Id where
  hash (Id name) = hash name

instance Hashable Globalname where
  hash (Globalname name) = hash name

instance Hashable Fieldname where
  hash (Fieldname name) = hash name
{-
instance Format Label where
  format (Label l) = "L" <> l

instance Format Fieldname where
  format (Fieldname f) = "f" <> f

instance Format Id where
  format (Id v) = "%" <> v

instance Format Globalname where
  format (Globalname g) = "@" <> g 

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

instance Show Label where
  show (Label l) = "L" ++ show l

instance Show Fieldname where
  show (Fieldname f) = "f" ++ show f

instance Show Id where
  show (Id v) = "%" ++ show v

instance Show Globalname where
  show (Globalname g) = "@" ++ show g 

instance Show Unop where
  show = show . format

instance Show Binop where
  show = show . format

instance Graph gr => Show (Module gr) where
  show = show . format
-}
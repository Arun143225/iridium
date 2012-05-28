{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
-- Copyright (c) 2012 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
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

-- | This module defines the SimpleIR language.
--
-- SimpleIR is a simply-typed flat-scoped intermediate language.  It
-- is designed to be reasonably close to LLVM, with instructions
-- similar to LLVM's, but without some of the difficulties of LLVM.
--
-- At the moment, the SimpleIR language is under revision, and will
-- probably change quite a bit.
--
-- Things that need to be done:
--   * Redesign the language so that compilation relies more on types
--   * Add notions of vtables and lookups to the language
--   * Add variant types
--   * Redesign/modify certain instructions (Deref, Call, Cast, Alloc)
--   * Add exception handling
--   * Add support for static linking
module SimpleIR(
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
       Mobility(..),
       PtrClass(..),
       Mutability(..),

       -- * Core language

       -- ** Types
       Type(..),
       ObjType(..),

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
import Data.Graph.Inductive.Query.DFS
import Data.Hash
import Data.Maybe
import Data.Word
import Prelude hiding (head)
import Text.Format


-- SimpleIR is a simply-typed IR intended to be close to LLVM, but not
-- in SSA form.  It is intended primarily as a jumping-off point for
-- other languages targeting LLVM.

-- Programs in SimpleIR are equipped with very detailed information
-- about garbage collection.  This is passed through to LLVM in the
-- form of metadata.

-- SimpleIR also contains a virtual call abstraction, which allows
-- polymorphic languages to compile to SimpleIR without having to
-- monomorphise everything. XXX IMPLEMENT THIS

-- SimpleIR will eventually contain transaction information.

-- In general, any optimization pass that would be written for
-- SimpleIR should instead be written for LLVM, unless there is a very
-- compelling reason for it.  Examples would be optimizations that
-- deal with GC or virtual calls (or eventually transactions).

-- | Types.  Types are monomorphic, and correspond roughly with LLVM
-- types. XXX add a variant type
data Type =
  -- | A function type
    FuncType Type [Type]
  -- | A structure, representing both tuples and records
  | StructType !Bool (Array Fieldname (String, Mutability, Type))
  -- | An array.  Unlike LLVM arrays, these may be variable-sized
  | ArrayType !(Maybe Word) Type
  -- | Pointers, both native and GC
  | PtrType !ObjType
  -- | An integer, possibly signed, with a size
  | IntType !Bool !Word
  -- | A defined type
  | IdType !Typename
  -- | Floating point types
  | FloatType !Word
  -- | The unit type, equivalent to SML unit and C/Java void
  | UnitType
    deriving Eq

-- | Object mobility.  All native objects are immobile.  GC objects
-- can be mobile or immobile.  Immobile objects must be supported for
-- a sane FFI.
data Mobility =
  -- | The object's address may change during execution (specifically,
  -- due to garbage collection)
    Mobile
  -- | The object's address cannot change during execution.  Use to
  -- allocate buffers for IO, or objects for foreign calls.
  | Immobile
    deriving (Eq, Ord)

-- | Indicates the class of pointers.  This is relevant only to
-- pointers to grabage collected objects.
data PtrClass =
  -- | A strong GC pointer.  Acts as a "normal" pointer.
    StrongPtr
  -- | A soft GC pointer.  Any object which is reachable from the root
  -- set only by soft pointers or weaker pointers may have all such
  -- pointers cleared in response to memory pressure.
  | SoftPtr
  -- | A weak GC pointer.  Any object which is reachable only from the
  -- root set only by weak pointers will have all such pointer cleared
  -- during a collection cycle.
  | WeakPtr
  -- | A finalizer GC pointer.  When an object is reachable only by
  -- finalizers, it will result in the finalizer threads becoming
  -- runnable.
  | FinalPtr
  -- | A phantom GC pointer.  These should never be accessed by the
  -- program code, but will prevent an object's deletion during a
  -- collection cycle.
  | PhantomPtr
    deriving (Eq, Ord)

-- | Mutability of fields and objects.  Mutability, and particular
-- variants thereof are of paramount importance during garbage
-- collection.
data Mutability =
  -- | The field is immutable
    Immutable
  -- | The field is mutable
  | Mutable
  -- | The field can only be updated once (ie. initialized)
  | WriteOnce
  -- | The field's mutability depends on its (or other fields') value
  | Custom !String
    deriving (Eq, Ord)

-- | The type of object pointed to by a pointer
data ObjType =
  -- | An object in GC space
    GCObj !PtrClass !GCHeader
  -- | An object in non-GC space
  | BasicObj Type
    deriving Eq

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
    Goto !Label
  -- | A case expression
  | Case Exp [(Integer, Label)] !Label
  -- | A return
  | Ret (Maybe Exp)
  -- | An unreachable instruction, usually following a call with no
  -- return
  | Unreachable

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

-- | An assignable value
data LValue =
  -- | An array (or pointer) index
    Index Exp Exp
  -- | A field in a structure
  | Field Exp !Fieldname
  -- | Dereference a pointer
  | Deref Exp
  -- | A local value (local variable or argument)
  | Var !Id
  -- | A global value (global variable or function)
  | Global !Globalname

-- | An expression
data Exp =
  -- | Allocate an object whose type is described by the given header.
  -- XXX probably replace this with a general Alloc instruction,
  -- represeting GC allocation, malloc, and alloca.
    GCAlloc !GCHeader (Maybe Exp) (Maybe Exp)
  -- | A binary operation
  | Binop !Binop Exp Exp
  -- | Call a function.  XXX extend this with static link information.
  | Call Exp [Exp]
  -- | A unary operation
  | Unop !Unop Exp
  -- | A conversion from one type to another.
  | Conv Type Exp
  -- | Treat an expression as if it were the given type regardless of
  -- its actual type.
  | Cast Type Exp
  -- | An LValue
  | LValue LValue
  -- | Address of an LValue
  | AddrOf LValue
  -- | A structure constant
  | StructConst Type (Array Fieldname Exp)
  -- | An array constant
  | ArrayConst Type [Exp]
  -- | A numerical constant with a given size and signedness XXX add a
  -- floating point constant.
  | NumConst Type !Integer

-- | A statement.  Represents an effectful action.
data Stm =
  -- | Update the given lvalue
    Move LValue Exp
  -- | Execute an expression
  | Do Exp

-- | A basic block
data Block = Block [Stm] Transfer

-- | The body of a basic block
data Body gr =
  Body
    -- | The entry block
    Label
    -- | The CFG
    (gr Block ())

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
      funcBody :: Maybe (Body gr)
    }
  -- | A global variable
  | GlobalVar {
      -- | The name of the variable
      gvarName :: !String,
      -- | The type of the variable
      gvarTy :: Type,
      -- | The initializer
      gvarInit :: Maybe Exp
    }

-- | A module.  Represents a concept similar to an LLVM module.
data Module gr = Module {
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
  modGlobals :: Array Globalname (Global gr)
}

instance Hashable Mutability where
  hash Mutable = hashInt 0
  hash Immutable = hashInt 1
  hash WriteOnce = hashInt 2
  hash (Custom str) = hashInt 3 `combine` hash str

instance Hashable PtrClass where
  hash StrongPtr = hashInt 0
  hash SoftPtr = hashInt 1
  hash WeakPtr = hashInt 2
  hash FinalPtr = hashInt 3
  hash PhantomPtr = hashInt 4

instance Hashable Mobility where
  hash Mobile = hashInt 0
  hash Immobile = hashInt 1

instance Hashable Typename where
  hash (Typename n) = hash n

instance Hashable GCHeader where
  hash (GCHeader n) = hash n

instance Hashable ObjType where
  hash (GCObj ptrclass hdr) =
    hashInt 0 `combine` hash ptrclass `combine` hash hdr
  hash (BasicObj inner) = hashInt 1 `combine` hash inner

instance Hashable Type where
  hash (FuncType retty params) =
    hashInt 0 `combine` hash retty `combine` hash params
  hash (StructType packed fields) =
    hashInt 1 `combine` hash packed `combine` hashFoldable fields
  hash (ArrayType Nothing inner) =
    hashInt 2 `combine` hashInt 0 `combine` hash inner
  hash (ArrayType (Just size) inner) =
    hashInt 2 `combine` hash size `combine` hash inner
  hash (PtrType objtype) = hashInt 3 `combine` hash objtype
  hash (IntType signed size) =
    hashInt 4 `combine` hash signed `combine` hash size
  hash (IdType (Typename str)) = hash str
  hash (FloatType size) = hashInt 5 `combine` hash size
  hash UnitType = hashInt 6

instance Format Label where
  format (Label l) = "L" <> l

instance Format Fieldname where
  format (Fieldname f) = "f" <> f

instance Format Id where
  format (Id v) = "%" <> v

instance Format Globalname where
  format (Globalname g) = "@" <> g 

instance Format Unop where
  format Neg = format "neg"
  format NegNW = format "negnw"
  format Not = format "not"

instance Format Binop where
  format Add = format "add"
  format AddNW = format "addnw"
  format Sub = format "sub"
  format SubNW = format "subnw"
  format Mul = format "mul"
  format MulNW = format "mulnw"
  format Div = format "div"
  format Mod = format "mod"
  format Shl = format "shl"
  format Shr = format "shr"
  format And = format "and"
  format Or = format "or"
  format Xor = format "xor"
  format Eq = format "eq"
  format Neq = format "ne"
  format Ge = format "ge"
  format Le = format "le"
  format Gt = format "gt"
  format Lt = format "lt"
  format FOEq = format "foeq"
  format FONeq = format "foneq"
  format FOGe = format "foge"
  format FOLe = format "fole"
  format FOGt = format "fogt"
  format FOLt = format "folt"
  format FUEq = format "fueq"
  format FUNeq = format "funeq"
  format FUGe = format "fuge"
  format FULe = format "fule"
  format FUGt = format "fugt"
  format FULt = format "fult"

instance Format Mobility where
  format Mobile = format "mobile"
  format Immobile = format "immobile"

instance Format PtrClass where
  format StrongPtr = format "ref"
  format SoftPtr = format "softref"
  format WeakPtr = format "weakref"
  format FinalPtr = format "finalref"
  format PhantomPtr = format "phantomref"

instance Format Mutability where
  format Mutable = format "mutable"
  format Immutable = format "const"
  format WriteOnce = format "writeonce"
  format (Custom str) = format str

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
      formatGlobalname fname = "@" <> (funcName (globals ! fname))

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
      formatType (PtrType (BasicObj inner)) = formatType inner <> "*"
      formatType (PtrType (GCObj ptrclass hdr)) =
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
  show = show . format

instance Show Id where
  show = show . format

instance Show Unop where
  show = show . format

instance Show Binop where
  show = show . format

instance Show PtrClass where
  show = show . format

instance Show Mutability where
  show = show . format

instance Graph gr => Show (Module gr) where
  show = show . format
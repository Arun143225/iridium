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

-- | This module implements compilation of SimpleIR to LLVM.  Roughly
-- speaking, the process goes something like this:
--
-- 1) Convert all named types to LLVM types
-- 2) Generate metadata for generated GC types
-- 3) Generate declarations for all necessary accessors and modifiers
-- for GC types.
-- 4) Generate declarations for all globals, and convert their types.
-- 5) Compute dominance frontiers, and from that, phi-sets for each
-- function.
-- 6) Generate code for all functions.
--
-- What isn't implemented:
--   * Aggregate types as local variables.  Small aggregates can be
--     split up into many SSA values and handled that way.  Larger
--     aggregates need to be alloca'ed
--   * Escape analysis to figure out what needs to be alloca'ed
--   * Storing local variables in alloca'ed slots at all
--
-- Notes:
--   * For aggregates-as-values, insert a value in the value map
--     during final code generation describing the aggregate, and
--     mapping its fields to new values.  For phis, treat an
--     assignment to an aggregate as an assignment to all of its
--     fields.  Do this field expansion BEFORE creating the phi-sets
--   * Variants can be treated like any other aggregate.  Anytime we
--     assign a particular variant to a variant typed value, set all
--     the fields for the other variants to undef.
--   * Local variables should probably be annotated with a source.
--     This would allow static link accessed variables to be bound
--     properly.
--   * Functions could take an additional argument list representing
--     static linking.
module SimpleIR.LLVMGen(
       toLLVM
       ) where

import Data.Array(Array)
import Data.Array.IArray
import Data.Array.IO
import Data.Array.Unboxed(UArray)
import Data.BitArray.IO
import Data.Foldable
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.DomFrontier
import Data.Map(Map)
import Data.Maybe
import Data.Traversable
import Data.Tree
import Data.Word
import Foreign.Ptr
import Prelude hiding (mapM_, mapM, foldr, foldl, sequence)
import SimpleIR

import qualified Data.Map as Map
import qualified LLVM.BitWriter as LLVM
import qualified LLVM.Core as LLVM

booltype :: Type
booltype = IntType False 1

constant :: Bool -> Mutability -> Bool
constant _ Immutable = True
constant True _ = True
constant _ _ = False


-- | Generate LLVM IR from the SimpleIR module.
toLLVM :: Graph gr => Module gr -> IO LLVM.ModuleRef
toLLVM (Module { modName = name, modTypes = types, modGlobals = globals,
                 modGCHeaders = gcheaders, modGenGCs = gengcs }) =
  let
    -- First thing: run through all the named types, and generate
    -- LLVM.TypeRefs for all of them
    genTypeDefs :: LLVM.ContextRef -> IO (UArray Typename LLVM.TypeRef)
    genTypeDefs ctx =
      let
        -- Fill in the array of types
        initTypeArray :: IOUArray Typename LLVM.TypeRef -> IO ()
        initTypeArray typemap =
          let
            -- Translate a SimpleIR type into an LLVM type.  We need
            -- the map from typenames to (uninitialized) LLVM types to
            -- do this.
            genLLVMType :: Type -> IO LLVM.TypeRef
            genLLVMType (StructType packed fields) =
              do
                fieldtys <- mapM (\(_, _, ty) -> genLLVMType ty) (elems fields)
                LLVM.structTypeInContext ctx fieldtys packed
            genLLVMType (ArrayType (Just size) inner) =
              do
                inner <- genLLVMType inner
                return (LLVM.arrayType inner size)
            genLLVMType (ArrayType Nothing inner) =
              do
                inner <- genLLVMType inner
                return (LLVM.arrayType inner 0)
            genLLVMType (PtrType (BasicObj inner)) =
              do
                inner <- genLLVMType inner
                return (LLVM.pointerType inner 0)
            genLLVMType (PtrType (GCObj _ id)) =
              let
                (tname, _, _) = gcheaders ! id
              in do
                innerty <- updateEntry tname
                return (LLVM.pointerType innerty 0)
            genLLVMType (IdType id) = updateEntry id
            genLLVMType (IntType _ 1) = LLVM.int1TypeInContext ctx
            genLLVMType (IntType _ 8) = LLVM.int8TypeInContext ctx
            genLLVMType (IntType _ 16) = LLVM.int16TypeInContext ctx
            genLLVMType (IntType _ 32) = LLVM.int32TypeInContext ctx
            genLLVMType (IntType _ 64) = LLVM.int64TypeInContext ctx
            genLLVMType (IntType _ size) = LLVM.intTypeInContext ctx size
            genLLVMType (FloatType 32) = LLVM.floatTypeInContext ctx
            genLLVMType (FloatType 64) = LLVM.doubleTypeInContext ctx
            genLLVMType (FloatType 128) = LLVM.fp128TypeInContext ctx

            -- Grab the type entry for this type name, possibly
            -- (re)initializing it
            updateEntry :: Typename -> IO LLVM.TypeRef
            updateEntry ind =
              case types ! ind of
                (_, Just ty @ (StructType packed fields)) ->
                  do
                    ent <- readArray typemap ind
                    if LLVM.isOpaqueStruct ent
                      then do
                        fieldtys <- mapM (\(_, _, ty) -> genLLVMType ty)
                                         (elems fields)
                        LLVM.structSetBody ent fieldtys packed
                        return ent
                      else return ent
                (_, Just ty) ->
                  do
                    ent <- readArray typemap ind
                    if ent == nullPtr
                      then do
                        newty <- genLLVMType ty
                        writeArray typemap ind newty
                        return ent
                      else return ent
                _ -> readArray typemap ind
          in
            mapM_ (\ind -> updateEntry ind >> return ()) (indices types)

        -- Initialize structures and opaques to empty named structures and
        -- everything else to null pointers.
        initEntry :: (String, Maybe Type) -> IO LLVM.TypeRef
        initEntry (str, Nothing) =
          LLVM.structCreateNamed ctx str
        initEntry (str, Just (StructType _ _)) =
          LLVM.structCreateNamed ctx str
        initEntry _ = return nullPtr
      in do
        elems <- mapM initEntry (elems types)
        typearr <- newListArray (bounds types) elems
        initTypeArray typearr
        unsafeFreeze typearr

    -- Generate the metadata descriptors for all of the generated GC types
    genMetadata :: LLVM.ModuleRef -> LLVM.ContextRef -> IO ()
    genMetadata mod ctx =
      let
        mutabilityValue :: Mutability -> IO LLVM.ValueRef
        mutabilityValue Immutable = LLVM.mdString "const"
        mutabilityValue Mutable = LLVM.mdString "mutable"
        mutabilityValue WriteOnce = LLVM.mdString "writeonce"
        mutabilityValue (Custom str) = LLVM.mdString str

        mobilityValue :: Mobility -> IO LLVM.ValueRef
        mobilityValue Mobile = LLVM.mdString "mobile"
        mobilityValue Immobile = LLVM.mdString "immobile"

        ptrClassValue :: PtrClass -> IO LLVM.ValueRef
        ptrClassValue StrongPtr = LLVM.mdString "strong"
        ptrClassValue SoftPtr = LLVM.mdString "soft"
        ptrClassValue WeakPtr = LLVM.mdString "weak"
        ptrClassValue FinalPtr = LLVM.mdString "final"
        ptrClassValue PhantomPtr = LLVM.mdString "phantom"

        genFieldNode :: (String, Mutability, Type) -> IO LLVM.ValueRef
        genFieldNode (str, mut, ty) =
          do
            namemd <- LLVM.mdString str
            mutmd <- mutabilityValue mut
            tymd <- genTypedesc ty
            LLVM.mdNode [ namemd, mutmd, tymd ]

        genTypedesc :: Type -> IO LLVM.ValueRef
        genTypedesc (StructType True fields) =
          do
            classmd <- LLVM.mdString "struct"
            packedmd <- LLVM.mdString "packed"
            fieldnodes <- mapM genFieldNode (elems fields)
            LLVM.mdNode (classmd : packedmd : fieldnodes)
        genTypedesc (StructType False fields) =
          do
            classmd <- LLVM.mdString "struct"
            packedmd <- LLVM.mdString "nonpacked"
            fieldnodes <- mapM genFieldNode (elems fields)
            LLVM.mdNode (classmd : packedmd : fieldnodes)
        genTypedesc (ArrayType (Just size) inner) =
          do
            classmd <- LLVM.mdString "array"
            innernode <- genTypedesc inner
            LLVM.mdNode [ classmd, LLVM.constInt LLVM.int64Type size False,
                          innernode ]
        genTypedesc (ArrayType Nothing inner) =
          do
            classmd <- LLVM.mdString "array"
            innernode <- genTypedesc inner
            LLVM.mdNode [ classmd,
                          LLVM.constInt LLVM.int64Type 0 False, innernode ]
        genTypedesc (PtrType (BasicObj inner)) =
          do
            classmd <- LLVM.mdString "nativeptr"
            innernode <- genTypedesc inner
            LLVM.mdNode [ classmd, innernode ]
        genTypedesc (PtrType (GCObj ptrclass header)) =
          let
            (tname, mob, innermut) = gcheaders ! header
            (_, Just inner) = types ! tname
          in do
            classmd <- LLVM.mdString "gcptr"
            mobmd <- mobilityValue mob
            ptrclassmd <- ptrClassValue ptrclass
            innernode <- genTypedesc inner
            LLVM.mdNode [ classmd, ptrclassmd, mobmd, innernode ]
        genTypedesc (IntType _ size) =
          do
            classmd <- LLVM.mdString "int"
            LLVM.mdNode [ classmd, LLVM.constInt LLVM.int32Type size False ]
        genTypedesc (IdType tname) =
          let
            (str, _) = types ! tname
          in do
            classmd <- LLVM.mdString "named"
            mdstr <- LLVM.mdString str
            LLVM.mdNode [ classmd, mdstr ]
        genTypedesc (FloatType 32) =
          do
            classmd <- LLVM.mdString "float"
            LLVM.mdNode [ classmd ]
        genTypedesc (FloatType 64) =
          do
            classmd <- LLVM.mdString "double"
            LLVM.mdNode [ classmd ]
        genTypedesc (FloatType 128) =
          do
            classmd <- LLVM.mdString "fp128"
            LLVM.mdNode [ classmd ]

        genHeaderMD :: GCHeader -> IO ()
        genHeaderMD header =
          let
            (tname, mob, mut) = gcheaders ! header
            (str, Just ty) = types ! tname
          in do
            typedesc <- genTypedesc ty
            mdstr <- LLVM.mdString str
            mobmd <- mobilityValue mob
            mutmd <- mutabilityValue mut
            mdnode <- LLVM.mdNode [ mdstr, mobmd, mutmd, typedesc ]
            LLVM.addNamedMetadataOperand mod "core.gc.typedesc.md" mdnode
      in
        mapM_ genHeaderMD gengcs

    -- Generate an array mapping GCHeaders to llvm globals
    gcHeaders :: LLVM.ModuleRef -> LLVM.ContextRef ->
                 UArray Typename LLVM.TypeRef ->
                 IO (Array GCHeader LLVM.ValueRef)
    gcHeaders mod ctx typemap =
      let
        mobilityStr Mobile = "mobile"
        mobilityStr Immobile = "immobile"

        mutabilityStr Immutable = "const"
        mutabilityStr WriteOnce = "writeonce"
        mutabilityStr Mutable = "mutable"
        mutabilityStr (Custom str) = str

        mapfun :: LLVM.TypeRef -> (Typename, Mobility, Mutability) ->
                  IO LLVM.ValueRef
        mapfun hdrty (tname, mob, mut) =
          let
            (str, _) = types ! tname
            name = "core.gc.typedesc." ++ str ++ "." ++
              mobilityStr mob ++ "." ++ mutabilityStr mut
          in do
            val <- LLVM.addGlobal mod hdrty name
            LLVM.setGlobalConstant val True
            LLVM.setLinkage val LLVM.LinkerPrivateLinkage
            return val
      in do
        hdrty <- LLVM.structCreateNamed ctx "core.gc.typedesc"
        mapM (mapfun hdrty) gcheaders

    -- Now that we have the complete type array, we can properly
    -- translate types.
    toLLVMType :: LLVM.ContextRef -> UArray Typename LLVM.TypeRef -> Type ->
                  IO LLVM.TypeRef
    toLLVMType ctx types =
      let
        toLLVMType' :: Type -> IO LLVM.TypeRef
        toLLVMType' (StructType packed fields) =
          do
            fieldtys <- mapM (\(_, _, ty) -> toLLVMType' ty) (elems fields)
            LLVM.structTypeInContext ctx fieldtys packed
        toLLVMType' (ArrayType (Just size) inner) =
          do
            inner <- toLLVMType' inner
            return (LLVM.arrayType inner size)
        toLLVMType' (ArrayType Nothing inner) =
          do
            inner <- toLLVMType' inner
            return (LLVM.arrayType inner 0)
        toLLVMType' (PtrType (BasicObj inner)) =
           do
            inner <- toLLVMType' inner
            return (LLVM.pointerType inner 0)
        toLLVMType' (PtrType (GCObj _ id)) =
          let
            (tname, _, _) = gcheaders ! id
          in
            return (LLVM.pointerType (types ! tname) 0)
        toLLVMType' (IdType id) = return (types ! id)
        toLLVMType' (IntType _ 1) = LLVM.int1TypeInContext ctx
        toLLVMType' (IntType _ 8) = LLVM.int8TypeInContext ctx
        toLLVMType' (IntType _ 16) = LLVM.int16TypeInContext ctx
        toLLVMType' (IntType _ 32) = LLVM.int32TypeInContext ctx
        toLLVMType' (IntType _ 64) = LLVM.int64TypeInContext ctx
        toLLVMType' (IntType _ size) = LLVM.intTypeInContext ctx size
        toLLVMType' (FloatType 32) = LLVM.floatTypeInContext ctx
        toLLVMType' (FloatType 64) = LLVM.doubleTypeInContext ctx
        toLLVMType' (FloatType 128) = LLVM.fp128TypeInContext ctx
      in
        toLLVMType'

    -- Run over all the global values, and generate declarations for
    -- them all.
    genDecl :: Graph gr => LLVM.ModuleRef -> LLVM.ContextRef ->
                           UArray Typename LLVM.TypeRef -> Global gr ->
                           IO LLVM.ValueRef
    genDecl mod ctx typedefs (Function { funcName = name, funcRetTy = resty,
                                         funcParams = args,
                                         funcValTys = scope }) =
      do
        argtys <- mapM (toLLVMType ctx typedefs . (!) scope) args
        resty <- toLLVMType ctx typedefs resty
        LLVM.addFunction mod name (LLVM.functionType resty argtys False)
    genDecl mod ctx typedefs (GlobalVar { gvarName = name, gvarTy = ty }) =
      do
        llvmty <- toLLVMType ctx typedefs ty
        LLVM.addGlobal mod llvmty name

    -- Generate the accessors and modifiers for the given type
    genAccModDecls :: LLVM.ModuleRef -> LLVM.ContextRef ->
                      UArray Typename LLVM.TypeRef -> IO ()
    genAccModDecls mod ctx typedefs =
      let
        genAccMods :: (Typename, (String, Maybe Type)) -> IO ()
        genAccMods (typename, (str, Just ty)) =
          let
            tyref = (typedefs ! typename)

            genDecls :: Bool -> Type -> String -> [LLVM.TypeRef] -> IO ()
            genDecls const ty name args =
              let
                readtype :: LLVM.TypeRef -> LLVM.TypeRef
                readtype resty = LLVM.functionType resty (reverse args) False

                writetype :: LLVM.TypeRef -> LLVM.TypeRef
                writetype resty =
                  LLVM.functionType LLVM.voidType (reverse (resty : args)) False
              in do
                resty <- toLLVMType ctx typedefs ty
                readfunc <- LLVM.addFunction mod (name ++ ".read")
                                                 (readtype resty)
                LLVM.addFunctionAttr readfunc LLVM.NoUnwindAttribute
                LLVM.addFunctionAttr readfunc LLVM.ReadOnlyAttribute
                LLVM.addFunctionAttr readfunc LLVM.AlwaysInlineAttribute
                if not const
                  then do
                    writefunc <- LLVM.addFunction mod (name ++ ".write")
                                                      (writetype resty)
                    LLVM.addFunctionAttr writefunc LLVM.NoUnwindAttribute
                    LLVM.addFunctionAttr writefunc LLVM.AlwaysInlineAttribute
                    return()
                  else return ()

            genAccMods' :: String -> Bool -> [LLVM.TypeRef] ->
                           (String, Mutability, Type) -> IO ()
            genAccMods' prefix const args (name, mut, StructType _ fields) =
              do
                mapM_ (genAccMods' (prefix ++ "." ++ name)
                                   (constant const mut) args) fields
            genAccMods' prefix const args (name, mut, ArrayType _ inner) =
              do
                genAccMods' prefix const (LLVM.int32Type : args)
                                         (name, mut, inner)
            genAccMods' prefix const args (name, mut, ty) =
              genDecls (constant const mut) ty (prefix ++ "." ++ name) args
          in do
            genAccMods' "core.types" False [tyref] (str, Mutable, ty)
            return ()
        genAccMods _ = return ()
      in
        mapM_ genAccMods (assocs types)

    -- Actually generate the definitions for all globals
    genDefs :: LLVM.ContextRef -> Array Globalname LLVM.ValueRef ->
               UArray Typename LLVM.TypeRef -> IO ()
    genDefs ctx decls typedefs =
      let
        getGlobalType :: Globalname -> Type
        getGlobalType name =
          case globals ! name of
            Function { funcRetTy = retty, funcValTys = valtys,
                       funcParams = params} ->
              FuncType retty (map ((!) valtys) params)
            GlobalVar { gvarTy = ty } -> ty

        -- Chase down references and get a concrete type (if it
        -- leads to an opaque type, then return the named type
        getActualType :: Type -> Type
        getActualType (IdType tyname) =
          case typedefs ! tyname of
            (_, Just ty) -> getActualType ty
            _ -> IdType tyname
        getActualType ty = ty

        genConstLValue :: LValue -> IO (LLVM.ValueRef, Type)
        genConstLValue =
          let
            genConstLValue' :: [Word] -> LValue -> IO (LLVM.ValueRef, Type)
            genConstLValue' fields (Field (LValue lval) (Fieldname field)) =
              genConstLValue' (field : fields) lval
            genConstLValue' fields (Field exp fname @ (Fieldname field)) =
              let
                foldfun (StructType _ fields) ind =
                  let
                    (_, _, ty) = fields ! (Fieldname ind)
                  in
                    ty

                indlist = field : fields
              in do
                (exp', ty) <- genConst exp
                val <- LLVM.constExtractValue exp' indlist
                return (val, foldl foldfun ty indlist)
            genConstLValue' _ (Deref _) =
              error "Dereference in constant initializer"
            genConstLValue' _ (Index _ _) =
              error "Index in constant initializer"
            genConstLValue' _ (Global _) =
              error "Global variable access in constant initializer"
            genConstLValue' _ (Var _) =
              error "Variable access in constant initializer"
          in
            genConstLValue' []

        genConstLValueAddr :: LValue -> IO (LLVM.ValueRef, Type)
        genConstLValueAddr lval =
          let
            getGEP :: [LLVM.ValueRef] -> LLVM.ValueRef -> LLVM.ValueRef
            getGEP [] val = val
            getGEP offsets val = LLVM.constGEP val (offsetVal 0 : offsets)

            genConstLValueAddr' :: [LLVM.ValueRef] -> LValue ->
                                   IO (LLVM.ValueRef, Type)
            genConstLValueAddr' offsets (Field (LValue lval)
                                               fname @ (Fieldname field)) =
              do
                (val, ty) <-
                  genConstLValueAddr' (offsetVal field : offsets) lval
                case ty of
                  StructType _ fields ->
                    let
                      (_, _, ty) = fields ! fname
                    in
                      return (val, ty)
            genConstLValueAddr' _ (Field _ _) =
              error "addrof applied to non-addressable value"
            genConstLValueAddr' offsets (Index (LValue lval) ind) =
              do
                (ind', _) <- genConst ind
                (val, ty) <- genConstLValueAddr' (ind' : offsets) lval
                case ty of
                  ArrayType _ innerty -> return (val, innerty)
            genConstLValueAddr' offsets (Index exp ind) =
              do
                (ind', _) <- genConst ind
                (exp', ty) <- genConst exp
                case ty of
                  ArrayType _ innerty ->
                    return (LLVM.constGEP exp' (ind' : offsets), innerty)
            genConstLValueAddr' _ (Deref (LValue exp)) =
              error "Dereference in constant initializer"
            genConstLValueAddr' offsets (Deref exp) =
              do
                (exp', ty) <- genConst exp
                case ty of
                  PtrType (BasicObj ty) ->
                    return (getGEP offsets exp', ty)
                  PtrType (GCObj _ ind) ->
                    let
                      (tyname, _, _) = gcheaders ! ind
                      ty = case types ! tyname of
                        (_, Just ty) -> ty
                        _ -> IdType tyname
                    in
                      return (getGEP offsets exp', ty)
            genConstLValueAddr' offsets (Global g) =
              let
                ty = getGlobalType g
                val = getGEP offsets (decls ! g)
              in
                return (val, ty)
            genConstLValueAddr' _ (Var _) =
              error "Variable access in constant initializer"
          in do
            (val, ty) <- genConstLValueAddr' [] lval
            return (val, PtrType (BasicObj ty))

        -- Generate a constant initializer for a global variable
        genConst :: Exp -> IO (LLVM.ValueRef, Type)
        genConst (Binop op l r) =
          do
            (l', lty) <- genConst l
            (r', rty) <- genConst r
            case (op, lty, rty) of
              (Add, IntType _ _, IntType _ _) ->
                return (LLVM.constAdd l' r', lty)
              (Add, FloatType _, FloatType _) ->
                return (LLVM.constFAdd l' r', lty)
              (AddNW, IntType True _, IntType True _) ->
                do
                  out <- LLVM.constNSWAdd l' r'
                  return (out, lty)
              (AddNW, IntType False _, IntType False _) ->
                do
                  out <- LLVM.constNUWAdd l' r'
                  return (out, lty)
              (Sub, IntType _ _, IntType _ _) ->
                return (LLVM.constSub l' r', lty)
              (Sub, FloatType _, FloatType _) ->
                return (LLVM.constFSub l' r', lty)
              (SubNW, IntType True _, IntType True _) ->
                do
                  out <- LLVM.constNSWSub l' r'
                  return (out, lty)
              (SubNW, IntType False _, IntType False _) ->
                do
                  out <- LLVM.constNUWSub l' r'
                  return (out, lty)
              (Mul, IntType _ _, IntType _ _) ->
                return (LLVM.constMul l' r', lty)
              (Mul, FloatType _, FloatType _) ->
                return (LLVM.constFMul l' r', lty)
              (MulNW, IntType True _, IntType True _) ->
                do
                  out <- LLVM.constNSWMul l' r'
                  return (out, lty)
              (MulNW, IntType False _, IntType True _) ->
                do
                  out <- LLVM.constNUWMul l' r'
                  return (out, lty)
              (Div, IntType True _, IntType True _) ->
                return (LLVM.constUDiv l' r', lty)
              (Div, IntType False _, IntType False _) ->
                return (LLVM.constSDiv l' r', lty)
              (Div, FloatType _, FloatType _) ->
                return (LLVM.constFDiv l' r', lty)
              (Mod, IntType True _, IntType True _) ->
                return (LLVM.constURem l' r', lty)
              (Mod, IntType False _, IntType False _) ->
                return (LLVM.constSRem l' r', lty)
              (Mod, FloatType _, FloatType _) ->
                return (LLVM.constFRem l' r', lty)
              (And, IntType _ _, IntType _ _) ->
                return (LLVM.constAnd l' r', lty)
              (Or, IntType _ _, IntType _ _) ->
                return (LLVM.constOr l' r', lty)
              (Xor, IntType _ _, IntType _ _) ->
                return (LLVM.constXor l' r', lty)
              (Shl, IntType _ _, IntType _ _) ->
                return (LLVM.constShl l' r', lty)
              (Shr, IntType True _, IntType _ _) ->
                return (LLVM.constAShr l' r', lty)
              (Shr, IntType False _, IntType _ _) ->
                return (LLVM.constLShr l' r', lty)
              (Eq, IntType _ _, IntType _ _) ->
                return (LLVM.constICmp LLVM.IntEQ l' r',
                        booltype)
              (Neq, IntType _ _, IntType _ _) ->
                return (LLVM.constICmp LLVM.IntNE l' r',
                        booltype)
              (Ge, IntType True _, IntType True _) ->
                return (LLVM.constICmp LLVM.IntSGE l' r',
                        booltype)
              (Ge, IntType False _, IntType False _) ->
                return (LLVM.constICmp LLVM.IntUGE l' r',
                        booltype)
              (Gt, IntType False _, IntType False _) ->
                return (LLVM.constICmp LLVM.IntUGT l' r',
                        booltype)
              (Gt, IntType True _, IntType True _) ->
                return (LLVM.constICmp LLVM.IntSGT l' r',
                        booltype)
              (Le, IntType False _, IntType False _) ->
                return (LLVM.constICmp LLVM.IntULE l' r',
                        booltype)
              (Le, IntType True _, IntType True _) ->
                return (LLVM.constICmp LLVM.IntSLE l' r',
                        booltype)
              (Lt, IntType False _, IntType False _) ->
                return (LLVM.constICmp LLVM.IntULT l' r',
                        booltype)
              (Lt, IntType True _, IntType True _) ->
                return (LLVM.constICmp LLVM.IntSLT l' r',
                        booltype)
              (FOEq, FloatType _, FloatType _) ->
                return (LLVM.constFCmp LLVM.RealOEQ l' r',
                        booltype)
              (FONeq, FloatType _, FloatType _) ->
                return (LLVM.constFCmp LLVM.RealONE l' r',
                        booltype)
              (FOGe, FloatType _, FloatType _) ->
                return (LLVM.constFCmp LLVM.RealOGE l' r',
                        booltype)
              (FOGt, FloatType _, FloatType _) ->
                return (LLVM.constFCmp LLVM.RealOGT l' r',
                        booltype)
              (FOLe, FloatType _, FloatType _) ->
                return (LLVM.constFCmp LLVM.RealOLE l' r',
                        booltype)
              (FOLt, FloatType _, FloatType _) ->
                return (LLVM.constFCmp LLVM.RealOLT l' r',
                        booltype)
              (FUEq, FloatType _, FloatType _) ->
                return (LLVM.constFCmp LLVM.RealUEQ l' r',
                        booltype)
              (FUNeq, FloatType _, FloatType _) ->
                return (LLVM.constFCmp LLVM.RealUNE l' r',
                        booltype)
              (FUGe, FloatType _, FloatType _) ->
                return (LLVM.constFCmp LLVM.RealUGE l' r',
                        booltype)
              (FUGt, FloatType _, FloatType _) ->
                return (LLVM.constFCmp LLVM.RealUGT l' r',
                        booltype)
              (FULe, FloatType _, FloatType _) ->
                return (LLVM.constFCmp LLVM.RealULE l' r',
                        booltype)
              (FULt, FloatType _, FloatType _) ->
                return (LLVM.constFCmp LLVM.RealULT l' r',
                        booltype)
        genConst (Unop op inner) =
          do
            (inner', ty) <- genConst inner
            case (op, ty) of
              (Neg, IntType _ _) -> return (LLVM.constNeg inner', ty)
              (NegNW, IntType True _) ->
                do
                  out <- LLVM.constNSWNeg inner'
                  return (inner', ty)
              (NegNW, IntType False _) ->
                do
                  out <- LLVM.constNUWNeg inner'
                  return (inner', ty)
              (Neg, FloatType _) -> return (LLVM.constFNeg inner', ty)
              (Not, IntType _ _) -> return (LLVM.constNot inner', ty)
        genConst (AddrOf lval) = genConstLValueAddr lval
        genConst (LValue lval) = genConstLValue lval
        genConst (Conv toty inner) =
          do
            toty' <- toLLVMType ctx typedefs toty
            (inner', fromty) <- genConst inner
            case (fromty, toty) of
              (IntType False fromsize, IntType _ tosize) ->
                if fromsize > tosize
                  then return (LLVM.constTrunc inner' toty', toty)
                  else if fromsize < tosize
                    then return (LLVM.constZExt inner' toty', toty)
                    else return (inner', toty)
              (IntType True fromsize, IntType _ tosize) ->
                if fromsize > tosize
                  then return (LLVM.constTrunc inner' toty', toty)
                  else if fromsize < tosize
                    then return (LLVM.constSExt inner' toty', toty)
                    else return (inner', toty)
              (FloatType fromsize, FloatType tosize) ->
                if fromsize > tosize
                  then return (LLVM.constFPTrunc inner' toty', toty)
                  else if fromsize < tosize
                    then return (LLVM.constFPExt inner' toty', toty)
                    else return (inner', toty)
              (IntType False _, FloatType _) ->
                return (LLVM.constUIToFP inner' toty', toty)
              (IntType True _, FloatType _) ->
                return (LLVM.constSIToFP inner' toty', toty)
              (FloatType _, IntType False _) ->
                return (LLVM.constFPToUI inner' toty', toty)
              (FloatType _, IntType True _) ->
                return (LLVM.constFPToSI inner' toty', toty)
              (IntType True fromsize, PtrType _) ->
                return (LLVM.constIntToPtr inner' toty', toty)
              (PtrType _, IntType True fromsize) ->
                return (LLVM.constPtrToInt inner' toty', toty)
              (PtrType _, PtrType _) ->
                do
                  out <- LLVM.constPointerCast inner' toty'
                  return (out, toty)
        genConst (Cast toty inner) =
          do
            toty' <- toLLVMType ctx typedefs toty
            (inner', _) <- genConst inner
            return (LLVM.constBitCast inner' toty', toty)
        genConst (StructConst ty @ (StructType packed _) inits) =
          do
            inits' <- mapM (\field -> genConst field >>= return . fst)
                           (elems inits)
            return (LLVM.constStruct inits' packed, ty)
        genConst (ArrayConst ty inits) =
          do
            ty' <- toLLVMType ctx typedefs ty
            inits' <- mapM (\field -> genConst field >>= return . fst)
                           inits
            return (LLVM.constArray ty' inits', ty)
        genConst (NumConst ty @ (IntType signed _) n) =
          do
            ty' <- toLLVMType ctx typedefs ty
            return (LLVM.constInt ty' n signed, ty)
        genConst val = fail ("Cannot generate constant")

        -- Add a definition to a global.  This function does all the
        -- real work
        addDef :: Graph gr =>
                  LLVM.BuilderRef -> (Globalname, Global gr) -> IO ()
        -- Globals are pretty simple, generate the initializer and set
        -- it for the variable
        addDef _ (gname, GlobalVar { gvarName = name, gvarInit = Just exp }) =
          do
            (init, _) <- genConst exp
            LLVM.setInitializer (decls ! gname) init
        addDef builder
               (gname, Function { funcBody = Just (Body (Label entry) graph),
                                  funcValTys = valtys, funcParams = params }) =
          let
            func = (decls ! gname)
            [dfstree] = dff [entry] graph
            dfsnodes @ (entrynode : _) = foldr (:) [] dfstree
            range @ (startnode, endnode) = nodeRange graph
            nodelist = nodes graph
            (Id startid, Id endid) = bounds valtys
            valids = indices valtys

            -- First, map each CFG block to an LLVM basic block
            genBlocks :: IO (UArray Node LLVM.BasicBlockRef)
            genBlocks =
              let
                genBlock :: Node -> IO (Node, LLVM.BasicBlockRef)
                genBlock node =
                  do
                    block <- LLVM.appendBasicBlockInContext ctx func
                                                            ("L" ++ show node)
                    return (node, block)
              in do
                vals <- mapM genBlock nodelist
                return (array range vals)

            -- Build the sets of phi-values for each basic block
            buildPhiSets :: IO [(Node, [Id])]
            buildPhiSets =
              let
                domfronts' = domFrontiers graph entry

                domfronts :: Array Node [Node]
                domfronts = array range domfronts'

                getIndex :: Node -> Id -> Int
                getIndex node (Id id) =
                  let
                    node' = node - startnode
                    id' = (fromIntegral id) - (fromIntegral startid)
                    span = (fromIntegral endid) - (fromIntegral startid) + 1
                  in
                    (node' * span) + id'

                -- Add id to the phi-set for node
                addPhi :: IOBitArray -> Node -> Id -> IO ()
                addPhi arr node id =
                  let
                    domset = domfronts ! node
                    appfun node = writeBit arr (getIndex node id) True
                  in
                    mapM_ appfun domset

                -- Translate the bit array into a list of ids
                getPhiSet :: IOBitArray -> Node -> IO (Node, [Id])
                getPhiSet sets node =
                  let
                    foldfun :: [Id] -> Id -> IO [Id]
                    foldfun phiset id =
                      do
                        bit <- readBit sets (getIndex node id)
                        if bit
                          then return (id : phiset)
                          else return phiset
                  in do
                    front <- foldlM foldfun [] valids
                    return (node, front)

                -- Run through a node, add anything it modifies to the
                -- the phi-set of each node in its dominance frontier.
                buildPhiSet :: IOBitArray -> Node -> IO ()
                buildPhiSet modset node =
                  let
                    Just (Block stms _) = lab graph node

                    appfun (Move (Var id) _) = addPhi modset node id
                    appfun _ = return ()
                  in do
                    mapM_ appfun stms
              in do
                sets <- newBitArray ((getIndex startnode (Id startid)),
                                     (getIndex endnode (Id endid))) False
                mapM_ (buildPhiSet sets) nodelist
                mapM (getPhiSet sets) nodelist

            -- Generate the phi instructions required by a phi-set,
            -- add them to a phi-map array.
            genPhis :: UArray Node LLVM.BasicBlockRef ->
                       Array Id LLVM.TypeRef -> [(Node, [Id])] ->
                       IO (Array Node [(Id, LLVM.ValueRef)])
            genPhis blocks tyarr phiset =
              let
                mapblocks :: (Node, [Id]) -> IO (Node, [(Id, LLVM.ValueRef)])
                mapblocks (node, ids) =
                  let
                    mapvars :: Id -> IO (Id, LLVM.ValueRef)
                    mapvars id =
                      do
                        phi <- LLVM.buildPhi builder (tyarr ! id) ""
                        return (id, phi)                    
                  in do
                    LLVM.positionAtEnd builder (blocks ! node)
                    phis <- mapM mapvars ids
                    return (node, phis)
              in do
                vals <- mapM mapblocks phiset
                return (array (bounds blocks) vals)

            -- The initial value map contains just the arguments,
            -- and undefs for everything else.
            initValMap :: LLVM.BuilderRef -> IO (Word, ValMap)
            initValMap builder =
              let
                -- Add arguments to the value map
                addArg :: (Word, ValMap) -> (Int, Id) -> IO (Word, ValMap)
                addArg vmap (arg, id @ (Id ind)) =
                  let
                    paramty = valtys ! id
                    param = LLVM.getParam func arg

                    getArgVal :: LLVM.ValueRef -> Type -> (Word, ValMap) ->
                                 IO (Location, Word, ValMap)
                    getArgVal baseval (StructType _ fields) vmap =
                      let
                        foldfun (vmap, inds) (Fieldname field,
                                              (_, _, fieldty)) =
                          do
                            newbase <- LLVM.buildExtractValue builder
                                       baseval field ""
                            (loc, newind, valmap) <-
                              getArgVal newbase fieldty vmap
                            return ((newind + 1, Map.insert newind loc valmap),
                                    newind : inds)
                      in do
                        ((newind, valmap), fieldlist) <-
                          foldlM foldfun (vmap, []) (assocs fields)
                        return (Struct (listArray (bounds fields)
                                                  (reverse fieldlist)),
                                newind, valmap)
                    getArgVal baseval _ (newind, valmap) =
                      return (Local baseval, newind, valmap)
                  in do
                    (loc, newind, valmap) <- getArgVal param paramty vmap
                    return (newind, Map.insert ind loc valmap)

                getVal :: Type -> (Word, ValMap) -> IO (Location, Word, ValMap)
                getVal (StructType _ fields) vmap =
                  let
                    foldfun (vmap, inds) (Fieldname field, (_, _, fieldty)) =
                      do
                        (loc, newind, valmap) <- getVal fieldty vmap
                        return ((newind + 1, Map.insert newind loc valmap),
                                newind : inds)

                  in do
                    ((newind, valmap), fieldlist) <-
                      foldlM foldfun (vmap, []) (assocs fields)
                    return (Struct (listArray (bounds fields)
                                              (reverse fieldlist)),
                            newind, valmap)
                getVal ty (newind, valmap) =
                  do
                    ty' <- toLLVMType ctx typedefs ty
                    return (Local (LLVM.getUndef ty'), newind, valmap)

                -- Add undef values in for everything else
                addUndef :: (Word, ValMap) -> Id -> IO (Word, ValMap)
                addUndef vmap @ (_, valmap) id @ (Id ind) =
                  case Map.lookup ind valmap of
                    Just val -> return vmap
                    Nothing ->
                      do
                        (loc, newind, valmap) <- getVal (valtys ! id) vmap
                        return (newind, Map.insert ind loc valmap)

                (_, Id maxval) = bounds valtys
                init = (maxval + 1, Map.empty)
                arginds = zip [0..length params] params
              in do
                withArgs <- foldlM addArg init arginds
                foldlM addUndef withArgs (indices valtys)

            -- Generate the instructions for a basic block
            genInstrs :: UArray Node LLVM.BasicBlockRef ->
                         Array Node [(Id, LLVM.ValueRef)] ->
                         Array Id LLVM.TypeRef -> LLVM.BasicBlockRef ->
                         LLVM.BuilderRef -> ValMap -> IO ()
            genInstrs blocks phiarr tyarr entryblock builder valmap =
              let
                -- Take the value map, and add the incoming edges to a
                -- successor block.  Called when leaving a block on
                -- all its successors.
                bindPhis :: LLVM.BasicBlockRef -> ValMap -> Node -> IO ()
                bindPhis from valmap to =
                  let
                    phis = phiarr ! to
                    fromval = LLVM.basicBlockAsValue from

                    bindPhi :: (Id, LLVM.ValueRef) -> IO ()
                    bindPhi (Id ind, phival) =
                      let
                        Just (Local oldval) = Map.lookup ind valmap
                      in do
                        LLVM.addIncoming phival [(oldval, fromval)]
                  in
                    mapM_ bindPhi phis

                -- Replace all values with corresponding phis when
                -- entering a block.
                addPhiVals :: Node -> ValMap -> ValMap
                addPhiVals node valmap =
                  let
                    phis = phiarr ! node

                    addPhi :: (Id, LLVM.ValueRef) -> ValMap -> ValMap
                    addPhi (Id ind, phival) = Map.insert ind (Local phival)
                  in do
                    foldr addPhi valmap phis

                -- XXX Reinstate these functions, use the value map to
                -- figure out where to get values from.
{-
                -- Descend an LValue and call an access generator
                -- function on the results.  This allows the descent
                -- to be implemented only once.
                genLValueAccess :: (Map Id LLVM.ValueRef -> [Word] ->
                                    [LLVM.ValueRef] -> LLVM.ValueRef ->
                                    IO LLVM.ValueRef) ->
                                    Map Id LLVM.ValueRef -> LValue ->
                                    IO LLVM.ValueRef
                genLValueAccess access valmap =
                  let
                    genLValueAccess' :: [Word] -> [LLVM.ValueRef] -> LValue ->
                                        IO LLVM.ValueRef
                    genLValueAccess' [] offsets (Index (LValue lval) ind) =
                      do
                        ind' <- genExp valmap ind
                        genLValueAccess' [] (ind' : offsets) lval
                    genLValueAccess' fields offsets (Index (LValue lval) ind) =
                      let
                        fields' = map offsetVal fields
                      in do
                        ind' <- genExp valmap ind
                        genLValueAccess' [] (ind' : fields' ++ offsets) lval
                    genLValueAccess' [] offsets (Index exp ind) =
                      do
                        exp' <- genExp valmap exp
                        ind' <- genExp valmap ind
                        access valmap [] (ind' : offsets) exp'
                    genLValueAccess' fields offsets (Index exp ind) =
                      let
                        fields' = map offsetVal fields
                      in do
                        exp' <- genExp valmap exp
                        ind' <- genExp valmap ind
                        access valmap [] (ind' : fields' ++ offsets) exp'
                    genLValueAccess' fields offsets
                                     (Field (LValue lval) (Fieldname field)) =
                      genLValueAccess' (field : fields) offsets lval
                    genLValueAccess' fields offsets
                                     (Field exp (Fieldname field)) =
                      do
                        exp' <- genExp valmap exp
                        access valmap (field : fields) offsets exp'
                    genLValueAccess' [] offsets (Global name) =
                      access valmap [] (offsetVal 0 : offsets)
                             (decls ! name)
                    genLValueAccess' fields offsets (Global name) =
                      let
                        fields' = map offsetVal fields
                      in
                        access valmap fields (offsetVal 0 : fields' ++ offsets)
                               (decls ! name)
                    genLValueAccess' fields offsets (Var id) =
                      access valmap fields offsets
                             (fromJust (Map.lookup id valmap))
                  in
                    genLValueAccess' [] []
-}
                -- Generate addrof code
                genLValueAddr :: ValMap -> LValue -> IO (LLVM.ValueRef, Type)
                genLValueAddr valmap =
                  let
                    genLValueAddr' :: [LLVM.ValueRef] -> LValue ->
                                      IO (LLVM.ValueRef, Type)
                    genLValueAddr' [] (Deref inner) =
                      do
                        (inner', ty) <- genExp valmap inner
                        case getActualType ty of
                          PtrType _ -> return (inner', ty)
                    genLValueAddr' offsets (Deref inner) =
                      do
                        (inner', ty) <- genExp valmap inner
                        case getActualType ty of
                          PtrType (GCObj _ _) -> 
                            error "GCObject field addresses not implemented"
                          PtrType (BasicObj innerty) ->
                            do
                              val <- LLVM.buildGEP builder inner'
                                                   (offsetVal 0 : offsets) ""
                              return (val, ty)
                    genLValueAddr' offsets (Index arr ind) =
                      do
                        (ind', _) <- genExp valmap ind
                        (val, ty) <-
                          case arr of
                            LValue lval ->
                              genLValueAddr' (ind' : offsets) lval
                            _ ->
                              do
                                (arr', ty) <- genExp valmap arr
                                val <- LLVM.buildGEP builder arr'
                                                     (ind' : offsets) ""
                                return (val, ty)
                        case getActualType ty of
                          PtrType (GCObj _ _) -> 
                            error "GCObject field addresses not implemented"
                          PtrType (BasicObj arrty) ->
                            case getActualType arrty of
                              ArrayType _ innerty ->
                                return (val, PtrType (BasicObj innerty))
                          ArrayType _ innerty ->
                            return (val, PtrType (BasicObj innerty))
                    genLValueAddr' offsets (Field (LValue lval)
                                                  fname @ (Fieldname field)) =
                      do
                        (val, ty) <- genLValueAddr' (offsetVal field : offsets)
                                                    lval
                        case getActualType ty of
                          PtrType (GCObj _ _) -> 
                            error "GCObject field addresses not implemented"
                          PtrType (BasicObj innerty) ->
                            case getActualType innerty of
                              StructType _ fields ->
                                let
                                  (_, _, ty) = fields ! fname
                                in
                                  return (val, PtrType (BasicObj ty))
                    genLValueAddr' _ (Field _ _) =
                      error "Getting address of non-addressable value"
                    genLValueAddr' indexes (Global name) =
                      return (decls ! name,
                              PtrType (BasicObj (getGlobalType name)))
                    -- XXX Address of local variables ought to be done
                    -- a different way
                    genLValueAddr' indexes (Var id) =
                      do
                        (out, volatile) <- genVarAddr valmap builder indexes id
                        -- XXX plug the volatility into the pointer type
                        return (out, PtrType (BasicObj (valtys ! id)))
                  in
                    genLValueAddr' []

                genLValueRead :: ValMap -> LValue -> IO (LLVM.ValueRef, Type)
                genLValueRead valmap =
                  let
                    genLValueRead' :: [Index] -> LValue ->
                                      IO (LLVM.ValueRef, Type)
                    genLValueRead' indexes (Deref inner) =
                      do
                        (inner', ty) <- genExp valmap inner
                        case getActualType ty of
                          -- XXX For GC objects, create a table and look up accessor functions
                          PtrType (GCObj _ _) ->
                            error "Reading GC object fields not implemented"
                          PtrType (BasicObj innerty) ->
                            do
                              addr <- genGEP builder inner' indexes
                              out <- LLVM.buildLoad builder addr ""
                              return (out, innerty)
                    genLValueRead' offsets (Index (LValue lval) ind) =
                      do
                        (ind', _) <- genExp valmap ind
                        (out, ty) <- genLValueRead' (ValueInd ind : indexes) lval
                        case getActualType ty of
                          ArrayType _ innerty -> return (out, innerty)
                    genLValueRead' offsets (Field exp field) =
                      do
                        (ind', _) <- genExp valmap ind
                        (out, ty) <-
                          case exp of
                            LValue lval -> genLValueRead' (FieldInd field : indexes) lval
                            _ ->
                              do
                                (exp', ty) <- genExp valmap exp
                                
                        case getActualType ty of
                          StructType _ fields ->
                            let
                              (_, _, fieldty) = fields ! field
                            in
                              return (out, fieldty)
                    genLValueRead' indexes (Global name) =
                      do
                        addr <- getGEP builder (decls ! name)
                                       (map getOffsetValue indexes)
                        val <- LLVM.buildLoad builder val ""
                        -- XXX set volatile
                        return (val, getGlobalType name)
                    genLValueRead' indexes (Var id) =
                      do
                        val <- genVarRead valmap builder indexes id
                        return (val, valtys ! id)
                  in
                    genLValueRead' []

                genLValueWrite :: ValMap -> LLVM.ValueRef -> LValue -> IO ValMap
                genLValueWrite valmap val (Var (Id id)) =
                  case Map.lookup id valmap of
                    Just (Local _) ->
                      return (Map.insert id (Local val) valmap)
                    Just (Memory volatile addr) ->
                      do
                        store <- LLVM.buildStore builder val addr
                        LLVM.setVolatile store volatile
                        return valmap
                genLValueWrite valmap val (Global name) =
                  do
                    LLVM.buildStore builder val (decls ! name)
                    return valmap
                genLValueWrite valmap val lval =
                  error "complex lvalues disabled"
{-
                  do
                    genLValueAccess (accessWrite val) valmap lval
                    return valmap
-}

                -- Generate code for an expression
                genExp :: ValMap -> Exp -> IO (LLVM.ValueRef, Type)
                genExp valmap (Binop op l r) =
                  do
                    (l', lty) <- genExp valmap l
                    (r', rty) <- genExp valmap r
                    case (op, lty, rty) of
                      (Add, IntType _ _, IntType _ _) ->
                        do
                          out <- LLVM.buildAdd builder l' r' ""
                          return (out, lty)
                      (Add, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFAdd builder l' r' ""
                          return (out, lty)
                      (AddNW, IntType True _, IntType True _) ->
                        do
                          out <- LLVM.buildNSWAdd builder l' r' ""
                          return (out, lty)
                      (AddNW, IntType False _, IntType False _) ->
                        do
                          out <- LLVM.buildNUWAdd builder l' r' ""
                          return (out, lty)
                      (Sub, IntType _ _, IntType _ _) ->
                        do
                          out <- LLVM.buildSub builder l' r' ""
                          return (out, lty)
                      (Sub, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFSub builder l' r' ""
                          return (out, lty)
                      (SubNW, IntType True _, IntType True _) ->
                        do
                          out <- LLVM.buildNSWSub builder l' r' ""
                          return (out, lty)
                      (SubNW, IntType False _, IntType False _) ->
                        do
                          out <- LLVM.buildNUWSub builder l' r' ""
                          return (out, lty)
                      (Mul, IntType _ _, IntType _ _) ->
                        do
                          out <- LLVM.buildMul builder l' r' ""
                          return (out, lty)
                      (Mul, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFMul builder l' r' ""
                          return (out, lty)
                      (MulNW, IntType True _, IntType True _) ->
                        do
                          out <- LLVM.buildNSWMul builder l' r' ""
                          return (out, lty)
                      (MulNW, IntType False _, IntType False _) ->
                        do
                          out <- LLVM.buildNUWMul builder l' r' ""
                          return (out, lty)
                      (Div, IntType True _, IntType True _) ->
                        do
                          out <- LLVM.buildSDiv builder l' r' ""
                          return (out, lty)
                      (Div, IntType False _, IntType False _) ->
                        do
                          out <- LLVM.buildUDiv builder l' r' ""
                          return (out, lty)
                      (Div, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFDiv builder l' r' ""
                          return (out, lty)
                      (Mod, IntType True _, IntType True _) ->
                        do
                          out <- LLVM.buildSRem builder l' r' ""
                          return (out, lty)
                      (Mod, IntType False _, IntType False _) ->
                        do
                          out <- LLVM.buildURem builder l' r' ""
                          return (out, lty)
                      (Mod, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFRem builder l' r' ""
                          return (out, lty)
                      (And, IntType _ _, IntType _ _) ->
                        do
                          out <- LLVM.buildAnd builder l' r' ""
                          return (out, lty)
                      (Or, IntType _ _, IntType _ _) ->
                        do
                          out <- LLVM.buildOr builder l' r' ""
                          return (out, lty)
                      (Xor, IntType _ _, IntType _ _) ->
                        do
                          out <- LLVM.buildXor builder l' r' ""
                          return (out, lty)
                      (Shl, IntType _ _, IntType _ _) ->
                        do
                          out <- LLVM.buildShl builder l' r' ""
                          return (out, lty)
                      (Shr, IntType True _, IntType _ _) ->
                        do
                          out <- LLVM.buildAShr builder l' r' ""
                          return (out, lty)
                      (Shr, IntType False _, IntType _ _) ->
                        do
                          out <- LLVM.buildLShr builder l' r' ""
                          return (out, lty)
                      (Eq, IntType _ _, IntType _ _) ->
                        do
                          out <- LLVM.buildICmp builder (LLVM.IntEQ) l' r' ""
                          return (out, lty)
                      (Neq, IntType _ _, IntType _ _) ->
                        do
                          out <- LLVM.buildICmp builder (LLVM.IntNE) l' r' ""
                          return (out, lty)
                      (Ge, IntType True _, IntType True _) ->
                        do
                          out <- LLVM.buildICmp builder (LLVM.IntSGE) l' r' ""
                          return (out, booltype)
                      (Ge, IntType False _, IntType False _) ->
                        do
                          out <- LLVM.buildICmp builder (LLVM.IntUGE) l' r' ""
                          return (out, booltype)
                      (Gt, IntType True _, IntType True _) ->
                        do
                          out <- LLVM.buildICmp builder (LLVM.IntSGT) l' r' ""
                          return (out, booltype)
                      (Gt, IntType False _, IntType False _) ->
                        do
                          out <- LLVM.buildICmp builder (LLVM.IntUGT) l' r' ""
                          return (out, booltype)
                      (Le, IntType True _, IntType True _) ->
                        do
                          out <- LLVM.buildICmp builder (LLVM.IntSLE) l' r' ""
                          return (out, booltype)
                      (Le, IntType False _, IntType False _) ->
                        do
                          out <- LLVM.buildICmp builder (LLVM.IntULE) l' r' ""
                          return (out, booltype)
                      (Lt, IntType True _, IntType True _) ->
                        do
                          out <- LLVM.buildICmp builder (LLVM.IntSLT) l' r' ""
                          return (out, booltype)
                      (Lt, IntType False _, IntType False _) ->
                        do
                          out <- LLVM.buildICmp builder (LLVM.IntULT) l' r' ""
                          return (out, booltype)
                      (FOEq, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFCmp builder (LLVM.RealOEQ) l' r' ""
                          return (out, booltype)
                      (FONeq, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFCmp builder (LLVM.RealONE) l' r' ""
                          return (out, booltype)
                      (FOGe, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFCmp builder (LLVM.RealOGE) l' r' ""
                          return (out, booltype)
                      (FOLe, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFCmp builder (LLVM.RealOLE) l' r' ""
                          return (out, booltype)
                      (FOGt, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFCmp builder (LLVM.RealOGT) l' r' ""
                          return (out, booltype)
                      (FOLt, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFCmp builder (LLVM.RealOLT) l' r' ""
                          return (out, booltype)
                      (FUEq, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFCmp builder (LLVM.RealUEQ) l' r' ""
                          return (out, booltype)
                      (FUNeq, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFCmp builder (LLVM.RealUNE) l' r' ""
                          return (out, booltype)
                      (FUGe, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFCmp builder (LLVM.RealUGE) l' r' ""
                          return (out, booltype)
                      (FULe, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFCmp builder (LLVM.RealULE) l' r' ""
                          return (out, booltype)
                      (FUGt, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFCmp builder (LLVM.RealUGT) l' r' ""
                          return (out, booltype)
                      (FULt, FloatType _, FloatType _) ->
                        do
                          out <- LLVM.buildFCmp builder (LLVM.RealULT) l' r' ""
                          return (out, booltype)
                genExp valmap (Call func args) =
                  do
                    (func', FuncType retty _) <- genExp valmap func
                    args' <- mapM (\param -> genExp valmap param >>= return . fst)
                                  args
                    out <- LLVM.buildCall builder func' args' ""
                    return (out, retty)
                genExp valmap (Unop op inner) =
                  do
                    (inner', ty) <- genExp valmap inner
                    out <- case (op, ty) of
                      (Neg, IntType _ _) -> LLVM.buildNeg builder inner' ""
                      (Neg, FloatType _) -> LLVM.buildFNeg builder inner' ""
                      (NegNW, IntType True _) -> LLVM.buildNSWNeg builder inner' ""
                      (NegNW, IntType False _) -> LLVM.buildNUWNeg builder inner' ""
                      (Not, IntType _ _) -> LLVM.buildNot builder inner' ""
                    return (out, ty)
                genExp valmap (Conv toty inner) =
                  do
                    toty' <- toLLVMType ctx typedefs toty
                    (inner', fromty) <- genExp valmap inner
                    out <- case (fromty, toty) of
                      (IntType False fromsize, IntType _ tosize) ->
                        if fromsize > tosize
                          then LLVM.buildTrunc builder inner' toty' ""
                          else if fromsize < tosize
                            then LLVM.buildZExt builder inner' toty' ""
                            else return inner'
                      (IntType True fromsize, IntType _ tosize) ->
                        if fromsize > tosize
                          then LLVM.buildTrunc builder inner' toty' ""
                          else if fromsize < tosize
                            then LLVM.buildSExt builder inner' toty' ""
                            else return inner'
                      (FloatType fromsize, FloatType tosize) ->
                        if fromsize > tosize
                          then LLVM.buildFPTrunc builder inner' toty' ""
                          else if fromsize < tosize
                            then LLVM.buildFPExt builder inner' toty' ""
                            else return inner'
                      (IntType False _, FloatType _) ->
                        LLVM.buildUIToFP builder inner' toty' ""
                      (IntType True _, FloatType _) ->
                        LLVM.buildSIToFP builder inner' toty' ""
                      (FloatType _, IntType False _) ->
                        LLVM.buildFPToUI builder inner' toty' ""
                      (FloatType _, IntType True _) ->
                        LLVM.buildFPToSI builder inner' toty' ""
                      (IntType True fromsize, PtrType _) ->
                        LLVM.buildIntToPtr builder inner' toty' ""
                      (PtrType _, IntType True fromsize) ->
                        LLVM.buildPtrToInt builder inner' toty' ""
                      (PtrType _, PtrType _) ->
                        LLVM.buildPointerCast builder inner' toty' ""
                    return (out, toty)
                genExp valmap (LValue lval) = genLValueRead valmap lval
                genExp valmap (AddrOf lval) = genLValueAddr valmap lval
                genExp valmap (StructConst ty @ (StructType packed _) inits) =
                  do
                    inits' <- mapM (\param -> genExp valmap param >>= return . fst)
                                   (elems inits)
                    return (LLVM.constStruct inits' packed, ty)
                genExp valmap (ArrayConst ty inits) =
                  do
                    inits' <- mapM (\param -> genExp valmap param >>= return . fst)
                                   inits
                    ty' <- toLLVMType ctx typedefs ty
                    return (LLVM.constArray ty' inits', ty)
                genExp _ (NumConst ty @ (IntType signed _) n) =
                  do
                    ty' <- toLLVMType ctx typedefs ty
                    return (LLVM.constInt ty' n signed, ty)

                genStm :: ValMap -> Stm -> IO ValMap
                genStm valmap (Do e) =
                  do
                    genExp valmap e
                    return valmap
                genStm valmap (Move lval rval) =
                  do
                    (rval', _) <- genExp valmap rval
                    genLValueWrite valmap rval' lval

                -- Generate code for a transfer
                genTransfer :: ValMap -> Transfer -> IO LLVM.ValueRef
                genTransfer valmap (Goto (Label l)) =
                  LLVM.buildBr builder (blocks ! l)
                genTransfer valmap (Case test cases (Label def)) =
                  do
                    (testval, _) <- genExp valmap test
                    case cases of
                      [(0, Label falsel)] ->
                        LLVM.buildCondBr builder testval (blocks ! def)
                                         (blocks ! falsel)
                      _ ->
                        let
                          numcases = length cases

                          addCase :: LLVM.ValueRef -> (Integer, Label) ->
                                     IO ()
                          addCase switch (num, Label label) =
                            let
                              numval = LLVM.constInt LLVM.int32Type num False
                            in
                            LLVM.addCase switch numval (blocks ! label)
                        in do
                          switch <- LLVM.buildSwitch builder testval
                                                     (blocks ! def) numcases
                          mapM_ (addCase switch) cases
                          return switch
                genTransfer valmap (Ret (Just ret)) =
                  do
                    (retval, _) <- genExp valmap ret
                    LLVM.buildRet builder retval
                genTransfer valmap (Ret Nothing) = LLVM.buildRetVoid builder
                genTransfer valmap Unreachable = LLVM.buildUnreachable builder

                -- Traverse the CFG.  This takes a DFS tree as an argument.
                traverse :: LLVM.BasicBlockRef -> ValMap -> Tree Node -> IO ()
                traverse from invalmap (Node { rootLabel = curr,
                                               subForest = nexts }) =
                  let
                    valmap = addPhiVals curr invalmap
                    Just (Block stms trans) = lab graph curr
                    currblock = blocks ! curr
                    outs = suc graph curr
                  in do
                    LLVM.positionAtEnd builder currblock
                    valmap <- foldlM genStm valmap stms
                    genTransfer valmap trans
                    mapM_ (bindPhis currblock valmap) outs
                    mapM_ (traverse currblock valmap) nexts
              in do
                bindPhis entryblock valmap entrynode
                traverse entryblock valmap dfstree
          in do
            tyarr <- mapM (toLLVMType ctx typedefs) valtys
            entryblock <- LLVM.appendBasicBlockInContext ctx func "entry"
            blocks <- genBlocks
            LLVM.positionAtEnd builder entryblock
            (numvals, valmap) <- initValMap builder
            LLVM.buildBr builder (blocks ! entrynode)
            phiset <- buildPhiSets
            phiarr <- genPhis blocks tyarr phiset
            genInstrs blocks phiarr tyarr entryblock builder valmap
      in do
        builder <- LLVM.createBuilderInContext ctx
        mapM_ (addDef builder) (assocs globals)
  in do
    mod <- LLVM.moduleCreateWithName name
    ctx <- LLVM.getModuleContext mod
    typedefs <- genTypeDefs ctx
    gcheaderdecls <- gcHeaders mod ctx typedefs
    genMetadata mod ctx
    decls <- mapM (genDecl mod ctx typedefs) globals
    genAccModDecls mod ctx typedefs
    genDefs ctx decls typedefs
    return mod
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
--   * Type-driven compilation
module SimpleIR.LLVMGen(
       toLLVM
       ) where

import Data.Array
import Data.Array.IO
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
import Prelude hiding (mapM_, mapM, foldr)
import SimpleIR

import qualified Data.Map as Map
import qualified LLVM.BitWriter as LLVM
import qualified LLVM.Core as LLVM

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
    genTypeDefs :: LLVM.ContextRef -> IO (Array Typename LLVM.TypeRef)
    genTypeDefs ctx =
      let
        -- Fill in the array of types
        initTypeArray :: IOArray Typename LLVM.TypeRef -> IO ()
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
                 Array Typename LLVM.TypeRef ->
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
    toLLVMType :: LLVM.ContextRef -> Array Typename LLVM.TypeRef -> Type ->
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
                           Array Typename LLVM.TypeRef -> Global gr ->
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
                      Array Typename LLVM.TypeRef -> IO ()
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
               Array Typename LLVM.TypeRef -> IO ()
    genDefs ctx decls typedefs =
      let
        -- Generates a constant for use in GEP and extract/insertvalue
        offsetVal :: Word -> LLVM.ValueRef
        offsetVal off = LLVM.constInt LLVM.int32Type off False
{-
        genConstLValue :: LValue -> IO LLVM.ValueRef
        genConstLValue =
          let
            genConstLValue' :: [Word] -> LValue -> IO LLVM.ValueRef
            genConstLValue' fields (Field (LValue lval) (Fieldname field)) =
              genConstLValue' (field : fields) lval
            genConstLValue' fields (Field exp (Fieldname field)) =
              do
                exp' <- genConst exp
                return (LLVM.constExtractElement (field : fields) exp')
            genConstLValue' _ (Index _ _) =
              error "Index in constant initializer"
            genConstLValue' _ (Global _) =
              error "Global variable access in constant initializer"
            genConstLValue' _ (Var _) =
              error "Variable access in constant initializer"
          in
            genConstLValue' []

        genConstLValueAddr :: LValue -> IO LLVM.ValueRef
        genConstLValueAddr =
          let
            genConstLValueAddr' :: [LLVM.ValueRef] -> LValue -> IO LLVM.ValueRef
            genConstLValueAddr' offsets (Field (LValue lval)
                                               (Fieldname field)) =
              genConstLValueAddr' (offsetVal field : offsets) lval
            genConstLValueAddr' offsets (Index (LValue lval) ind) =
              do
                ind' <- genConst ind
                genConstLValueAddr' (ind' : offsets) lval
            genConstLValueAddr' offsets (Index exp ind) =
              do
                ind' <- genConst ind
                exp' <- genConst exp
                return (LLVM.constGEP (ind' : offsets) exp')
            genConstLValueAddr' [] (Global g) = return (decls ! g)
            genConstLValueAddr' offsets (Global g) =
              return (LLVM.constGEP (offsetVal 0 : offsets) (decls ! g))
            genConstLValueAddr' _ (Field _ _) =
              error "addrof applied to non-addressable value"
            genConstLValueAddr' _ (Var _) =
              error "Variable access in constant initializer"
          in
            genConstLValueAddr' []
-}
        -- Generate a constant initializer for a global variable
        genConst :: Exp -> IO LLVM.ValueRef
        genConst (Binop op l r) =
          do
            l' <- genConst l
            r' <- genConst r
            case op of
              Add -> return (LLVM.constAdd l' r')
              AddNSW -> LLVM.constNSWAdd l' r'
              AddNUW -> LLVM.constNUWAdd l' r'
              FAdd -> return (LLVM.constFAdd l' r')
              Sub -> return (LLVM.constSub l' r')
              SubNSW -> LLVM.constNSWSub l' r'
              SubNUW -> LLVM.constNUWSub l' r'
              FSub -> return (LLVM.constFSub l' r')
              Mul -> return (LLVM.constMul l' r')
              MulNSW -> LLVM.constNSWMul l' r'
              MulNUW -> LLVM.constNUWMul l' r'
              FMul -> return (LLVM.constFMul l' r')
              UDiv -> return (LLVM.constUDiv l' r')
              SDiv -> return (LLVM.constSDiv l' r')
              FDiv -> return (LLVM.constFDiv l' r')
              UMod -> return (LLVM.constURem l' r')
              SMod -> return (LLVM.constSRem l' r')
              FMod -> return (LLVM.constFRem l' r')
              And -> return (LLVM.constAnd l' r')
              Or -> return (LLVM.constOr l' r')
              Xor -> return (LLVM.constXor l' r')
              Shl -> return (LLVM.constShl l' r')
              AShr -> return (LLVM.constAShr l' r')
              LShr -> return (LLVM.constLShr l' r')
              Eq -> return (LLVM.constICmp LLVM.IntEQ l' r')
              Neq -> return (LLVM.constICmp LLVM.IntNE l' r')
              UGe -> return (LLVM.constICmp LLVM.IntUGE l' r')
              UGt -> return (LLVM.constICmp LLVM.IntUGT l' r')
              ULe -> return (LLVM.constICmp LLVM.IntULE l' r')
              ULt -> return (LLVM.constICmp LLVM.IntULT l' r')
              SGe -> return (LLVM.constICmp LLVM.IntSGE l' r')
              SGt -> return (LLVM.constICmp LLVM.IntSGT l' r')
              SLe -> return (LLVM.constICmp LLVM.IntSLE l' r')
              SLt -> return (LLVM.constICmp LLVM.IntSLT l' r')
              FOEq -> return (LLVM.constFCmp LLVM.RealOEQ l' r')
              FONeq -> return (LLVM.constFCmp LLVM.RealONE l' r')
              FOGe -> return (LLVM.constFCmp LLVM.RealOGE l' r')
              FOGt -> return (LLVM.constFCmp LLVM.RealOGT l' r')
              FOLe -> return (LLVM.constFCmp LLVM.RealOLE l' r')
              FOLt -> return (LLVM.constFCmp LLVM.RealOLT l' r')
              FUEq -> return (LLVM.constFCmp LLVM.RealUEQ l' r')
              FUNeq -> return (LLVM.constFCmp LLVM.RealUNE l' r')
              FUGe -> return (LLVM.constFCmp LLVM.RealUGE l' r')
              FUGt -> return (LLVM.constFCmp LLVM.RealUGT l' r')
              FULe -> return (LLVM.constFCmp LLVM.RealULE l' r')
              FULt -> return (LLVM.constFCmp LLVM.RealULT l' r')
        genConst (Unop op inner) =
          do
            inner' <- genConst inner
            case op of
              Neg -> return (LLVM.constNeg inner')
              NegNSW -> LLVM.constNSWNeg inner'
              NegNUW -> LLVM.constNUWNeg inner'
              FNeg -> return (LLVM.constFNeg inner')
              Not -> return (LLVM.constNot inner')
        genConst (AddrOf lval) = error "Complex lvalues disabled"
--genConstLValueAddr lval
        genConst (LValue lval) = error "Complex lvalues disabled"
--genConstLValue lval
        genConst (Conv fromty toty inner) =
          do
            toty' <- toLLVMType ctx typedefs toty
            inner' <- genConst inner
            case (fromty, toty) of
              (IntType False fromsize, IntType _ tosize) ->
                if fromsize > tosize
                  then return (LLVM.constTrunc inner' toty')
                  else if fromsize < tosize
                    then return (LLVM.constZExt inner' toty')
                    else return inner'
              (IntType True fromsize, IntType _ tosize) ->
                if fromsize > tosize
                  then return (LLVM.constTrunc inner' toty')
                  else if fromsize < tosize
                    then return (LLVM.constSExt inner' toty')
                    else return inner'
              (FloatType fromsize, FloatType tosize) ->
                if fromsize > tosize
                  then return (LLVM.constFPTrunc inner' toty')
                  else if fromsize < tosize
                    then return (LLVM.constFPExt inner' toty')
                    else return inner'
              (IntType False _, FloatType _) ->
                return (LLVM.constUIToFP inner' toty')
              (IntType True _, FloatType _) ->
                return (LLVM.constSIToFP inner' toty')
              (FloatType _, IntType False _) ->
                return (LLVM.constFPToUI inner' toty')
              (FloatType _, IntType True _) ->
                return (LLVM.constFPToSI inner' toty')
              (IntType True fromsize, PtrType _) ->
                return (LLVM.constIntToPtr inner' toty')
              (PtrType _, IntType True fromsize) ->
                return (LLVM.constPtrToInt inner' toty')
              (PtrType _, PtrType _) ->
                LLVM.constPointerCast inner' toty'
        genConst (StructConst (StructType packed _) inits) =
          do
            inits' <- mapM genConst (elems inits)
            return (LLVM.constStruct inits' packed)
        genConst (ArrayConst ty inits) =
          do
            ty' <- toLLVMType ctx typedefs ty
            inits' <- mapM genConst inits
            return (LLVM.constArray ty' inits')
        genConst (NumConst ty @ (IntType signed _) n) =
          do
            ty' <- toLLVMType ctx typedefs ty
            return (LLVM.constInt ty' n signed)
        genConst val = fail ("Cannot generate constant")

        -- Add a definition to a global.  This function does all the
        -- real work
        addDef :: Graph gr =>
                  LLVM.BuilderRef -> (Globalname, Global gr) -> IO ()
        -- Globals are pretty simple, generate the initializer and set
        -- it for the variable
        addDef _ (gname, GlobalVar { gvarName = name, gvarInit = Just exp }) =
          do
            init <- genConst exp
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
            genBlocks :: IO (Array Node LLVM.BasicBlockRef)
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
            genPhis :: Array Node LLVM.BasicBlockRef ->
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

            -- Generate the instructions for a basic block
            genInstrs :: Array Node LLVM.BasicBlockRef ->
                         Array Node [(Id, LLVM.ValueRef)] ->
                         Array Id LLVM.TypeRef ->
                         LLVM.BasicBlockRef -> LLVM.BuilderRef -> IO ()
            genInstrs blocks phiarr tyarr entryblock builder =
              let
                indexes = indices tyarr

                -- The initial value map contains just the arguments,
                -- and undefs for everything else.
                -- XXX later on, we'll have to add alloca's to this
                initValMap :: Map Id LLVM.ValueRef
                initValMap =
                  let
                    addArg :: (Int, Id) -> Map Id LLVM.ValueRef ->
                              Map Id LLVM.ValueRef
                    addArg (arg, id) =
                      let
                        param = LLVM.getParam func arg
                      in
                        Map.insert id param

                    addUndefs :: Id -> Map Id LLVM.ValueRef ->
                                 Map Id LLVM.ValueRef
                    addUndefs id valmap =
                      case Map.lookup id valmap of
                        Just val -> valmap
                        Nothing ->
                          Map.insert id (LLVM.getUndef (tyarr ! id)) valmap

                    withArgs = foldr addArg Map.empty
                                     (zip [0..length params] params)
                  in
                    foldr addUndefs withArgs indexes

                -- Take the value map, and add the incoming edges to a
                -- successor block.  Called when leaving a block on
                -- all its successors.
                bindPhis :: LLVM.BasicBlockRef -> Map Id LLVM.ValueRef ->
                            Node -> IO ()
                bindPhis from valmap to =
                  let
                    phis = phiarr ! to
                    fromval = LLVM.basicBlockAsValue from

                    bindPhi :: (Id, LLVM.ValueRef) -> IO ()
                    bindPhi (id, phival) =
                      let
                        Just oldval = Map.lookup id valmap
                      in do
                        LLVM.addIncoming phival [(oldval, fromval)]
                  in
                    mapM_ bindPhi phis

                -- Replace all values with corresponding phis when
                -- entering a block.
                addPhiVals :: Node -> Map Id LLVM.ValueRef ->
                              Map Id LLVM.ValueRef
                addPhiVals node valmap =
                  let
                    phis = phiarr ! node

                    addPhi :: (Id, LLVM.ValueRef) -> Map Id LLVM.ValueRef ->
                              Map Id LLVM.ValueRef
                    addPhi (id, phival) = Map.insert id phival
                  in do
                    foldr addPhi valmap phis
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

                -- Generate read code
                accessRead :: Map Id LLVM.ValueRef -> [Word] ->
                              [LLVM.ValueRef] -> LLVM.ValueRef ->
                              IO LLVM.ValueRef
                accessRead fields [] inner =
                  LLVM.buildExtractElement builder inner fields ""
                accessRead [] offsets inner =
                  do
                    gep <- LLVM.buildGEP builder inner offsets ""
                    LLVM.buildLoad builder gep ""
                accessRead fields offsets inner =
                  do
                    extract <- LLVM.buildExtractElement builder inner fields ""
                    gep <- LLVM.buildGEP builder extract offsets ""
                    LLVM.buildLoad builder gep ""

                -- Generate write code
                accessWrite :: LLVM.ValueRef -> Map Id LLVM.ValueRef ->
                               [Word] -> [LLVM.ValueRef] -> LLVM.ValueRef ->
                               IO LLVM.ValueRef
                accessWrite val _ [] offsets inner =
                  do
                    gep <- LLVM.buildGEP builder inner offsets ""
                    LLVM.buildStore builder val gep
                accessWrite val _ fields [] inner =
                  error "write to aggregates not yet implemented"
                accessWrite val _ fields offsets inner =
                  let
                    fields' = map offsetVal fields
                  in do
                    extract <- LLVM.buildExtractElement builder inner fields' ""
                    gep <- LLVM.buildGEP builder extract offsets ""
                    LLVM.buildStore builder val gep

                -- Generate addrof code
                accessAddr :: Map Id LLVM.ValueRef -> [Word] ->
                              [LLVM.ValueRef] -> LLVM.ValueRef ->
                              IO LLVM.ValueRef
                accessAddr _ fields [] inner =
                  error "addrof aggregate not yet implemented"
                accessAddr _ [] offsets inner =
                  LLVM.buildGEP builder inner offsets ""
                accessAddr _ fields offsets inner =
                  let
                    fields' = map offsetVal fields
                  in do
                    extract <- LLVM.buildExtractElement builder inner fields' ""
                    LLVM.buildGEP builder extract offsets ""
-}
                genLValueRead :: Map Id LLVM.ValueRef -> LValue ->
                                 IO LLVM.ValueRef
                genLValueRead valmap (Var id) =
                  return (fromJust (Map.lookup id valmap))
                genLValueRead valmap (Global name) =
                  LLVM.buildLoad builder (decls ! name) ""
                genLValueRead valmap lval =
                  error "complex lvalues disabled"
--                  genLValueAccess accessRead valmap lval

                genLValueWrite :: Map Id LLVM.ValueRef -> LLVM.ValueRef ->
                                  LValue -> IO (Map Id LLVM.ValueRef)
                genLValueWrite valmap val (Var id) =
                  return (Map.insert id val valmap)
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
                genLValueAddr :: Map Id LLVM.ValueRef -> LValue ->
                                 IO LLVM.ValueRef
                genLValueAddr valmap (Var _) =
                  error "Address of local variables not implemented"
                genLValueAddr valmap lval =
                  error "complex lvalues disabled"
--                  genLValueAccess accessAddr valmap lval

                -- Generate code for an expression
                genExp :: Map Id LLVM.ValueRef -> Exp -> IO LLVM.ValueRef
                genExp valmap (Binop op l r) =
                  do
                    l' <- genExp valmap l
                    r' <- genExp valmap r
                    case op of
                      Add -> LLVM.buildAdd builder l' r' ""
                      AddNSW -> LLVM.buildNSWAdd builder l' r' ""
                      AddNUW -> LLVM.buildNUWAdd builder l' r' ""
                      FAdd -> LLVM.buildFAdd builder l' r' ""
                      Sub -> LLVM.buildSub builder l' r' ""
                      SubNSW -> LLVM.buildNSWSub builder l' r' ""
                      SubNUW -> LLVM.buildNUWSub builder l' r' ""
                      FSub -> LLVM.buildFSub builder l' r' ""
                      Mul -> LLVM.buildMul builder l' r' ""
                      MulNSW -> LLVM.buildNSWMul builder l' r' ""
                      MulNUW -> LLVM.buildNUWMul builder l' r' ""
                      FMul -> LLVM.buildFMul builder l' r' ""
                      UDiv -> LLVM.buildUDiv builder l' r' ""
                      SDiv -> LLVM.buildSDiv builder l' r' ""
                      FDiv -> LLVM.buildFDiv builder l' r' ""
                      UMod -> LLVM.buildURem builder l' r' ""
                      SMod -> LLVM.buildSRem builder l' r' ""
                      FMod -> LLVM.buildFRem builder l' r' ""
                      And -> LLVM.buildAnd builder l' r' ""
                      Or -> LLVM.buildOr builder l' r' ""
                      Xor -> LLVM.buildXor builder l' r' ""
                      Shl -> LLVM.buildShl builder l' r' ""
                      LShr -> LLVM.buildLShr builder l' r' ""
                      AShr -> LLVM.buildAShr builder l' r' ""
                      Eq -> LLVM.buildICmp builder (LLVM.IntEQ) l' r' ""
                      Neq -> LLVM.buildICmp builder (LLVM.IntNE) l' r' ""
                      UGe -> LLVM.buildICmp builder (LLVM.IntUGE) l' r' ""
                      ULe -> LLVM.buildICmp builder (LLVM.IntULE) l' r' ""
                      UGt -> LLVM.buildICmp builder (LLVM.IntUGT) l' r' ""
                      ULt -> LLVM.buildICmp builder (LLVM.IntULT) l' r' ""
                      SGe -> LLVM.buildICmp builder (LLVM.IntSGE) l' r' ""
                      SLe -> LLVM.buildICmp builder (LLVM.IntSLE) l' r' ""
                      SGt -> LLVM.buildICmp builder (LLVM.IntSGT) l' r' ""
                      SLt -> LLVM.buildICmp builder (LLVM.IntSLT) l' r' ""
                      FOEq -> LLVM.buildFCmp builder (LLVM.RealOEQ) l' r' ""
                      FONeq -> LLVM.buildFCmp builder (LLVM.RealONE) l' r' ""
                      FOGe -> LLVM.buildFCmp builder (LLVM.RealOGE) l' r' ""
                      FOLe -> LLVM.buildFCmp builder (LLVM.RealOLE) l' r' ""
                      FOGt -> LLVM.buildFCmp builder (LLVM.RealOGT) l' r' ""
                      FOLt -> LLVM.buildFCmp builder (LLVM.RealOLT) l' r' ""
                      FUEq -> LLVM.buildFCmp builder (LLVM.RealUEQ) l' r' ""
                      FUNeq -> LLVM.buildFCmp builder (LLVM.RealUNE) l' r' ""
                      FUGe -> LLVM.buildFCmp builder (LLVM.RealUGE) l' r' ""
                      FULe -> LLVM.buildFCmp builder (LLVM.RealULE) l' r' ""
                      FUGt -> LLVM.buildFCmp builder (LLVM.RealUGT) l' r' ""
                      FULt -> LLVM.buildFCmp builder (LLVM.RealULT) l' r' ""
                genExp valmap (Call func args) =
                  do
                    func' <- genExp valmap func
                    args' <- mapM (genExp valmap) args
                    LLVM.buildCall builder func' args' ""
                genExp valmap (Unop op inner) =
                  do
                    inner' <- genExp valmap inner
                    case op of
                      Neg -> LLVM.buildNeg builder inner' ""
                      NegNSW -> LLVM.buildNSWNeg builder inner' ""
                      NegNUW -> LLVM.buildNUWNeg builder inner' ""
                      FNeg -> LLVM.buildFNeg builder inner' ""
                      Not -> LLVM.buildNot builder inner' ""
                genExp valmap (Conv fromty toty inner) =
                  do
                    toty' <- toLLVMType ctx typedefs toty
                    inner' <- genExp valmap inner
                    case (fromty, toty) of
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
                genExp valmap (LValue lval) = genLValueRead valmap lval
                genExp valmap (AddrOf lval) = genLValueAddr valmap lval
                genExp valmap (StructConst (StructType packed _) inits) =
                  do
                    inits' <- mapM (genExp valmap) (elems inits)
                    return (LLVM.constStruct inits' packed)
                genExp valmap (ArrayConst ty inits) =
                  do
                    inits' <- mapM (genExp valmap) inits
                    ty' <- toLLVMType ctx typedefs ty
                    return (LLVM.constArray ty' inits')
                genExp _ (NumConst ty @ (IntType signed _) n) =
                  do
                    ty' <- toLLVMType ctx typedefs ty
                    return (LLVM.constInt ty' n signed)

                genStm :: Map Id LLVM.ValueRef -> Stm ->
                          IO (Map Id LLVM.ValueRef)
                genStm valmap (Do e) =
                  do
                    genExp valmap e
                    return valmap
                genStm valmap (Move lval rval) =
                  do
                    rval' <- genExp valmap rval
                    genLValueWrite valmap rval' lval

                -- Generate code for a transfer
                genTransfer :: Map Id LLVM.ValueRef -> Transfer ->
                               IO LLVM.ValueRef
                genTransfer valmap (Goto (Label l)) =
                  LLVM.buildBr builder (blocks ! l)
                genTransfer valmap (Case test cases (Label def)) =
                  do
                    testval <- genExp valmap test
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
                    retval <- genExp valmap ret
                    LLVM.buildRet builder retval
                genTransfer valmap (Ret Nothing) = LLVM.buildRetVoid builder
                genTransfer valmap Unreachable = LLVM.buildUnreachable builder

                -- Traverse the CFG.  This takes a DFS tree as an argument.
                traverse :: LLVM.BasicBlockRef -> Map Id LLVM.ValueRef ->
                            Tree Node -> IO ()
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
                bindPhis entryblock initValMap entrynode
                traverse entryblock initValMap dfstree
          in do
            tyarr <- mapM (toLLVMType ctx typedefs) valtys
            entryblock <- LLVM.appendBasicBlockInContext ctx func "entry"
            blocks <- genBlocks
            phiset <- buildPhiSets
            phiarr <- genPhis blocks tyarr phiset
            LLVM.positionAtEnd builder entryblock
            LLVM.buildBr builder (blocks ! entrynode)
            genInstrs blocks phiarr tyarr entryblock builder
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
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
{-# OPTIONS_GHC -Wall -Werror #-}

-- | This module contains code for generating constant initializers.
module IR.FlatIR.LLVMGen.ConstValue(
       genConst
       ) where

import Data.Array.IArray
import Data.Array.Unboxed(UArray)
import Data.Graph.Inductive
import Data.Pos
import Data.Word
import IR.Common.Ptr
import IR.FlatIR.Syntax
import IR.FlatIR.LLVMGen.LLVMValue

import qualified IR.FlatIR.LLVMGen.Types as Types
import qualified IR.FlatIR.LLVMGen.Utils as Utils
import qualified LLVM.Core as LLVM

-- | Generate an LLVM value representing an initializer for a global
-- constant.
genConst :: Graph gr =>
            Module gr
         -- ^ The FlatIR module being translated.
         -> LLVM.ContextRef
         -- ^ The LLVM Context handle.
         -> UArray Typename LLVM.TypeRef
         -- ^ An array mapping Typenames to LLVM Type handles.
         -> Array Globalname LLVM.ValueRef
         -- ^ An array mapping Globalnames to LLVM global variable handles.
         -> Exp
         -- ^ The initializer being generated.
         -> IO (LLVM.ValueRef, Type)
         -- ^ The LLVM value representing the initializer, and its FlatIR type
genConst irmod @ (Module { {-modTypes = types, modGCHeaders = gcheaders-} })
         ctx typedefs decls =
  let
    getGlobalMutability = Utils.getGlobalMutability irmod
    getGlobalType = Utils.getGlobalType irmod
    booltype = Utils.booltype
    toLLVMType = Types.toLLVMType irmod ctx typedefs

    -- Generate a constant value from an LValue.  The only allowable
    -- cases are for fields and array indexes.
    genConstLValue :: LValue -> IO (LLVM.ValueRef, Type)
    genConstLValue =
      let
        genConstLValue' :: [Word] -> LValue -> IO (LLVM.ValueRef, Type)
        -- For field access with an LValue inner, build up the fields list
        genConstLValue' fields (Field { fieldVal = LValue lval,
                                        fieldName = Fieldname field }) =
          genConstLValue' (field : fields) lval
        -- When we get to the end, generate an extractvalue constant
        -- and figure out the types.
        genConstLValue' fields (Field { fieldName = Fieldname field,
                                        fieldVal = expr }) =
          let
            foldfun (StructType { structFields = innerfields }) ind =
              let
                (_, _, ty) = innerfields ! (Fieldname ind)
              in
                ty
            foldfun _ _ = error "Given type does not have fields"

            indlist = field : fields
          in do
            (expr', ty) <- genConst' expr
            val <- LLVM.constExtractValue expr' indlist
            return (val, foldl foldfun ty indlist)
        -- XXX should handle indexes just like fields, as long as the indexes
        -- are constant.
        genConstLValue' _ (Index {}) =
          error "Index in constant initializer"
        -- Dereferences are an error, since this can't be computed in
        -- advance.
        genConstLValue' _ (Deref {}) =
          error "Dereference in constant initializer"
        -- Global references are an error, since we can only know
        -- their addresses.
        genConstLValue' _ (Global {}) =
          error "Global variable access in constant initializer"
        -- Variable references are an error, since there are no
        -- locals.
        genConstLValue' _ (Var {}) =
          error "Variable access in constant initializer"
      in
        genConstLValue' []

    -- Get a constant representing the address of an LValue.  Since
    -- this is a global constant initializer, the allowable cases are
    -- severely constrained.
    genConstLValueAddr :: LValue -> IO (LLVM.ValueRef, Type)
    genConstLValueAddr lval =
      let
        getGEP :: [LLVM.ValueRef] -> LLVM.ValueRef -> LLVM.ValueRef
        getGEP [] val = val
        getGEP offsets val =
          LLVM.constGEP val (toValue (Fieldname 0) : offsets)

        -- The returned type here is the pointer *element* type
        genConstLValueAddr' :: [LLVM.ValueRef] -> LValue ->
                               IO (LLVM.ValueRef, Mutability, Type)
        -- For fields, add to the index list and continue
        genConstLValueAddr' offsets (Field { fieldVal = LValue innerlval,
                                             fieldName = field }) =
          do
            (val, outermut, ty') <-
              genConstLValueAddr' (toValue field : offsets) innerlval
            case ty' of
              StructType { structFields = innerfields } ->
                let
                  (_, innermut, fieldty) = innerfields ! field
                in
                  return (val, mergeMutability outermut innermut, fieldty)
              _ -> error "Cannot take field index of given type"
        -- We can only take the address of a global
        genConstLValueAddr' _ (Field {}) =
          error "addrof applied to non-addressable value"
        -- For indexes, add to the index list and continue
        genConstLValueAddr' offsets (Index { idxVal = LValue innerlval,
                                             idxIndex = ind }) =
          do
            (ind', _) <- genConst' ind
            (val, mut, ty') <- genConstLValueAddr' (ind' : offsets) innerlval
            case ty' of
              ArrayType { arrayElemTy = innerty } ->
                return (val, mut, innerty)
              _ -> error "Given type cannot be indexed"
        genConstLValueAddr' _ (Index {}) =
          error "addrof applied to non-addressable value"
{-
        genConstLValueAddr' offsets (Index { idxVal = expr,
                                             idxIndex = ind }) =
          do
            (ind', _) <- genConst' ind
            (expr', ty) <- genConst' expr
            case ty of
              ArrayType { arrayElemTy = innerty } ->
                return (LLVM.constGEP expr' (ind' : offsets), mut, innerty)
              _ -> error "Given type cannot be indexed"
-}
        -- There cannot be a dereference in a constant initializer
        genConstLValueAddr' _ (Deref { derefVal = LValue _ }) =
          error "Dereference in constant initializer"
        -- XXX something seems wrong here
        genConstLValueAddr' _ (Deref {}) =
          error "addrof applied to non-addressable value"
{-
        genConstLValueAddr' offsets (Deref { derefVal = expr }) =
          do
            (exp', ty) <- genConst' expr
            case ty of
              PtrType { ptrTy = Native { nativeTy = elemty,
                                         nativeMutability = mut } } ->
                return (getGEP offsets exp', mut, elemty)
              PtrType { ptrTy = GC { gcTy = ind, gcPos = p,
                                     gcMutability = mut } } ->
                let
                  (tyname, _, _) = gcheaders ! ind
                  elemty = case types ! tyname of
                    (_, Just elemty') -> elemty'
                    _ -> IdType { idName = tyname, idPos = p }
                in
                  return (getGEP offsets exp', mut, elemty)
              _ -> error "Cannot take address of constant of given type"
-}
        -- We can take the address of a global
        genConstLValueAddr' offsets (Global { globalName = name }) =
          let
            ty = getGlobalType name
            val = getGEP offsets (decls ! name)
            mut = getGlobalMutability name
          in
            return (val, mut, ty)
        -- There are no local variables
        genConstLValueAddr' _ (Var {}) =
          error "Variable access in constant initializer"
      in do
        (val, mut, ty) <- genConstLValueAddr' [] lval
        return (val, PtrType { ptrTy = Native { nativeMutability = mut,
                                                nativeTy = ty },
                               ptrPos = pos lval })

    -- Generate a constant initializer for a global variable
    genConst' :: Exp -> IO (LLVM.ValueRef, Type)
    genConst' (Binop { binopOp = op, binopLeft = l,
                       binopRight = r, binopPos = posn }) =
      do
        (l', lty) <- genConst' l
        (r', rty) <- genConst' r
        -- Compile each operation using the types of the operands
        case (op, lty, rty) of
          (Add, IntType {}, IntType {}) ->
            return (LLVM.constAdd l' r', lty)
          (Add, FloatType {}, FloatType {}) ->
            return (LLVM.constFAdd l' r', lty)
          (AddNW, IntType { intSigned = True },
           IntType { intSigned = True }) ->
            do
              addval <- LLVM.constNSWAdd l' r'
              return (addval, lty)
          (AddNW, IntType { intSigned = False },
           IntType { intSigned = False }) ->
            do
              addval <- LLVM.constNUWAdd l' r'
              return (addval, lty)
          (Sub, IntType {}, IntType {}) ->
            return (LLVM.constSub l' r', lty)
          (Sub, FloatType {}, FloatType {}) ->
            return (LLVM.constFSub l' r', lty)
          (SubNW, IntType { intSigned = True },
           IntType { intSigned = True }) ->
            do
              subval <- LLVM.constNSWSub l' r'
              return (subval, lty)
          (SubNW, IntType { intSigned = False },
           IntType { intSigned = False }) ->
            do
              subval <- LLVM.constNUWSub l' r'
              return (subval, lty)
          (Mul, IntType {}, IntType {}) ->
            return (LLVM.constMul l' r', lty)
          (Mul, FloatType {}, FloatType {}) ->
            return (LLVM.constFMul l' r', lty)
          (MulNW, IntType { intSigned = True },
           IntType { intSigned = True }) ->
            do
              mulval <- LLVM.constNSWMul l' r'
              return (mulval, lty)
          (MulNW, IntType { intSigned = False },
           IntType { intSigned = False }) ->
            do
              mulval <- LLVM.constNUWMul l' r'
              return (mulval, lty)
          (Div, IntType { intSigned = True },
           IntType { intSigned = True }) ->
            return (LLVM.constUDiv l' r', lty)
          (Div, IntType { intSigned = False },
           IntType { intSigned = False }) ->
            return (LLVM.constSDiv l' r', lty)
          (Div, FloatType {}, FloatType {}) ->
            return (LLVM.constFDiv l' r', lty)
          (Mod, IntType { intSigned = True },
           IntType { intSigned = True }) ->
            return (LLVM.constURem l' r', lty)
          (Mod, IntType { intSigned = False },
           IntType { intSigned = False }) ->
            return (LLVM.constSRem l' r', lty)
          (Mod, FloatType {}, FloatType {}) ->
            return (LLVM.constFRem l' r', lty)
          (And, IntType {}, IntType {}) ->
            return (LLVM.constAnd l' r', lty)
          (Or, IntType {}, IntType {}) ->
            return (LLVM.constOr l' r', lty)
          (Xor, IntType {}, IntType {}) ->
            return (LLVM.constXor l' r', lty)
          (Shl, IntType {}, IntType {}) ->
            return (LLVM.constShl l' r', lty)
          (Shr, IntType { intSigned = True }, IntType {}) ->
            return (LLVM.constAShr l' r', lty)
          (Shr, IntType { intSigned = False }, IntType {}) ->
            return (LLVM.constLShr l' r', lty)
          (Eq, IntType {}, IntType {}) ->
            return (LLVM.constICmp LLVM.IntEQ l' r', booltype posn)
          (Neq, IntType {}, IntType {}) ->
            return (LLVM.constICmp LLVM.IntNE l' r', booltype posn)
          (Ge, IntType { intSigned = True },
           IntType { intSigned = True }) ->
            return (LLVM.constICmp LLVM.IntSGE l' r', booltype posn)
          (Ge, IntType { intSigned = False },
           IntType { intSigned = False }) ->
            return (LLVM.constICmp LLVM.IntUGE l' r', booltype posn)
          (Gt, IntType { intSigned = False },
           IntType { intSigned = False }) ->
            return (LLVM.constICmp LLVM.IntUGT l' r', booltype posn)
          (Gt, IntType { intSigned = True },
           IntType { intSigned = True }) ->
            return (LLVM.constICmp LLVM.IntSGT l' r', booltype posn)
          (Le, IntType { intSigned = False },
           IntType { intSigned = False }) ->
            return (LLVM.constICmp LLVM.IntULE l' r', booltype posn)
          (Le, IntType { intSigned = True },
           IntType { intSigned = True }) ->
            return (LLVM.constICmp LLVM.IntSLE l' r', booltype posn)
          (Lt, IntType { intSigned = False },
           IntType { intSigned = False }) ->
            return (LLVM.constICmp LLVM.IntULT l' r', booltype posn)
          (Lt, IntType { intSigned = True },
           IntType { intSigned = True }) ->
            return (LLVM.constICmp LLVM.IntSLT l' r', booltype posn)
          (FOEq, FloatType {}, FloatType {}) ->
            return (LLVM.constFCmp LLVM.RealOEQ l' r', booltype posn)
          (FONeq, FloatType {}, FloatType {}) ->
            return (LLVM.constFCmp LLVM.RealONE l' r', booltype posn)
          (FOGe, FloatType {}, FloatType {}) ->
            return (LLVM.constFCmp LLVM.RealOGE l' r', booltype posn)
          (FOGt, FloatType {}, FloatType {}) ->
            return (LLVM.constFCmp LLVM.RealOGT l' r', booltype posn)
          (FOLe, FloatType {}, FloatType {}) ->
            return (LLVM.constFCmp LLVM.RealOLE l' r', booltype posn)
          (FOLt, FloatType {}, FloatType {}) ->
            return (LLVM.constFCmp LLVM.RealOLT l' r', booltype posn)
          (FUEq, FloatType {}, FloatType {}) ->
            return (LLVM.constFCmp LLVM.RealUEQ l' r', booltype posn)
          (FUNeq, FloatType {}, FloatType {}) ->
            return (LLVM.constFCmp LLVM.RealUNE l' r', booltype posn)
          (FUGe, FloatType {}, FloatType {}) ->
            return (LLVM.constFCmp LLVM.RealUGE l' r', booltype posn)
          (FUGt, FloatType {}, FloatType {}) ->
            return (LLVM.constFCmp LLVM.RealUGT l' r', booltype posn)
          (FULe, FloatType {}, FloatType {}) ->
            return (LLVM.constFCmp LLVM.RealULE l' r', booltype posn)
          (FULt, FloatType {}, FloatType {}) ->
            return (LLVM.constFCmp LLVM.RealULT l' r', booltype posn)
          _ -> error "Cannot generate binary operator for given types"
    genConst' (Unop { unopOp = op, unopVal = inner }) =
      do
        (inner', ty) <- genConst' inner
        -- Compile the operation using the type of the operand
        case (op, ty) of
          (Neg, IntType {}) -> return (LLVM.constNeg inner', ty)
          (NegNW, IntType { intSigned = True }) ->
            do
              negval <- LLVM.constNSWNeg inner'
              return (negval, ty)
          (NegNW, IntType { intSigned = False }) ->
            do
              negval <- LLVM.constNUWNeg inner'
              return (negval, ty)
          (Neg, FloatType {}) -> return (LLVM.constFNeg inner', ty)
          (Not, IntType {}) -> return (LLVM.constNot inner', ty)
          _ -> error "Cannot generate unary operator for the given type"
    -- Get an address from the LValue
    genConst' (AddrOf { addrofVal = lval }) = genConstLValueAddr lval
    -- Get the value of the LValue
    genConst' (LValue lval) = genConstLValue lval
    genConst' (Conv { convTy = toty, convVal = inner }) =
      do
        toty' <- toLLVMType toty
        (inner', fromty) <- genConst' inner
        -- Generate a conversion based on the types
        case (fromty, toty) of
          (IntType { intSigned = False, intSize = fromsize },
           IntType { intSize = tosize }) ->
            if fromsize > tosize
              then return (LLVM.constTrunc inner' toty', toty)
              else if fromsize < tosize
                then return (LLVM.constZExt inner' toty', toty)
                else return (inner', toty)
          (IntType { intSigned = True, intSize = fromsize},
           IntType { intSize = tosize }) ->
            if fromsize > tosize
              then return (LLVM.constTrunc inner' toty', toty)
              else if fromsize < tosize
                then return (LLVM.constSExt inner' toty', toty)
                else return (inner', toty)
          (FloatType { floatSize = fromsize },
           FloatType { floatSize = tosize }) ->
            if fromsize > tosize
              then return (LLVM.constFPTrunc inner' toty', toty)
              else if fromsize < tosize
                then return (LLVM.constFPExt inner' toty', toty)
                else return (inner', toty)
          (IntType { intSigned = False }, FloatType {}) ->
            return (LLVM.constUIToFP inner' toty', toty)
          (IntType { intSigned = True }, FloatType {}) ->
            return (LLVM.constSIToFP inner' toty', toty)
          (FloatType {}, IntType { intSigned = False }) ->
            return (LLVM.constFPToUI inner' toty', toty)
          (FloatType {}, IntType { intSigned = True }) ->
            return (LLVM.constFPToSI inner' toty', toty)
          (IntType { intSigned = True }, PtrType {}) ->
            return (LLVM.constIntToPtr inner' toty', toty)
          (PtrType {}, IntType { intSigned = True }) ->
            return (LLVM.constPtrToInt inner' toty', toty)
          (PtrType {}, PtrType {}) ->
            do
              cast <- LLVM.constPointerCast inner' toty'
              return (cast, toty)
          _ -> error "Cannot generate conversion"
    -- Just generate a bitcast
    genConst' (Cast { castTy = toty, castVal = inner }) =
      do
        toty' <- toLLVMType toty
        (inner', _) <- genConst' inner
        return (LLVM.constBitCast inner' toty', toty)
    -- Generate a structure constant
    genConst' (StructConst { structConstTy =
                               ty @ (StructType { structPacked = packed }),
                             structConstFields = inits }) =
      do
        inits' <- mapM (\field -> genConst' field >>= return . fst)
                       (elems inits)
        return (LLVM.constStruct inits' packed, ty)
    -- Generate an array constant
    genConst' (ArrayConst { arrayConstTy = ty, arrayConstVals = inits }) =
      do
        ty' <- toLLVMType ty
        inits' <- mapM (\field -> genConst' field >>= return . fst) inits
        return (LLVM.constArray ty' inits', ty)
    -- Generate a numerical constant
    genConst' (NumConst { numConstTy = ty @ (IntType { intSigned = signed }),
                          numConstVal = n }) =
      do
        ty' <- toLLVMType ty
        return (LLVM.constInt ty' n signed, ty)
    genConst' _ = fail ("Cannot generate constant")
  in
    genConst'

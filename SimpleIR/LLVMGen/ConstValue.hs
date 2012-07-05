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

-- | This module contains code for generating constant initializers.
module SimpleIR.LLVMGen.ConstValue(
       genConst
       ) where

import Data.Array(Array)
import Data.Array.IArray
import Data.Array.Unboxed(UArray)
import Data.Graph.Inductive
import Data.Word
import SimpleIR
import SimpleIR.LLVMGen.LLVMValue

import qualified LLVM.Core as LLVM
import qualified SimpleIR.LLVMGen.Types as Types
import qualified SimpleIR.LLVMGen.Utils as Utils

-- | Generate an LLVM value representing an initializer for a global
-- constant.
genConst :: Graph gr => Module gr -> LLVM.ContextRef ->
            UArray Typename LLVM.TypeRef -> Array Globalname LLVM.ValueRef ->
            Exp -> IO (LLVM.ValueRef, Type)
genConst mod @ (Module { modTypes = types, modGCHeaders = gcheaders })
         ctx typedefs decls =
  let
    getGlobalType = Utils.getGlobalType mod
    booltype = Utils.booltype
    toLLVMType = Types.toLLVMType mod ctx typedefs

    -- Generate a constant value from an LValue.  The only allowable
    -- cases are for fields and array indexes.
    genConstLValue :: LValue -> IO (LLVM.ValueRef, Type)
    genConstLValue =
      let
        genConstLValue' :: [Word] -> LValue -> IO (LLVM.ValueRef, Type)
        -- For field access with an LValue inner, build up the fields list
        genConstLValue' fields (Field (LValue lval) (Fieldname field)) =
          genConstLValue' (field : fields) lval
        -- When we get to the end, generate an extractvalue constant
        -- and figure out the types.
        genConstLValue' fields (Field exp (Fieldname field)) =
          let
            foldfun (StructType _ fields) ind =
              let
                (_, _, ty) = fields ! (Fieldname ind)
              in
                ty

            indlist = field : fields
          in do
            (exp', ty) <- genConst' exp
            val <- LLVM.constExtractValue exp' indlist
            return (val, foldl foldfun ty indlist)
        -- XXX handle indexes just like fields, as long as the indexes
        -- are constant.
        genConstLValue' _ (Index _ _) =
          error "Index in constant initializer"
        -- Dereferences are an error, since this can't be computed in
        -- advance.
        genConstLValue' _ (Deref _) =
          error "Dereference in constant initializer"
        -- Global references are an error, since we can only know
        -- their addresses.
        genConstLValue' _ (Global _) =
          error "Global variable access in constant initializer"
        -- Variable references are an error, since there are no
        -- locals.
        genConstLValue' _ (Var _) =
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
        getGEP offsets val = LLVM.constGEP val (toValue (Fieldname 0) : offsets)

        genConstLValueAddr' :: [LLVM.ValueRef] -> LValue ->
                               IO (LLVM.ValueRef, Type)
        -- For fields, add to the index list and continue
        genConstLValueAddr' offsets (Field (LValue lval) field) =
          do
            (val, ty) <- genConstLValueAddr' (toValue field : offsets) lval
            case ty of
              StructType _ fields ->
                let
                  (_, _, ty) = fields ! field
                in
                  return (val, ty)
        -- We can only take the address of a global
        genConstLValueAddr' _ (Field _ _) =
          error "addrof applied to non-addressable value"
        -- For indexes, add to the index list and continue
        genConstLValueAddr' offsets (Index (LValue lval) ind) =
          do
            (ind', _) <- genConst' ind
            (val, ty) <- genConstLValueAddr' (ind' : offsets) lval
            case ty of
              ArrayType _ innerty -> return (val, innerty)
        -- XXX something seems wrong here
        genConstLValueAddr' offsets (Index exp ind) =
          do
            (ind', _) <- genConst' ind
            (exp', ty) <- genConst' exp
            case ty of
              ArrayType _ innerty ->
                return (LLVM.constGEP exp' (ind' : offsets), innerty)
        -- There cannot be a dereference in a constant initializer
        genConstLValueAddr' _ (Deref (LValue exp)) =
          error "Dereference in constant initializer"
        -- XXX something seems wrong here
        genConstLValueAddr' offsets (Deref exp) =
          do
            (exp', ty) <- genConst' exp
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
        -- We can take the address of a global
        genConstLValueAddr' offsets (Global g) =
          let
            ty = getGlobalType g
            val = getGEP offsets (decls ! g)
          in
            return (val, ty)
        -- There are no local variables
        genConstLValueAddr' _ (Var _) =
          error "Variable access in constant initializer"
      in do
        (val, ty) <- genConstLValueAddr' [] lval
        return (val, PtrType (BasicObj ty))

    -- Generate a constant initializer for a global variable
    genConst' :: Exp -> IO (LLVM.ValueRef, Type)
    genConst' (Binop op l r) =
      do
        (l', lty) <- genConst' l
        (r', rty) <- genConst' r
        -- Compile each operation using the types of the operands
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
            return (LLVM.constICmp LLVM.IntEQ l' r', booltype)
          (Neq, IntType _ _, IntType _ _) ->
            return (LLVM.constICmp LLVM.IntNE l' r', booltype)
          (Ge, IntType True _, IntType True _) ->
            return (LLVM.constICmp LLVM.IntSGE l' r', booltype)
          (Ge, IntType False _, IntType False _) ->
            return (LLVM.constICmp LLVM.IntUGE l' r', booltype)
          (Gt, IntType False _, IntType False _) ->
            return (LLVM.constICmp LLVM.IntUGT l' r', booltype)
          (Gt, IntType True _, IntType True _) ->
            return (LLVM.constICmp LLVM.IntSGT l' r', booltype)
          (Le, IntType False _, IntType False _) ->
            return (LLVM.constICmp LLVM.IntULE l' r', booltype)
          (Le, IntType True _, IntType True _) ->
            return (LLVM.constICmp LLVM.IntSLE l' r', booltype)
          (Lt, IntType False _, IntType False _) ->
            return (LLVM.constICmp LLVM.IntULT l' r', booltype)
          (Lt, IntType True _, IntType True _) ->
            return (LLVM.constICmp LLVM.IntSLT l' r', booltype)
          (FOEq, FloatType _, FloatType _) ->
            return (LLVM.constFCmp LLVM.RealOEQ l' r', booltype)
          (FONeq, FloatType _, FloatType _) ->
            return (LLVM.constFCmp LLVM.RealONE l' r', booltype)
          (FOGe, FloatType _, FloatType _) ->
            return (LLVM.constFCmp LLVM.RealOGE l' r', booltype)
          (FOGt, FloatType _, FloatType _) ->
            return (LLVM.constFCmp LLVM.RealOGT l' r', booltype)
          (FOLe, FloatType _, FloatType _) ->
            return (LLVM.constFCmp LLVM.RealOLE l' r', booltype)
          (FOLt, FloatType _, FloatType _) ->
            return (LLVM.constFCmp LLVM.RealOLT l' r', booltype)
          (FUEq, FloatType _, FloatType _) ->
            return (LLVM.constFCmp LLVM.RealUEQ l' r', booltype)
          (FUNeq, FloatType _, FloatType _) ->
            return (LLVM.constFCmp LLVM.RealUNE l' r', booltype)
          (FUGe, FloatType _, FloatType _) ->
            return (LLVM.constFCmp LLVM.RealUGE l' r', booltype)
          (FUGt, FloatType _, FloatType _) ->
            return (LLVM.constFCmp LLVM.RealUGT l' r', booltype)
          (FULe, FloatType _, FloatType _) ->
            return (LLVM.constFCmp LLVM.RealULE l' r', booltype)
          (FULt, FloatType _, FloatType _) ->
            return (LLVM.constFCmp LLVM.RealULT l' r', booltype)
    genConst' (Unop op inner) =
      do
        (inner', ty) <- genConst' inner
        -- Compile the operation using the type of the operand
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
    -- Get an address from the LValue
    genConst' (AddrOf lval) = genConstLValueAddr lval
    -- Get the value of the LValue
    genConst' (LValue lval) = genConstLValue lval
    genConst' (Conv toty inner) =
      do
        toty' <- toLLVMType toty
        (inner', fromty) <- genConst' inner
        -- Generate a conversion based on the types
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
    -- Just generate a bitcast
    genConst' (Cast toty inner) =
      do
        toty' <- toLLVMType toty
        (inner', _) <- genConst' inner
        return (LLVM.constBitCast inner' toty', toty)
    -- Generate a structure constant
    genConst' (StructConst ty @ (StructType packed _) inits) =
      do
        inits' <- mapM (\field -> genConst' field >>= return . fst)
                       (elems inits)
        return (LLVM.constStruct inits' packed, ty)
    -- Generate an array constant
    genConst' (ArrayConst ty inits) =
      do
        ty' <- toLLVMType ty
        inits' <- mapM (\field -> genConst' field >>= return . fst) inits
        return (LLVM.constArray ty' inits', ty)
    -- Generate a numerical constant
    genConst' (NumConst ty @ (IntType signed _) n) =
      do
        ty' <- toLLVMType ty
        return (LLVM.constInt ty' n signed, ty)
    genConst' val = fail ("Cannot generate constant")
  in
    genConst'

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
{-# OPTIONS_GHC -Wall -Werror #-}

-- | This module contains code for generating constant initializers.
module IR.FlatIR.LLVMGen.ConstValue(
       genConst
       ) where

import Data.Array.IArray
import Data.Array.Unboxed(UArray)
import Data.Graph.Inductive
import Data.Word
import IR.FlatIR.Syntax
import IR.FlatIR.LLVMGen.LLVMValue

import qualified IR.FlatIR.LLVMGen.Types as Types
import qualified IR.FlatIR.LLVMGen.Utils as Utils
import qualified IR.GC.Types as GC
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
genConst irmod @ (Module { modTypes = types, modGCHeaders = gcheaders })
         ctx typedefs decls =
  let
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
        genConstLValue' fields (Field (LValue lval) (Fieldname field)) =
          genConstLValue' (field : fields) lval
        -- When we get to the end, generate an extractvalue constant
        -- and figure out the types.
        genConstLValue' fields (Field expr (Fieldname field)) =
          let
            foldfun (StructType _ innerfields) ind =
              let
                (_, _, ty) = innerfields ! (Fieldname ind)
              in
                ty
            foldfun _ _ = error "Given type does not have fields"

            indlist = field : fields
          in do
            (exp', ty) <- genConst' expr
            val <- LLVM.constExtractValue exp' indlist
            return (val, foldl foldfun ty indlist)
        -- XXX should handle indexes just like fields, as long as the indexes
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
        genConstLValueAddr' offsets (Field (LValue innerlval) field) =
          do
            (val, ty') <-
              genConstLValueAddr' (toValue field : offsets) innerlval
            case ty' of
              StructType _ innerfields ->
                let
                  (_, _, fieldty) = innerfields ! field
                in
                  return (val, fieldty)
              _ -> error "Cannot take field index of given type"
        -- We can only take the address of a global
        genConstLValueAddr' _ (Field _ _) =
          error "addrof applied to non-addressable value"
        -- For indexes, add to the index list and continue
        genConstLValueAddr' offsets (Index (LValue innerlval) ind) =
          do
            (ind', _) <- genConst' ind
            (val, ty') <- genConstLValueAddr' (ind' : offsets) innerlval
            case ty' of
              ArrayType _ innerty -> return (val, innerty)
              _ -> error "Given type cannot be indexed"
        -- XXX something seems wrong here
        genConstLValueAddr' offsets (Index expr ind) =
          do
            (ind', _) <- genConst' ind
            (expr', ty) <- genConst' expr
            case ty of
              ArrayType _ innerty ->
                return (LLVM.constGEP expr' (ind' : offsets), innerty)
              _ -> error "Given type cannot be indexed"
        -- There cannot be a dereference in a constant initializer
        genConstLValueAddr' _ (Deref (LValue _)) =
          error "Dereference in constant initializer"
        -- XXX something seems wrong here
        genConstLValueAddr' offsets (Deref expr) =
          do
            (exp', ty) <- genConst' expr
            case ty of
              PtrType (GC.Native elemty) ->
                return (getGEP offsets exp', elemty)
              PtrType (GC.GC _ ind) ->
                let
                  (tyname, _, _) = gcheaders ! ind
                  elemty = case types ! tyname of
                    (_, Just elemty') -> elemty'
                    _ -> IdType tyname
                in
                  return (getGEP offsets exp', elemty)
              _ -> error "Cannot take address of constant of given type"
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
        return (val, PtrType (GC.Native ty))

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
              addval <- LLVM.constNSWAdd l' r'
              return (addval, lty)
          (AddNW, IntType False _, IntType False _) ->
            do
              addval <- LLVM.constNUWAdd l' r'
              return (addval, lty)
          (Sub, IntType _ _, IntType _ _) ->
            return (LLVM.constSub l' r', lty)
          (Sub, FloatType _, FloatType _) ->
            return (LLVM.constFSub l' r', lty)
          (SubNW, IntType True _, IntType True _) ->
            do
              subval <- LLVM.constNSWSub l' r'
              return (subval, lty)
          (SubNW, IntType False _, IntType False _) ->
            do
              subval <- LLVM.constNUWSub l' r'
              return (subval, lty)
          (Mul, IntType _ _, IntType _ _) ->
            return (LLVM.constMul l' r', lty)
          (Mul, FloatType _, FloatType _) ->
            return (LLVM.constFMul l' r', lty)
          (MulNW, IntType True _, IntType True _) ->
            do
              mulval <- LLVM.constNSWMul l' r'
              return (mulval, lty)
          (MulNW, IntType False _, IntType True _) ->
            do
              mulval <- LLVM.constNUWMul l' r'
              return (mulval, lty)
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
          _ -> error "Cannot generate binary operator for given types"
    genConst' (Unop op inner) =
      do
        (inner', ty) <- genConst' inner
        -- Compile the operation using the type of the operand
        case (op, ty) of
          (Neg, IntType _ _) -> return (LLVM.constNeg inner', ty)
          (NegNW, IntType True _) ->
            do
              negval <- LLVM.constNSWNeg inner'
              return (negval, ty)
          (NegNW, IntType False _) ->
            do
              negval <- LLVM.constNUWNeg inner'
              return (negval, ty)
          (Neg, FloatType _) -> return (LLVM.constFNeg inner', ty)
          (Not, IntType _ _) -> return (LLVM.constNot inner', ty)
          _ -> error "Cannot generate unary operator for the given type"
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
          (IntType True _, PtrType _) ->
            return (LLVM.constIntToPtr inner' toty', toty)
          (PtrType _, IntType True _) ->
            return (LLVM.constPtrToInt inner' toty', toty)
          (PtrType _, PtrType _) ->
            do
              cast <- LLVM.constPointerCast inner' toty'
              return (cast, toty)
          _ -> error "Cannot generate conversion"
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
    genConst' _ = fail ("Cannot generate constant")
  in
    genConst'

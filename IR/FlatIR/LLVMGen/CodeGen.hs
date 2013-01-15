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
{-# OPTIONS_GHC -Wall -Werror #-}

-- | This module contains code for generating LLVM code for
-- statements and transfers.
module IR.FlatIR.LLVMGen.CodeGen(
       genStm,
       genTransfer
       ) where

import Data.Array.IArray
import Data.Array.Unboxed(UArray)
import Data.Graph.Inductive.Graph
import Data.Traversable
import Data.Word
import IR.FlatIR.Syntax
import IR.FlatIR.LLVMGen.LLVMValue
import IR.FlatIR.LLVMGen.VarAccess(Index(..), Access(..),
                                   Location(..), ValMap)
import Prelude hiding (mapM)

import qualified LLVM.Core as LLVM
import qualified IR.FlatIR.LLVMGen.Types as Types
import qualified IR.FlatIR.LLVMGen.Utils as Utils
import qualified IR.FlatIR.LLVMGen.VarAccess as VarAccess
import qualified IR.GC.Types as GC

getVarLocation :: ValMap -> Id -> Location
getVarLocation = VarAccess.getVarLocation

zeroind :: VarAccess.Index
zeroind = ValueInd (LLVM.constInt LLVM.int32Type (0 :: Word) False)

-- XXX Given that the LLVM code generation is already highly monadic,
-- most of the state here could probably be rolled up into a
-- Reader/State monad.  This would also allow us to generate
-- meaningful variable names.

-- | Generate code for an expression and store it in the form of an Access.
genExp :: Graph gr =>
          Module gr
       -- ^ The FlatIR module being compiled
       -> LLVM.ContextRef
       -- ^ The LLVM Context handle
       -> LLVM.BuilderRef
       -- ^ The LLVM Instruction builder handle
       -> Array Globalname LLVM.ValueRef
       -- ^ An array mapping global names to LLVM values
       -> Array Id Type
       -- ^ An array mapping variables to their types
       -> UArray Typename LLVM.TypeRef
       -- ^ An array mapping typenames to LLVM types
       -> ValMap
       -- ^ A map from Id's to Locations that hold their current value
       -> Exp
       -- ^ The expression to compile
       -> IO (Access, Type)
       -- ^ The compiled expression and its FlatIR type
genExp irmod ctx builder decls valtys typedefs valmap =
  let
    getActualType = Utils.getActualType irmod
    booltype = Utils.booltype
    getGlobalType = Utils.getGlobalType irmod
    toLLVMType = Types.toLLVMType irmod ctx typedefs
    genVarAddr = VarAccess.genVarAddr builder valmap
    genGEP = VarAccess.genGEP builder
    genExtractValue = VarAccess.genExtractValue builder
    genVarRead = VarAccess.genVarRead builder valmap

    -- Generate code for getting the address of a given LValue and
    -- return the LLVM value and type.
    genLValueAddr' :: [Index] -> LValue -> IO (LLVM.ValueRef, Type)
    -- For a simple dereference with no indexes, we can just cancel
    -- out the dereference operator
    genLValueAddr' [] (Deref inner) =
      do
        (DirectAcc inner', ty) <- genExp' inner
        case getActualType ty of
          PtrType _ -> return (inner', ty)
          _ -> error "Wrong value type in dereference"
    -- For dereferences with indexes, generate a getelementptr
    genLValueAddr' indexes (Deref inner) =
      do
        (DirectAcc inner', ty) <- genExp' inner
        case getActualType ty of
          PtrType (GC.GC _ _) -> 
            error "GC object field addresses not implemented"
          PtrType (GC.Native innerty) ->
            let
              indexvals = map toValue (zeroind : indexes)
            in do
              val <- LLVM.buildGEP builder inner' indexvals ""
              return (val, innerty)
          _ -> error "Wrong value type in dereference"
    -- Indexes should only every be done on something being
    -- dereferenced, so we need only worry about LValue inners.
    genLValueAddr' indexes (Index (LValue lval) ind) =
      do
        (DirectAcc ind', _) <- genExp' ind
        (val, ty) <- genLValueAddr' (ValueInd ind' : indexes) lval
        case getActualType ty of
          PtrType (GC.GC _ _) -> 
            error "GC object field addresses not implemented"
          PtrType (GC.Native arrty) ->
            case getActualType arrty of
              ArrayType _ innerty ->
                return (val, PtrType (GC.Native innerty))
              _ -> error "Wrong value type in array index"
          _ -> error "Taking address of non-addressable array index"
    genLValueAddr' _ (Index _ _) =
      error "Taking index address of non-addressable value"
    -- Field accesses should also only go through a dereference here,
    -- or else the value is non-addressable.
    genLValueAddr' indexes (Field (LValue lval) field) =
      do
        (val, ty) <- genLValueAddr' (FieldInd field : indexes) lval
        case getActualType ty of
          PtrType (GC.GC _ _) -> 
            error "GC object field addresses not implemented"
          PtrType (GC.Native innerty) ->
            case getActualType innerty of
              StructType _ fields ->
                let
                  (_, _, fieldty) = fields ! field
                in
                  return (val, PtrType (GC.Native fieldty))
              _ -> error "Field access to non-structure type"
          _ -> error "Taking address of non-addressable field access"
    genLValueAddr' _ (Field _ _) =
      error "Taking field address of non-addressable value"
    -- For globals, the corresponding LLVM value is actually its
    -- address.
    genLValueAddr' _ (Global name) =
      return (decls ! name, PtrType (GC.Native (getGlobalType name)))
    genLValueAddr' indexes (Var name) =
      do
        (val, _ {- volatile -}) <- genVarAddr indexes name
        -- XXX add volatility into the pointer type
        return (val, PtrType (GC.Native (valtys ! name)))

    genLValueAddr :: LValue -> IO (Access, Type)
    genLValueAddr lval =
      do
        (val, ty) <- genLValueAddr' [] lval
        return (DirectAcc val, ty)

    -- Generate code for reading an LValue.  Return an access and a type.
    genLValueRead' :: [Index] -> LValue -> IO (Access, Type)
    -- For dereferencing, generate a getelementptr and a load
    -- instruction.
    genLValueRead' indexes (Deref inner) =
      do
        (DirectAcc inner', ty) <- genExp' inner
        case getActualType ty of
          -- XXX For GC objects, create a table and look up accessor
          -- functions
          PtrType (GC.GC _ _) ->
            error "Reading GC object fields not implemented"
          PtrType (GC.Native innerty) ->
            do
              addr <- genGEP inner' indexes
              val <- LLVM.buildLoad builder addr ""
              return (DirectAcc val, innerty)
          _ -> error "Wrong value type in dereference"
    -- For an index, add to the list of indexes and continue.  We
    -- can't index without going through a dereference.
    genLValueRead' indexes (Index (LValue lval) ind) =
      do
        (DirectAcc ind', _) <- genExp' ind
        (val, ty) <- genLValueRead' (ValueInd ind' : indexes) lval
        case getActualType ty of
          ArrayType _ innerty -> return (val, innerty)
          _ -> error "Wrong value type in array index"
    genLValueRead' _ (Index _ _) =
      error "Generating array index for non-array value"
    -- For a field, possibly stop, and possibly add to the list of
    -- indexes and keep going.
    genLValueRead' indexes (Field expr field) =
      let
        indexes' = FieldInd field : indexes
      in do
        (val, ty) <- case expr of
          -- If the inner is an LValue, keep going
          LValue lval -> genLValueRead' indexes' lval
          -- Otherwise, generate an extractvalue
          _ ->
            do
              (expr', ty) <- genExp' expr
              acc <- genExtractValue expr' indexes'
              return (acc, ty)
        case getActualType ty of
          StructType _ fields ->
            let
              (_, _, fieldty) = fields ! field
            in
              return (val, fieldty)
          _ -> error "Wrong value type in field access"
    -- For globals, the LLVM value is actually a pointer.  Generate a
    -- getelementptr and a load.
    genLValueRead' indexes (Global name) =
      do
        addr <- genGEP (decls ! name) indexes
        val <- LLVM.buildLoad builder addr ""
        -- XXX set volatile
        return (DirectAcc val, getGlobalType name)
    -- For variables, just call out to the genVarRead function.
    genLValueRead' indexes (Var name) =
      do
        val <- genVarRead indexes name
        return (val, valtys ! name)

    genLValueRead :: LValue -> IO (Access, Type)
    genLValueRead = genLValueRead' []

    -- Get an LLVM value from the parameter, neglecting the type
    genExpValue :: Exp -> IO LLVM.ValueRef
    genExpValue param =
      do
        (val, _) <- genExp' param
        return (toValue val)

    genExp' :: Exp -> IO (Access, Type)
    genExp' (Binop op l r) =
      do
        (DirectAcc l', lty) <- genExp' l
        (DirectAcc r', rty) <- genExp' r
        case (op, lty, rty) of
          (Add, IntType _ _, IntType _ _) ->
            do
              val <- LLVM.buildAdd builder l' r' ""
              return (DirectAcc val, lty)
          (Add, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFAdd builder l' r' ""
              return (DirectAcc val, lty)
          (AddNW, IntType True _, IntType True _) ->
            do
              val <- LLVM.buildNSWAdd builder l' r' ""
              return (DirectAcc val, lty)
          (AddNW, IntType False _, IntType False _) ->
            do
              val <- LLVM.buildNUWAdd builder l' r' ""
              return (DirectAcc val, lty)
          (Sub, IntType _ _, IntType _ _) ->
            do
              val <- LLVM.buildSub builder l' r' ""
              return (DirectAcc val, lty)
          (Sub, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFSub builder l' r' ""
              return (DirectAcc val, lty)
          (SubNW, IntType True _, IntType True _) ->
            do
              val <- LLVM.buildNSWSub builder l' r' ""
              return (DirectAcc val, lty)
          (SubNW, IntType False _, IntType False _) ->
            do
              val <- LLVM.buildNUWSub builder l' r' ""
              return (DirectAcc val, lty)
          (Mul, IntType _ _, IntType _ _) ->
            do
              val <- LLVM.buildMul builder l' r' ""
              return (DirectAcc val, lty)
          (Mul, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFMul builder l' r' ""
              return (DirectAcc val, lty)
          (MulNW, IntType True _, IntType True _) ->
            do
              val <- LLVM.buildNSWMul builder l' r' ""
              return (DirectAcc val, lty)
          (MulNW, IntType False _, IntType False _) ->
            do
              val <- LLVM.buildNUWMul builder l' r' ""
              return (DirectAcc val, lty)
          (Div, IntType True _, IntType True _) ->
            do
              val <- LLVM.buildSDiv builder l' r' ""
              return (DirectAcc val, lty)
          (Div, IntType False _, IntType False _) ->
            do
              val <- LLVM.buildUDiv builder l' r' ""
              return (DirectAcc val, lty)
          (Div, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFDiv builder l' r' ""
              return (DirectAcc val, lty)
          (Mod, IntType True _, IntType True _) ->
            do
              val <- LLVM.buildSRem builder l' r' ""
              return (DirectAcc val, lty)
          (Mod, IntType False _, IntType False _) ->
            do
              val <- LLVM.buildURem builder l' r' ""
              return (DirectAcc val, lty)
          (Mod, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFRem builder l' r' ""
              return (DirectAcc val, lty)
          (And, IntType _ _, IntType _ _) ->
            do
              val <- LLVM.buildAnd builder l' r' ""
              return (DirectAcc val, lty)
          (Or, IntType _ _, IntType _ _) ->
            do
              val <- LLVM.buildOr builder l' r' ""
              return (DirectAcc val, lty)
          (Xor, IntType _ _, IntType _ _) ->
            do
              val <- LLVM.buildXor builder l' r' ""
              return (DirectAcc val, lty)
          (Shl, IntType _ _, IntType _ _) ->
            do
              val <- LLVM.buildShl builder l' r' ""
              return (DirectAcc val, lty)
          (Shr, IntType True _, IntType _ _) ->
            do
              val <- LLVM.buildAShr builder l' r' ""
              return (DirectAcc val, lty)
          (Shr, IntType False _, IntType _ _) ->
            do
              val <- LLVM.buildLShr builder l' r' ""
              return (DirectAcc val, lty)
          (Eq, IntType _ _, IntType _ _) ->
            do
              val <- LLVM.buildICmp builder LLVM.IntEQ l' r' ""
              return (DirectAcc val, booltype)
          (Neq, IntType _ _, IntType _ _) ->
            do
              val <- LLVM.buildICmp builder LLVM.IntNE l' r' ""
              return (DirectAcc val, booltype)
          (Ge, IntType True _, IntType True _) ->
            do
              val <- LLVM.buildICmp builder LLVM.IntSGE l' r' ""
              return (DirectAcc val, booltype)
          (Ge, IntType False _, IntType False _) ->
            do
              val <- LLVM.buildICmp builder LLVM.IntUGE l' r' ""
              return (DirectAcc val, booltype)
          (Gt, IntType True _, IntType True _) ->
            do
              val <- LLVM.buildICmp builder LLVM.IntSGT l' r' ""
              return (DirectAcc val, booltype)
          (Gt, IntType False _, IntType False _) ->
            do
              val <- LLVM.buildICmp builder LLVM.IntUGT l' r' ""
              return (DirectAcc val, booltype)
          (Le, IntType True _, IntType True _) ->
            do
              val <- LLVM.buildICmp builder LLVM.IntSLE l' r' ""
              return (DirectAcc val, booltype)
          (Le, IntType False _, IntType False _) ->
            do
              val <- LLVM.buildICmp builder LLVM.IntULE l' r' ""
              return (DirectAcc val, booltype)
          (Lt, IntType True _, IntType True _) ->
            do
              val <- LLVM.buildICmp builder LLVM.IntSLT l' r' ""
              return (DirectAcc val, booltype)
          (Lt, IntType False _, IntType False _) ->
            do
              val <- LLVM.buildICmp builder LLVM.IntULT l' r' ""
              return (DirectAcc val, booltype)
          (FOEq, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFCmp builder LLVM.RealOEQ l' r' ""
              return (DirectAcc val, booltype)
          (FONeq, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFCmp builder LLVM.RealONE l' r' ""
              return (DirectAcc val, booltype)
          (FOGe, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFCmp builder LLVM.RealOGE l' r' ""
              return (DirectAcc val, booltype)
          (FOLe, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFCmp builder LLVM.RealOLE l' r' ""
              return (DirectAcc val, booltype)
          (FOGt, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFCmp builder LLVM.RealOGT l' r' ""
              return (DirectAcc val, booltype)
          (FOLt, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFCmp builder LLVM.RealOLT l' r' ""
              return (DirectAcc val, booltype)
          (FUEq, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFCmp builder LLVM.RealUEQ l' r' ""
              return (DirectAcc val, booltype)
          (FUNeq, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFCmp builder LLVM.RealUNE l' r' ""
              return (DirectAcc val, booltype)
          (FUGe, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFCmp builder LLVM.RealUGE l' r' ""
              return (DirectAcc val, booltype)
          (FULe, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFCmp builder LLVM.RealULE l' r' ""
              return (DirectAcc val, booltype)
          (FUGt, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFCmp builder LLVM.RealUGT l' r' ""
              return (DirectAcc val, booltype)
          (FULt, FloatType _, FloatType _) ->
            do
              val <- LLVM.buildFCmp builder LLVM.RealULT l' r' ""
              return (DirectAcc val, booltype)
          _ -> error "Bad types for arguments to binary operator"
    genExp' (Call func args) =
      do
        (DirectAcc func', FuncType retty _) <- genExp' func
        args' <- mapM genExpValue args
        val <- LLVM.buildCall builder func' args' ""
        return (DirectAcc val, retty)
    genExp' (Unop op inner) =
      do
        (DirectAcc inner', ty) <- genExp' inner
        val <- case (op, ty) of
          (Neg, IntType _ _) -> LLVM.buildNeg builder inner' ""
          (Neg, FloatType _) -> LLVM.buildFNeg builder inner' ""
          (NegNW, IntType True _) -> LLVM.buildNSWNeg builder inner' ""
          (NegNW, IntType False _) -> LLVM.buildNUWNeg builder inner' ""
          (Not, IntType _ _) -> LLVM.buildNot builder inner' ""
          _ -> error "Bad type for argument to unary operator"
        return (DirectAcc val, ty)
    genExp' (Conv toty inner) =
      do
        toty' <- toLLVMType toty
        (DirectAcc inner', fromty) <- genExp' inner
        val <- case (fromty, toty) of
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
          (IntType True _, PtrType _) ->
            LLVM.buildIntToPtr builder inner' toty' ""
          (PtrType _, IntType True _) ->
            LLVM.buildPtrToInt builder inner' toty' ""
          (PtrType _, PtrType _) ->
            LLVM.buildPointerCast builder inner' toty' ""
          _ -> error "Cannot generate conversion for given types"
        return (DirectAcc val, toty)
    genExp' (LValue lval) = genLValueRead lval
    genExp' (AddrOf lval) = genLValueAddr lval
    genExp' (StructConst ty inits) =
      do
        inits' <- mapM (\expr -> genExp' expr >>= return . fst) inits
        return (StructAcc inits', ty)
    genExp' (ArrayConst ty inits) =
      do
        inits' <- mapM genExpValue inits
        ty' <- toLLVMType ty
        return (DirectAcc (LLVM.constArray ty' inits'), ty)
    genExp' (NumConst ty @ (IntType signed _) n) =
      do
        ty' <- toLLVMType ty
        return (DirectAcc (LLVM.constInt ty' n signed), ty)
    genExp' (NumConst _ _) =
      error "Bad type for numeric constant"
    -- XXX implement GCAlloc
    genExp' (GCAlloc _ _ _) = error "GCAlloc generation not implemented"
    genExp' (Cast _ _) = error "Cast generation not implemented"
  in
    genExp'

-- | Generate code for a statement, and update the value map
genStm :: Graph gr =>
          Module gr
       -- ^ The FlatIR module being compiled
       -> LLVM.ContextRef
       -- ^ The LLVM Context handle
       -> LLVM.BuilderRef
       -- ^ The LLVM Instruction builder handle
       -> Array Globalname LLVM.ValueRef
       -- ^ An array mapping global names to LLVM values
       -> Array Id Type
       -- ^ An array mapping variables to their FlatIR types
       -> UArray Typename LLVM.TypeRef
       -- ^ An array mapping type names to LLVM types
       -> ValMap
       -- ^ A map from Id's to Locations that hold their current value
       -> Stm
       -- ^ The statement being compiled
       -> IO ValMap
       -- ^ The updated valmap
genStm irmod ctx builder decls valtys typedefs valmap stm =
  let
    genExp' = genExp irmod ctx builder decls valtys typedefs valmap
    genWrite = VarAccess.genWrite builder valmap

    -- Generate code for reading an LValue.  Return an access and a type.
    genLValueWrite' :: Access -> [Index] -> LValue -> IO ValMap
    -- For dereferencing, get the inner as an expression, and treat it
    -- as a memory address.
    genLValueWrite' acc indexes (Deref inner) =
      do
        (DirectAcc inner', _) <- genExp' inner
        -- XXX get volatility from the pointer type
        genWrite acc indexes (MemLoc False inner')
    -- For an index, add to the list of indexes and continue.  We
    -- can't index without going through a dereference.
    genLValueWrite' acc indexes (Index (LValue lval) ind) =
      do
        (DirectAcc ind', _) <- genExp' ind
        genLValueWrite' acc (ValueInd ind' : indexes) lval
    genLValueWrite' _ _ (Index _ _) =
      error "Generating array index write to non-writable value"
    -- For a field, add to the list of indexes and continue.  A field
    -- is only writable if it goes through a dereference or if it is a
    -- field of a local or global variable.
    genLValueWrite' acc indexes (Field (LValue lval) field) =
      genLValueWrite' acc (FieldInd field : indexes) lval
    genLValueWrite' _ _ (Field _ _) =
      error "Generating field write to non-writable value"
    -- For globals, the LLVM value is actually a pointer.  Generate a
    -- write to a memory location using the global's value.
    genLValueWrite' acc indexes (Global name) =
      -- XXX get volatile from the global
      genWrite acc indexes (MemLoc False (decls ! name))
    -- For variables, just call out to the genVarWrite function.
    genLValueWrite' acc indexes (Var name) =
      genWrite acc indexes (getVarLocation valmap name)

    genLValueWrite :: Access -> LValue -> IO ValMap
    genLValueWrite acc = genLValueWrite' acc []
  in case stm of
    Do e ->
      do
        _ <- genExp' e
        return valmap
    Move lval rval ->
      do
        (acc, _) <- genExp' rval
        genLValueWrite acc lval

-- | Generate code for a transfer.
genTransfer :: Graph gr =>
               Module gr
            -- ^ The FlatIR module being compiled
            -> LLVM.ContextRef
            -- ^ The LLVM context handle
            -> LLVM.BuilderRef
            -- ^ The LLVM Instruction builder
            -> Array Globalname LLVM.ValueRef
            -- ^ An array mapping global names to LLVM values
            -> Array Id Type
            -- ^ An array mapping variable names to their types
            -> UArray Typename LLVM.TypeRef
            -- ^ An array mapping type names to LLVM types
            -> UArray Node LLVM.BasicBlockRef
            -- ^ An array mapping Node Ids to LLVM basic blocks
            -> ValMap
            -- ^ A map from Id's to Locations that hold their current value
            -> Transfer
            -- ^ The transfer being compiled
            -> IO LLVM.ValueRef
            -- ^ The LLVM transfer instruction
genTransfer irmod ctx builder decls valtys typedefs blocks valmap trans =
  let
    genExp' = genExp irmod ctx builder decls valtys typedefs valmap
  in case trans of
    Goto (Label l) -> LLVM.buildBr builder (blocks ! l)
    -- XXX compile these to selects if possible
    Case test cases (Label def) ->
      do
        (DirectAcc testval, _) <- genExp' test
        case cases of
          -- Compile binary case statements to if statements
          [(0, Label falsel)] ->
            LLVM.buildCondBr builder testval (blocks ! def) (blocks ! falsel)
          _ ->
            let
              numcases = length cases

              addCase :: LLVM.ValueRef -> (Integer, Label) -> IO ()
              addCase switch (num, Label label) =
                let
                  numval = LLVM.constInt LLVM.int32Type num False
                in
                  LLVM.addCase switch numval (blocks ! label)
            in do
              switch <- LLVM.buildSwitch builder testval (blocks ! def) numcases
              mapM_ (addCase switch) cases
              return switch
    Ret (Just ret) ->
      do
        (acc, _) <- genExp' ret
        case acc of
          DirectAcc val -> LLVM.buildRet builder val
          StructAcc fields ->
            LLVM.buildAggregateRet builder (map toValue (elems fields))
    Ret Nothing -> LLVM.buildRetVoid builder
    Unreachable -> LLVM.buildUnreachable builder

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

-- | This module contains code for generating LLVM code for
-- statements and transfers.
module SimpleIR.LLVMGen.CodeGen(
       genStm,
       genTransfer
       ) where

import Data.Array(Array)
import Data.Array.IArray
import Data.Array.Unboxed(UArray)
import Data.Graph.Inductive
import Data.Traversable
import Prelude hiding (mapM)
import SimpleIR
import SimpleIR.LLVMGen.LLVMValue
import SimpleIR.LLVMGen.VarAccess(Index(..), Access(..), Location(..), ValMap)

import qualified LLVM.Core as LLVM
import qualified SimpleIR.LLVMGen.Types as Types
import qualified SimpleIR.LLVMGen.Utils as Utils
import qualified SimpleIR.LLVMGen.VarAccess as VarAccess

getVarLocation = VarAccess.getVarLocation
zeroind = ValueInd (LLVM.constInt LLVM.int32Type 0 False)

-- Generate code for an expression
genExp :: Graph gr => Module gr -> LLVM.ContextRef -> LLVM.BuilderRef ->
          Array Globalname LLVM.ValueRef -> Array Id Type ->
          UArray Typename LLVM.TypeRef -> ValMap ->
          Exp -> IO (Access, Type)
genExp mod ctx builder decls valtys typedefs valmap =
  let
    getActualType = Utils.getActualType mod
    booltype = Utils.booltype
    getGlobalType = Utils.getGlobalType mod
    toLLVMType = Types.toLLVMType mod ctx typedefs
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
    -- For dereferences with indexes, generate a getelementptr
    genLValueAddr' indexes (Deref inner) =
      do
        (DirectAcc inner', ty) <- genExp' inner
        case getActualType ty of
          PtrType (GCObj _ _) -> 
            error "GCObject field addresses not implemented"
          PtrType (BasicObj innerty) ->
            let
              indexvals = map toValue (zeroind : indexes)
            in do
              val <- LLVM.buildGEP builder inner' indexvals ""
              return (val, ty)
    -- Indexes should only every be done on something being
    -- dereferenced, so we need only worry about LValue inners.
    genLValueAddr' indexes (Index (LValue lval) ind) =
      do
        (DirectAcc ind', _) <- genExp' ind
        (val, ty) <- genLValueAddr' (ValueInd ind' : indexes) lval
        case getActualType ty of
          PtrType (GCObj _ _) -> 
            error "GCObject field addresses not implemented"
          PtrType (BasicObj arrty) ->
            case getActualType arrty of
              ArrayType _ innerty ->
                return (val, PtrType (BasicObj innerty))
    -- Field accesses should also only go through a dereference here,
    -- or else the value is non-addressable.
    genLValueAddr' indexes (Field (LValue lval) field) =
      do
        (val, ty) <- genLValueAddr' (FieldInd field : indexes) lval
        case getActualType ty of
          PtrType (GCObj _ _) -> 
            error "GCObject field addresses not implemented"
          PtrType (BasicObj innerty) ->
            case getActualType innerty of
              StructType _ fields ->
                let
                  (_, _, ty) = fields ! field
                in
                  return (val, PtrType (BasicObj ty))
    genLValueAddr' _ (Field _ _) =
      error "Getting address of non-addressable value"
    -- For globals, the corresponding LLVM value is actually its
    -- address.
    genLValueAddr' indexes (Global name) =
      return (decls ! name, PtrType (BasicObj (getGlobalType name)))
    genLValueAddr' indexes (Var id) =
      do
        (out, volatile) <- genVarAddr indexes id
        -- XXX plug the volatility into the pointer type
        return (out, PtrType (BasicObj (valtys ! id)))

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
          PtrType (GCObj _ _) ->
            error "Reading GC object fields not implemented"
          PtrType (BasicObj innerty) ->
            do
              addr <- genGEP inner' indexes
              out <- LLVM.buildLoad builder addr ""
              return (DirectAcc out, innerty)
    -- For an index, add to the list of indexes and continue.  We
    -- can't index without going through a dereference.
    genLValueRead' indexes (Index (LValue lval) ind) =
      do
        (DirectAcc ind', _) <- genExp' ind
        (out, ty) <- genLValueRead' (ValueInd ind' : indexes) lval
        case getActualType ty of
          ArrayType _ innerty -> return (out, innerty)
    -- For a field, possibly stop, and possibly add to the list of
    -- indexes and keep going.
    genLValueRead' indexes (Field exp field) =
      let
        indexes' = FieldInd field : indexes
      in do
        (out, ty) <- case exp of
          -- If the inner is an LValue, keep going
          LValue lval -> genLValueRead' indexes' lval
          -- Otherwise, generate an extractvalue
          _ ->
            do
              (exp', ty) <- genExp' exp
              acc <- genExtractValue exp' indexes'
              return (acc, ty)
        case getActualType ty of
          StructType _ fields ->
            let
              (_, _, fieldty) = fields ! field
            in
              return (out, fieldty)
    -- For globals, the LLVM value is actually a pointer.  Generate a
    -- getelementptr and a load.
    genLValueRead' indexes (Global name) =
      do
        addr <- genGEP (decls ! name) indexes
        val <- LLVM.buildLoad builder addr ""
        -- XXX set volatile
        return (DirectAcc val, getGlobalType name)
    -- For variables, just call out to the genVarRead function.
    genLValueRead' indexes (Var id) =
      do
        val <- genVarRead indexes id
        return (val, valtys ! id)

    genLValueRead :: LValue -> IO (Access, Type)
    genLValueRead = genLValueRead' []

    -- Get an LLVM value from the parameter, neglecting the type
    genExpValue :: Exp -> IO LLVM.ValueRef
    genExpValue param =
      do
        (out, _) <- genExp' param
        return (toValue out)

    genExp' :: Exp -> IO (Access, Type)
    genExp' (Binop op l r) =
      do
        (DirectAcc l', lty) <- genExp' l
        (DirectAcc r', rty) <- genExp' r
        case (op, lty, rty) of
          (Add, IntType _ _, IntType _ _) ->
            do
              out <- LLVM.buildAdd builder l' r' ""
              return (DirectAcc out, lty)
          (Add, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFAdd builder l' r' ""
              return (DirectAcc out, lty)
          (AddNW, IntType True _, IntType True _) ->
            do
              out <- LLVM.buildNSWAdd builder l' r' ""
              return (DirectAcc out, lty)
          (AddNW, IntType False _, IntType False _) ->
            do
              out <- LLVM.buildNUWAdd builder l' r' ""
              return (DirectAcc out, lty)
          (Sub, IntType _ _, IntType _ _) ->
            do
              out <- LLVM.buildSub builder l' r' ""
              return (DirectAcc out, lty)
          (Sub, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFSub builder l' r' ""
              return (DirectAcc out, lty)
          (SubNW, IntType True _, IntType True _) ->
            do
              out <- LLVM.buildNSWSub builder l' r' ""
              return (DirectAcc out, lty)
          (SubNW, IntType False _, IntType False _) ->
            do
              out <- LLVM.buildNUWSub builder l' r' ""
              return (DirectAcc out, lty)
          (Mul, IntType _ _, IntType _ _) ->
            do
              out <- LLVM.buildMul builder l' r' ""
              return (DirectAcc out, lty)
          (Mul, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFMul builder l' r' ""
              return (DirectAcc out, lty)
          (MulNW, IntType True _, IntType True _) ->
            do
              out <- LLVM.buildNSWMul builder l' r' ""
              return (DirectAcc out, lty)
          (MulNW, IntType False _, IntType False _) ->
            do
              out <- LLVM.buildNUWMul builder l' r' ""
              return (DirectAcc out, lty)
          (Div, IntType True _, IntType True _) ->
            do
              out <- LLVM.buildSDiv builder l' r' ""
              return (DirectAcc out, lty)
          (Div, IntType False _, IntType False _) ->
            do
              out <- LLVM.buildUDiv builder l' r' ""
              return (DirectAcc out, lty)
          (Div, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFDiv builder l' r' ""
              return (DirectAcc out, lty)
          (Mod, IntType True _, IntType True _) ->
            do
              out <- LLVM.buildSRem builder l' r' ""
              return (DirectAcc out, lty)
          (Mod, IntType False _, IntType False _) ->
            do
              out <- LLVM.buildURem builder l' r' ""
              return (DirectAcc out, lty)
          (Mod, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFRem builder l' r' ""
              return (DirectAcc out, lty)
          (And, IntType _ _, IntType _ _) ->
            do
              out <- LLVM.buildAnd builder l' r' ""
              return (DirectAcc out, lty)
          (Or, IntType _ _, IntType _ _) ->
            do
              out <- LLVM.buildOr builder l' r' ""
              return (DirectAcc out, lty)
          (Xor, IntType _ _, IntType _ _) ->
            do
              out <- LLVM.buildXor builder l' r' ""
              return (DirectAcc out, lty)
          (Shl, IntType _ _, IntType _ _) ->
            do
              out <- LLVM.buildShl builder l' r' ""
              return (DirectAcc out, lty)
          (Shr, IntType True _, IntType _ _) ->
            do
              out <- LLVM.buildAShr builder l' r' ""
              return (DirectAcc out, lty)
          (Shr, IntType False _, IntType _ _) ->
            do
              out <- LLVM.buildLShr builder l' r' ""
              return (DirectAcc out, lty)
          (Eq, IntType _ _, IntType _ _) ->
            do
              out <- LLVM.buildICmp builder LLVM.IntEQ l' r' ""
              return (DirectAcc out, booltype)
          (Neq, IntType _ _, IntType _ _) ->
            do
              out <- LLVM.buildICmp builder LLVM.IntNE l' r' ""
              return (DirectAcc out, booltype)
          (Ge, IntType True _, IntType True _) ->
            do
              out <- LLVM.buildICmp builder LLVM.IntSGE l' r' ""
              return (DirectAcc out, booltype)
          (Ge, IntType False _, IntType False _) ->
            do
              out <- LLVM.buildICmp builder LLVM.IntUGE l' r' ""
              return (DirectAcc out, booltype)
          (Gt, IntType True _, IntType True _) ->
            do
              out <- LLVM.buildICmp builder LLVM.IntSGT l' r' ""
              return (DirectAcc out, booltype)
          (Gt, IntType False _, IntType False _) ->
            do
              out <- LLVM.buildICmp builder LLVM.IntUGT l' r' ""
              return (DirectAcc out, booltype)
          (Le, IntType True _, IntType True _) ->
            do
              out <- LLVM.buildICmp builder LLVM.IntSLE l' r' ""
              return (DirectAcc out, booltype)
          (Le, IntType False _, IntType False _) ->
            do
              out <- LLVM.buildICmp builder LLVM.IntULE l' r' ""
              return (DirectAcc out, booltype)
          (Lt, IntType True _, IntType True _) ->
            do
              out <- LLVM.buildICmp builder LLVM.IntSLT l' r' ""
              return (DirectAcc out, booltype)
          (Lt, IntType False _, IntType False _) ->
            do
              out <- LLVM.buildICmp builder LLVM.IntULT l' r' ""
              return (DirectAcc out, booltype)
          (FOEq, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFCmp builder LLVM.RealOEQ l' r' ""
              return (DirectAcc out, booltype)
          (FONeq, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFCmp builder LLVM.RealONE l' r' ""
              return (DirectAcc out, booltype)
          (FOGe, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFCmp builder LLVM.RealOGE l' r' ""
              return (DirectAcc out, booltype)
          (FOLe, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFCmp builder LLVM.RealOLE l' r' ""
              return (DirectAcc out, booltype)
          (FOGt, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFCmp builder LLVM.RealOGT l' r' ""
              return (DirectAcc out, booltype)
          (FOLt, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFCmp builder LLVM.RealOLT l' r' ""
              return (DirectAcc out, booltype)
          (FUEq, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFCmp builder LLVM.RealUEQ l' r' ""
              return (DirectAcc out, booltype)
          (FUNeq, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFCmp builder LLVM.RealUNE l' r' ""
              return (DirectAcc out, booltype)
          (FUGe, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFCmp builder LLVM.RealUGE l' r' ""
              return (DirectAcc out, booltype)
          (FULe, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFCmp builder LLVM.RealULE l' r' ""
              return (DirectAcc out, booltype)
          (FUGt, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFCmp builder LLVM.RealUGT l' r' ""
              return (DirectAcc out, booltype)
          (FULt, FloatType _, FloatType _) ->
            do
              out <- LLVM.buildFCmp builder LLVM.RealULT l' r' ""
              return (DirectAcc out, booltype)
    genExp' (Call func args) =
      do
        (DirectAcc func', FuncType retty _) <- genExp' func
        args' <- mapM genExpValue args
        out <- LLVM.buildCall builder func' args' ""
        return (DirectAcc out, retty)
    genExp' (Unop op inner) =
      do
        (DirectAcc inner', ty) <- genExp' inner
        out <- case (op, ty) of
          (Neg, IntType _ _) -> LLVM.buildNeg builder inner' ""
          (Neg, FloatType _) -> LLVM.buildFNeg builder inner' ""
          (NegNW, IntType True _) -> LLVM.buildNSWNeg builder inner' ""
          (NegNW, IntType False _) -> LLVM.buildNUWNeg builder inner' ""
          (Not, IntType _ _) -> LLVM.buildNot builder inner' ""
        return (DirectAcc out, ty)
    genExp' (Conv toty inner) =
      do
        toty' <- toLLVMType toty
        (DirectAcc inner', fromty) <- genExp' inner
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
        return (DirectAcc out, toty)
    genExp' (LValue lval) = genLValueRead lval
    genExp' (AddrOf lval) = genLValueAddr lval
    genExp' (StructConst ty inits) =
      do
        inits' <- mapM (\exp -> genExp' exp >>= return . fst) inits
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
  in
    genExp'

-- | Generate code for a statement
genStm :: Graph gr => Module gr -> LLVM.ContextRef -> LLVM.BuilderRef ->
          Array Globalname LLVM.ValueRef -> Array Id Type ->
          UArray Typename LLVM.TypeRef -> ValMap -> Stm -> IO ValMap
genStm mod ctx builder decls valtys typedefs valmap stm =
  let
    genExp' = genExp mod ctx builder decls valtys typedefs valmap
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
    -- For a field, add to the list of indexes and continue.  A field
    -- is only writable if it goes through a dereference or if it is a
    -- field of a local or global variable.
    genLValueWrite' acc indexes (Field (LValue lval) field) =
      genLValueWrite' acc (FieldInd field : indexes) lval
    -- For globals, the LLVM value is actually a pointer.  Generate a
    -- write to a memory location using the global's value.
    genLValueWrite' acc indexes (Global name) =
      -- XXX get volatile from the global
      genWrite acc indexes (MemLoc False (decls ! name))
    -- For variables, just call out to the genVarWrite function.
    genLValueWrite' acc indexes (Var id) =
      genWrite acc indexes (getVarLocation valmap id)

    genLValueWrite :: Access -> LValue -> IO ValMap
    genLValueWrite acc = genLValueWrite' acc []
  in case stm of
    Do e ->
      do
        genExp' e
        return valmap
    Move lval rval ->
      do
        (acc, _) <- genExp' rval
        genLValueWrite acc lval

-- | Generate code for a transfer.
genTransfer :: Graph gr => Module gr -> LLVM.ContextRef -> LLVM.BuilderRef ->
               Array Globalname LLVM.ValueRef -> Array Id Type ->
               UArray Typename LLVM.TypeRef -> UArray Node LLVM.BasicBlockRef ->
               ValMap -> Transfer -> IO LLVM.ValueRef
genTransfer mod ctx builder decls valtys typedefs blocks valmap trans =
  let
    genExp' = genExp mod ctx builder decls valtys typedefs valmap
  in case trans of
    Goto (Label l) -> LLVM.buildBr builder (blocks ! l)
    Case test cases (Label def) ->
      do
        (DirectAcc testval, _) <- genExp' test
        case cases of
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

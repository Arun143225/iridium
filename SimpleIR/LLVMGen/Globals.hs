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

-- | This module contains code for generating declarations and
-- definitions for globals.  This, combined with CodeGen, represents
-- the "meat" of compilation.
module SimpleIR.LLVMGen.Globals(
       genDecls,
       genDefs
       ) where

import Data.Array(Array)
import Data.Array.IArray
import Data.Array.Unboxed(UArray)
import Data.BitArray.IO
import Data.Foldable
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.DomFrontier
import Data.Traversable
import Data.Tree
import Data.Word
import Prelude hiding (mapM_, mapM, foldr, foldl, sequence)
import SimpleIR
import SimpleIR.LLVMGen.VarAccess(Location(..), ValMap, getVarLocation)

import qualified Data.Map as Map
import qualified LLVM.Core as LLVM
import qualified SimpleIR.LLVMGen.ConstValue as ConstValue
import qualified SimpleIR.LLVMGen.CodeGen as CodeGen
import qualified SimpleIR.LLVMGen.Types as Types

-- Run over all the global values, and generate declarations for
-- them all.
genDecls :: Graph gr => Module gr -> LLVM.ModuleRef -> LLVM.ContextRef ->
                        UArray Typename LLVM.TypeRef ->
                        IO (Array Globalname LLVM.ValueRef)
genDecls m @ (Module { modGlobals = globals }) mod ctx typedefs =
  let
    toLLVMType = Types.toLLVMType m ctx typedefs

    genDecl :: Graph gr => Global gr -> IO LLVM.ValueRef
    genDecl (Function { funcName = name, funcRetTy = resty,
                        funcParams = args, funcValTys = scope }) =
      do
        argtys <- mapM (toLLVMType . (!) scope) args
        resty <- toLLVMType resty
        LLVM.addFunction mod name (LLVM.functionType resty argtys False)
    genDecl (GlobalVar { gvarName = name, gvarTy = ty }) =
      do
        llvmty <- toLLVMType ty
        LLVM.addGlobal mod llvmty name
  in
    mapM genDecl globals

-- Actually generate the definitions for all globals
genDefs :: Graph gr => Module gr -> LLVM.ContextRef ->
           Array Globalname LLVM.ValueRef ->
           UArray Typename LLVM.TypeRef -> IO ()
genDefs mod @ (Module { modGlobals = globals }) ctx decls typedefs =
  let
    genConst = ConstValue.genConst mod ctx typedefs decls
    toLLVMType = Types.toLLVMType mod ctx typedefs

    -- Add a definition to a global.  This function does all the
    -- real work
    addDef :: Graph gr => LLVM.BuilderRef -> (Globalname, Global gr) -> IO ()
    -- Globals are pretty simple, generate the initializer and set
    -- it for the variable
    addDef _ (gname, GlobalVar { gvarName = name, gvarInit = Just exp }) =
      do
        (init, _) <- genConst exp
        LLVM.setInitializer (decls ! gname) init
    addDef _ (gname, GlobalVar { gvarName = name, gvarInit = Nothing }) = return ()
    addDef builder (name, Function { funcBody = Just (Body (Label entry) graph),
                                     funcValTys = valtys,
                                     funcParams = params }) =
      let
        genStm = CodeGen.genStm mod ctx builder decls valtys typedefs
        func = (decls ! name)
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
                    return (StructLoc (listArray (bounds fields)
                                                 (reverse fieldlist)),
                            newind, valmap)
                getArgVal baseval _ (newind, valmap) =
                  return (BindLoc baseval, newind, valmap)
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
                return (StructLoc (listArray (bounds fields)
                                             (reverse fieldlist)),
                        newind, valmap)
            getVal ty (newind, valmap) =
              do
                ty' <- toLLVMType ty
                return (BindLoc (LLVM.getUndef ty'), newind, valmap)

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
            genTransfer = CodeGen.genTransfer mod ctx builder decls valtys
                                              typedefs blocks

            -- Take the value map, and add the incoming edges to a
            -- successor block.  Called when leaving a block on
            -- all its successors.
            bindPhis :: LLVM.BasicBlockRef -> ValMap -> Node -> IO ()
            bindPhis from valmap to =
              let
                phis = phiarr ! to
                fromval = LLVM.basicBlockAsValue from

                bindPhi :: (Id, LLVM.ValueRef) -> IO ()
                bindPhi (id, phival) =
                  let
                    BindLoc oldval = getVarLocation valmap id
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
                addPhi (Id ind, phival) = Map.insert ind (BindLoc phival)
              in do
                foldr addPhi valmap phis

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
        tyarr <- mapM toLLVMType valtys
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

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

-- | This module contains code for generating declarations and
-- definitions for globals.  This, combined with CodeGen, represents
-- the "meat" of compilation.
module IR.FlatIR.LLVMGen.Globals(
       genDecls,
       genDefs
       ) where

import Control.Monad.LLVMGen.Globals.Class
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
import IR.FlatIR.Syntax
import IR.FlatIR.LLVMGen.VarAccess(Location(..), ValMap, getVarLocation)
import Prelude hiding (mapM_, mapM, foldr, foldl, sequence)

import qualified Data.Map as Map
import qualified IR.FlatIR.LLVMGen.ConstValue as ConstValue
import qualified IR.FlatIR.LLVMGen.CodeGen as CodeGen
import qualified IR.FlatIR.LLVMGen.Types as Types
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Name as LLVM

-- | Run over all the global values, and generate declarations for
-- them all, but do not define them.  This is necessary, because of
-- cross-referencing between globals.
genDecls :: (MonadGlobals m, Graph gr) =>
            Module gr
         -- ^ The FlatIR module being translated
         -> m (Array Globalname LLVM.Name)
         -- ^ An array mapping Globalnames to LLVM global variable handles
genDecls m @ Module { modGlobals = globals } =
  let
    getname :: Graph gr => Global gr -> LLVM.Name
    getname Function { funcName = Just DeclNames { linkageName = linkname } } =
      return (Name linkname)
    getname Function { funcName = Nothing } =
      do
        name <- createGlobalID
        return (UnName name)
    getname GlobalVar { gvarName = Just DeclNames { linkageName = linkname } } =
      return (Name linkname)
    getname GlobalVar { gvarName = Nothing } =
      do
        name <- createGlobalID
        return (UnName name)
  in
    mapM getname globals

-- | Actually generate the definitions for all globals.  This goes
-- back and fills in all the definitions for the globals.
genDefs :: Graph gr =>
           Module gr
        -- ^ The FlatIR module being translated
        -> LLVM.ContextRef
        -- ^ The LLVM Context handle
        -> Array Globalname LLVM.ValueRef
        -- ^ An array mapping Globalnames to LLVM global variable handles
        -> UArray Typename LLVM.TypeRef
        -- ^ An array mapping Typenames to LLVM Type handles
        -> IO ()
genDefs irmod @ Module { modGlobals = globals } ctx decls typedefs =
  let
    genConst = ConstValue.genConst irmod ctx typedefs decls
    toLLVMType = Types.toLLVMType irmod ctx typedefs

    -- Add a definition to a global.  This function does all the
    -- real work
    addDef :: Graph gr => LLVM.BuilderRef -> (Globalname, Global gr) -> IO ()
    -- Globals are pretty simple, generate the initializer and set
    -- it for the variable
    addDef _ (gname, GlobalVar { gvarInit = Just expr }) =
      do
        (val, _) <- genConst expr
        LLVM.setInitializer (decls ! gname) val
    addDef _ (_, GlobalVar { gvarInit = Nothing }) =
      return ()
    addDef builder (name, Function { funcBody = Just (Body (Label entry) graph),
                                     funcValTys = valtys,
                                     funcParams = params }) =
      let
        genStm = CodeGen.genStm irmod ctx builder decls valtys typedefs
        func = (decls ! name)
        [dfstree] = dff [entry] graph
        entrynode : _ = foldr (:) [] dfstree
        noderange @ (startnode, endnode) = nodeRange graph
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
            return (array noderange vals)

        -- Build the sets of phi-values for each basic block
        buildPhiSets :: IO [(Node, [Id])]
        buildPhiSets =
          let
            domfronts' = domFrontiers graph entry

            domfronts :: Array Node [Node]
            domfronts = array noderange domfronts'

            getIndex :: Node -> Id -> Int
            getIndex nodeid (Id varname) =
              let
                nodeid' = nodeid - startnode
                varname' = (fromIntegral varname) - (fromIntegral startid)
                nodespan = (fromIntegral endid) - (fromIntegral startid) + 1
              in
                (nodeid' * nodespan) + varname'

            -- Add id to the phi-set for node
            addPhi :: IOBitArray -> Node -> Id -> IO ()
            addPhi arr nodeid var =
              let
                domset = domfronts ! nodeid
                appfun nodeid' = writeBit arr (getIndex nodeid' var) True
              in
                mapM_ appfun domset

            -- Translate the bit array into a list of ids
            getPhiSet :: IOBitArray -> Node -> IO (Node, [Id])
            getPhiSet sets nodeid =
              let
                foldfun :: [Id] -> Id -> IO [Id]
                foldfun phiset var =
                  do
                    bit <- readBit sets (getIndex nodeid var)
                    if bit
                      then return (var : phiset)
                      else return phiset
              in do
                front <- foldlM foldfun [] valids
                return (nodeid, front)

            -- Run through a node, add anything it modifies to the
            -- the phi-set of each node in its dominance frontier.
            buildPhiSet :: IOBitArray -> Node -> IO ()
            buildPhiSet modset node =
              let
                Just (Block { blockStms = stms }) = lab graph node

                appfun (Move { moveDst = Var { varName = name' } }) =
                  addPhi modset node name'
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
                mapvars var =
                  do
                    phi <- LLVM.buildPhi builder (tyarr ! var) ""
                    return (var, phi)
              in do
                LLVM.positionAtEnd builder (blocks ! node)
                phis <- mapM mapvars ids
                return (node, phis)
          in do
            vals <- mapM mapblocks phiset
            return (array (bounds blocks) vals)

        -- The initial value map contains just the arguments,
        -- and undefs for everything else.
        initValMap :: IO (Word, ValMap)
        initValMap =
          let
            -- Add arguments to the value map
            addArg :: (Word, ValMap) -> (Int, Id) -> IO (Word, ValMap)
            addArg vmap (arg, var @ (Id ind)) =
              let
                paramty = valtys ! var
                param = LLVM.getParam func arg

                getArgVal :: LLVM.ValueRef -> Type -> (Word, ValMap) ->
                             IO (Location, Word, ValMap)
                getArgVal baseval (StructType { structFields = fields }) vmap' =
                  let
                    foldfun (vmap'', inds) (Fieldname field,
                                          (_, _, fieldty)) =
                      do
                        newbase <- LLVM.buildExtractValue builder
                                   baseval field ""
                        (loc, newind, valmap) <-
                          getArgVal newbase fieldty vmap''
                        return ((newind + 1, Map.insert newind loc valmap),
                                newind : inds)
                  in do
                    ((newind, valmap), fieldlist) <-
                      foldlM foldfun (vmap', []) (assocs fields)
                    return (StructLoc (listArray (bounds fields)
                                                 (reverse fieldlist)),
                            newind, valmap)
                getArgVal baseval _ (newind, valmap) =
                  return (BindLoc baseval, newind, valmap)
              in do
                (loc, newind, valmap) <- getArgVal param paramty vmap
                return (newind, Map.insert ind loc valmap)

            getVal :: Type -> (Word, ValMap) -> IO (Location, Word, ValMap)
            getVal (StructType { structFields = fields }) vmap =
              let
                foldfun (vmap', inds) (_, (_, _, fieldty)) =
                  do
                    (loc, newind, valmap) <- getVal fieldty vmap'
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
            addUndef vmap @ (_, valmap) var @ (Id ind) =
              case Map.lookup ind valmap of
                Just _ -> return vmap
                Nothing ->
                  do
                    (loc, newind, valmap') <- getVal (valtys ! var) vmap
                    return (newind, Map.insert ind loc valmap')

            (_, Id maxval) = bounds valtys
            initmap = (maxval + 1, Map.empty)
            arginds = zip [0..length params] params
          in do
            withArgs <- foldlM addArg initmap arginds
            foldlM addUndef withArgs (indices valtys)

        -- Generate the instructions for a basic block
        genInstrs :: UArray Node LLVM.BasicBlockRef ->
                     Array Node [(Id, LLVM.ValueRef)] ->
                     Array Id LLVM.TypeRef -> LLVM.BasicBlockRef ->
                     LLVM.BuilderRef -> ValMap -> IO ()
        genInstrs blocks phiarr _ entryblock currbuilder valmap =
          let
            genTransfer = CodeGen.genTransfer irmod ctx currbuilder decls
                                              valtys typedefs blocks

            -- Take the value map, and add the incoming edges to a
            -- successor block.  Called when leaving a block on
            -- all its successors.
            bindPhis :: LLVM.BasicBlockRef -> ValMap -> Node -> IO ()
            bindPhis from valmap' to =
              let
                phis = phiarr ! to
                fromval = LLVM.basicBlockAsValue from

                bindPhi :: (Id, LLVM.ValueRef) -> IO ()
                bindPhi (var, phival) =
                  let
                    BindLoc oldval = getVarLocation valmap' var
                  in do
                    LLVM.addIncoming phival [(oldval, fromval)]
              in
                mapM_ bindPhi phis

            -- Replace all values with corresponding phis when
            -- entering a block.
            addPhiVals :: Node -> ValMap -> ValMap
            addPhiVals nodeid valmap' =
              let
                phis = phiarr ! nodeid

                addPhi :: (Id, LLVM.ValueRef) -> ValMap -> ValMap
                addPhi (Id ind, phival) = Map.insert ind (BindLoc phival)
              in do
                foldr addPhi valmap' phis

            -- Traverse the CFG.  This takes a DFS tree as an argument.
            traverseCFG :: ValMap -> Tree Node -> IO ()
            traverseCFG valmapin (Node { rootLabel = curr,
                                         subForest = nexts }) =
              let
                valmapphis = addPhiVals curr valmapin
                Just (Block { blockStms = stms, blockXfer = trans }) =
                  lab graph curr
                currblock = blocks ! curr
                outs = suc graph curr
              in do
                _ <- LLVM.positionAtEnd currbuilder currblock
                valmapstms <- foldlM genStm valmapphis stms
                _ <- genTransfer valmapstms trans
                _ <- mapM_ (bindPhis currblock valmapstms) outs
                mapM_ (traverseCFG valmapstms) nexts
          in do
            _ <- bindPhis entryblock valmap entrynode
            traverseCFG valmap dfstree
      in do
        tyarr <- mapM toLLVMType valtys
        entryblock <- LLVM.appendBasicBlockInContext ctx func "entry"
        blocks <- genBlocks
        _ <- LLVM.positionAtEnd builder entryblock
        (_, valmap) <- initValMap
        _ <- LLVM.buildBr builder (blocks ! entrynode)
        phiset <- buildPhiSets
        phiarr <- genPhis blocks tyarr phiset
        genInstrs blocks phiarr tyarr entryblock builder valmap
    addDef _ (_, Function { funcBody = Nothing }) =
      return ()
  in do
    builder <- LLVM.createBuilderInContext ctx
    mapM_ (addDef builder) (assocs globals)

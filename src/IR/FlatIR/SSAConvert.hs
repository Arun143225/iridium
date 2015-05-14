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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

-- | This module contains code to convert the statement form of FlatIR
-- to the SSA form.
module IR.FlatIR.SSAConvert(
       ssaConvert
       ) where

import Data.BitArray.ST
import Data.Graph.Inductive.Graph
import IR.FlatIR.Syntax
import Prelude hiding (mapM_, mapM, foldr, foldl, sequence)

type TransState = (Word, [(Word, Type)])

-- | Convert a global to SSA form.  Only changes functions.
ssaConvertGlobal :: Graph gr => Global Stm gr -> Global Bind gr
ssaConvertGlobal GlobalVar { gvarName = name, gvarTy = ty, gvarInit = initval,
                             gvarMutability = mut, gvarPos = pos } =
  GlobalVar { gvarName = name, gvarTy = ty, gvarInit = initval,
              gvarMutability = mut, gvarPos = pos }
ssaConvertGlobal f @ Function { funcBody = Nothing } = f { funcBody = Nothing }
ssaConvertGlobal f @ Function { funcValTys = valtys, funcParams = params,
                                funcBody = Just Body { bodyEntry = entry,
                                                       bodyCFG = body } } =
  let
    -- Get a DFS tree from a DFS forest (we shouldn't have
    -- disconnected nodes).
    [dfstree] = dff [entry] graph
    -- Get the first node in the DFS tree
    entrynode : _ = foldr (:) [] dfstree
    noderange @ (startnode, endnode) = nodeRange graph
    nodelist = nodes graph
    (Id startid, Id endid) = bounds valtys
    valids = indices valtys

    -- How this works: ultimately, we'll do a walk over all the
    -- instructions, mapping Id's in the statement form to Id's in the
    -- SSA form.  This is mostly straightforward; we'll have a map
    -- from old Id's to new Id's that we update whenever there's an
    -- assignment.  The hardest part is figuring out where to place
    -- the Phi-nodes.
    --
    -- To do that, we use the standard dominance frontier technique
    -- and build a bitmap representing which Id's get replated by
    -- Phi-nodes at the beginnings of which blocks.  Ultimately, we
    -- get a mapping from Node's to lists of (old) Id's, representing which
    -- ones to replace.

    -- First thing we need is a way to figure out which Id's get phi
    -- nodes in a given block.  To this end, we build the phi-sets.
    -- We do the crunching with bitarrays to make it fast.
    phiSet :: Node -> [Id]
    phiSet =
      let
        -- Convert a node and an (old) Id into an index to be used in
        -- the bit array representing the phi-sets.
        getIndex :: Node -> Id -> Int
        getIndex nodeid (Id varname) =
          let
            nodeid' = nodeid - startnode
            varname' = (fromIntegral varname) - (fromIntegral startid)
            nodespan = (fromIntegral endid) - (fromIntegral startid) + 1
          in
            (nodeid' * nodespan) + varname'

        -- The sets of phi-values for each basic block.  This is a bit
        -- array, which we expect to access using getIndex.  For a
        -- given Node and (old) Id, this array contains a bit
        -- indicating whether or not a phi-node needs to be generated
        -- for that Id at the start of that Node.
        phiBits :: BitArray
        phiBits =
          let
            domfronts' = domFrontiers graph entry

            -- Dominance frontiers as an array
            domfronts :: Array Node [Node]
            domfronts = array noderange domfronts'

            -- Add id to the phi-set for node.  Phi-sets are the sets of
            -- phi-nodes to be generated for a given basic block.
            addPhi :: STBitArray -> Node -> Id -> ST Int ()
            addPhi arr nodeid var =
              let
                domset = domfronts ! nodeid
                appfun nodeid' = writeBit arr (getIndex nodeid' var) True
              in
               mapM_ appfun domset

            -- Run through a node, add anything it modifies to the
            -- the phi-set of each node in its dominance frontier.
            buildPhiSet :: STBitArray -> Node -> ST Int ()
            buildPhiSet modset node =
              let
                Just (Block { blockStms = stms }) = lab graph node

                appfun (Move { moveDst = Var { varName = name' } }) =
                  addPhi modset node name'
                appfun _ = return ()
              in do
                mapM_ appfun stms

            -- Run through all nodes, build the phi-sets for them all.
            buildPhiSets :: ST Int BitArray
            buildPhiSets =
              do
                sets <- newBitArray ((getIndex startnode (Id startid)),
                                     (getIndex endnode (Id endid))) False
                mapM_ (buildPhiSet sets) nodelist
                -- unsafeFreeze is fine, this is the only reference to it
                unsafeFreezeBitArray sets
          in
            runST buildPhiSets

        -- Extract a list of (old) Id's representing the phi set for a
        -- given Node.  This indicates which (old) Id's need to have a
        -- phi-node at the start of the Node.
        getPhiSet :: Node -> [Id]
        getPhiSet nodeid =
          let
            foldfun :: [Id] -> Id -> [Id]
            foldfun phiset var =
              if lookupBit phiBits (getIndex nodeid var)
                then return (var : phiset)
                else return phiset
          in
            foldr foldfun [] valids

        -- Save all the phi sets
        phiSets :: Array Node [Id]
        phiSets = array noderange (map (\n -> (n, getPhiSet n)) nodelist)
      in
        (phiSets !)

    -- Update the value map to replace all values with phi-values.
    -- This is meant to be called when entering a block.
    addPhiVals :: Node -> ValMap -> ValMap
    addPhiVals nodeid valmap' =
      let
        -- Get the phi set
        phis = phiSet nodeid

        -- XXX need to get a fresh Id for the phi nodes
        addPhi :: (Id, LLVM.ValueRef) -> ValMap -> ValMap
        addPhi (Id ind, phival) = Map.insert ind (BindLoc phival)
      in do
        foldr addPhi valmap' phis

    -- Now, we go ahead and rename all the (old) Id's.  We do this
    -- by keeping a map from old to new values, updating it for
    -- each statement, and using it to rewrite all the expressions
    -- and transfers.
    --
    -- In order to rewrite a block, we need to know the map at the
    -- end of rewriting all of its predecessors, except for places
    -- where we insert a phi node for a given value.  Because
    -- phi-nodes happen wherever we have a join or a loop, a
    -- depth-first traversal works fine for our rewriting.

    -- Traverse the CFG.  This takes a DFS tree as an argument.

    -- XXX this function needs to return two things: data to build a
    -- new graph, and data to build the phi-nodes.  We will
    -- subsequently map over the node data and add all the phi nodes.
    -- The edge data for the new CFG is taken directly from the old
    -- graph.

    -- XXX This needs to be in a state monad.  We need to build up a
    -- new type array and also generate fresh variable names.
    traverseCFG :: ValMap -> Tree Node
                -> ([(Node, ([Bind], Transfer, Pos))], Map Id Bind)
                -- ^ The first component is a list of node data.  The
                -- second is a map from (old) Id's to Phi-nodes.
    traverseCFG valmapin (Node { rootLabel = curr, subForest = nexts }) =
      let
        -- First, update the valmap with any phis
        valmapphis = addPhiVals curr valmapin
        -- Get the current block data
        Just (Block { blockStms = stms, blockXfer = trans }) = lab graph curr
        -- Walk over the statements, translating them, and updating the valmap
        (valmapstms, ssastms) = mapAccumL genStm valmapphis stms
        -- Translate the end-of-block transfer
        ssatrans = ssaConvertTransfer valmapstms trans
        -- XXX Now we need to recurse down the tree, so that the
        -- phi-builder map will actually contain the phis that we want
        -- it to.

        -- XXX now we need to look at all our successors' phi-sets,
        -- and add the values that we contribute to a map from
        -- phi-values to incoming value lists.
        -- All successors
        successors = suc graph curr
      in

    -- The last thing we need to do is insert all the phi-nodes
    -- for each block, and connect up their incoming values.
    completeCFG :: Map Id Bind
                -- ^ Map from (old) Ids to (new) phis
                -> [(Node, ([Bind], Transfer, Pos))]
                -- ^ Partial node data
                -> (Node, Block Bind)
                -- ^ Completed node data
    completeCFG phimap nodedata =
      let
        -- Take the partial node data, look up the phi-set, then use
        -- that to get the new phi instruction from the phi map.
        completeBlock :: (Node, ([Bind], Transfer, Pos))
                      -- ^ Partial node data
                      -> (Node, Block Bind)
                      -- ^ Completed node data
        completeBlock phimap (nodeid, (body, trans, p)) =
          let
            -- (old) Id's from the phi-set
            oldids = phiSet nodeid
            -- (new) Phi instructions
            phis = map (phimap !) oldids
          in
           Block { blockBody = phis ++ body, blockXfer = trans, blockPos = p }

        newnodes = map completeBlock nodedata
      in
        mkGraph newnodes (labEdges body)
  in
    f { funcBody = Just Body { bodyEntry = entry, {- XXX the new CFG -} } }

ssaConvertExp :: ValMap -> Exp -> Exp
ssaConvertExp valmap ex = -- XXX rename the exp using the valmap

-- | Convert a statement to the SSA form of the language
ssaConvertStm :: ValMap -> Stm -> (ValMap, Stm)
-- Move's need to be converted into Bind's, and the value map needs to
-- be update.
ssaConvertStm Move { moveDst = dst, moveSrc = src, movePos = pos } =
  -- XXX generate a fresh variable name, update the value map
-- Do's and Effect's are the same thing in both languages
ssaConvertStm valmap (Do ex) = (valmap, Effect (ssaConvertExp valmap ex))

-- | Convert a transfer to the SSA form language.  This process
-- consists solely of mapping identifiers; transfers don't do any
-- binding, and are the same in both languages.
ssaConvertTransfer :: ValMap -> Transfer
ssaConvertTransfer valmap trans @ Case { caseVal = val } =
  trans { caseVal = ssaConvertExp valmap val }
ssaConvertTransfer valmap trans @ Ret { retVal = val } =
  trans { retVal = ssaConvertExp valmap val }
ssaConvertTransfer _ trans = trans

-- | Convert a module to SSA form.
ssaConvert :: Graph gr => Module Stm gr -> Module Bind gr
ssaConvert m @ Module { modGlobals = globals } =
    m { modGlobals = fmap ssaConvertGlobal globals }

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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts, UndecidableInstances #-}

module IR.Common.Body(
       Body(..),
       Block(..),
       Stm(..),
       Phi(..),
       Bind(..),
       SSAElems,
       StmElems
       ) where

import Data.Graph.Inductive.Graph hiding (out)
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.List
import Data.Position.DWARFPosition(DWARFPosition)
import IR.Common.LValue
import IR.Common.Names
import IR.Common.Transfer
import Prelude.Extras
import Text.Format hiding (concat)
import Text.XML.Expat.Pickle hiding (Node)
import Text.XML.Expat.Tree(NodeG)

import qualified Data.HashMap.Strict as HashMap

-- | A statement.  Represents an effectful action for the statement
-- form of the IR.
data Stm exp =
  -- | Update the given lvalue
    Move {
      -- | The destination LValue.
      moveDst :: (LValue exp),
      -- | The source expression.
      moveSrc :: exp,
      -- | The position in source from which this originates.
      movePos :: !(DWARFPosition Globalname Typename)
    }
  -- | Execute an expression
  | Do { doExp :: !exp }

-- | A binding.  Represents an SSA binding in the SSA form of the
-- language.
data Phi =
  -- | A Phi-node.
  Phi {
    -- | The name being bound.
    phiName :: !Id,
    -- | A map from inbound edges to values
    phiVals :: !(HashMap Label Id),
    -- | The position in source from which this arises.
    phiPos :: !(DWARFPosition Globalname Typename)
  }

-- | A binding.  Represents an SSA binding in the SSA form of the
-- language.
data Bind exp =
    Bind {
      -- | The name being bound.
      bindName :: !Id,
      -- | The value beind bound.
      bindVal :: exp,
      -- | The position in source from which this originates.
      bindPos :: !(DWARFPosition Globalname Typename)
    }
    -- | Execute an expression for its effect only.  Analogous to Do
    -- in the statement language.
  | Effect !exp

-- | A basic block
data Block exp elems =
    Block {
      -- | The statements in the basic block
      blockBody :: elems,
      -- | The transfer for the basic block
      blockXfer :: Transfer exp,
      -- | The position in source from which this arises.
      blockPos :: !(DWARFPosition Globalname Typename)
    }

-- There is no straightforward ordering, equality, or hashing on Body.

-- | The body of a function
data Body exp elems gr =
    Body {
      -- | The entry block
      bodyEntry :: !Label,
      -- | The CFG
      bodyCFG :: gr (Block exp elems) ()
    }

type SSAElems exp = ([Phi], [Bind exp])
type StmElems exp = [Stm exp]

instance Eq1 Stm where
  Move { moveSrc = src1, moveDst = dst1 } ==#
    Move { moveSrc = src2, moveDst = dst2 } = src1 == src2 && dst1 == dst2
  (Do exp1) ==# (Do exp2) = exp1 == exp2
  _ ==# _ = False

instance Eq Phi where
  Phi { phiName = name1, phiVals = vals1 } ==
    Phi { phiName = name2, phiVals = vals2 } =
    name1 == name2 && vals1 == vals2

instance Eq1 Bind where
  Bind { bindName = name1, bindVal = val1 } ==#
    Bind { bindName = name2, bindVal = val2 } =
    name1 == name2 && val1 == val2
  (Effect e1) ==# (Effect e2) = e1 == e2
  _ ==# _ = False

instance Eq2 Block where
  Block { blockBody = body1, blockXfer = xfer1 } ==##
    Block { blockBody = body2, blockXfer = xfer2 } =
    xfer1 == xfer2 && (body1 == body2)

instance Eq exp => Eq (Bind exp) where (==) = (==#)
instance Eq exp => Eq (Stm exp) where (==) = (==#)
instance (Eq elems, Eq exp) => Eq (Block elems exp) where (==) = (==##)

instance Ord1 Stm where
  compare1 Move { moveSrc = src1, moveDst = dst1 }
          Move { moveSrc = src2, moveDst = dst2 } =
    case compare src1 src2 of
      EQ -> compare dst1 dst2
      out -> out
  compare1 Move {} _ = LT
  compare1 _ Move {} = GT
  compare1 (Do exp1) (Do exp2) = compare exp1 exp2

instance Ord Phi where
  compare Phi { phiName = name1, phiVals = vals1 }
          Phi { phiName = name2, phiVals = vals2 } =
    let
      sortedvals1 = sort (HashMap.toList vals1)
      sortedvals2 = sort (HashMap.toList vals2)
    in case compare name1 name2 of
      EQ -> compare sortedvals1 sortedvals2
      out -> out

instance Ord1 Bind where
  compare1 Bind { bindName = name1, bindVal = val1 }
          Bind { bindName = name2, bindVal = val2 } =
    case compare name1 name2 of
      EQ -> compare val1 val2
      out -> out
  compare1 Bind {} _ = LT
  compare1 _ Bind {} = GT
  compare1 (Effect e1) (Effect e2) = compare e1 e2

instance Ord2 Block where
  compare2 Block { blockBody = body1, blockXfer = xfer1 }
           Block { blockBody = body2, blockXfer = xfer2 } =
    case compare xfer1 xfer2 of
      EQ -> compare body1 body2
      out -> out

instance Ord exp => Ord (Bind exp) where compare = compare1
instance Ord exp => Ord (Stm exp) where compare = compare1
instance (Ord exp, Ord elem) => Ord (Block elem exp) where compare = compare2

instance Hashable exp => Hashable (Stm exp) where
  hashWithSalt s Move { moveSrc = src, moveDst = dst } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` src `hashWithSalt` dst
  hashWithSalt s (Do val) = s `hashWithSalt` (2 :: Int) `hashWithSalt` val

instance Hashable Phi where
  hashWithSalt s Phi { phiName = name, phiVals = vals } =
    let
      sortedvals = sort (HashMap.toList vals)
    in
      s `hashWithSalt` name `hashWithSalt` sortedvals

instance Hashable exp => Hashable (Bind exp) where
  hashWithSalt s Bind { bindName = name1, bindVal = val1 } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` name1 `hashWithSalt` val1
  hashWithSalt s (Effect e1) =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` e1

instance (Hashable exp, Hashable elem) => Hashable (Block elem exp) where
  hashWithSalt s Block { blockBody = body, blockXfer = xfer } =
    s `hashWithSalt` xfer `hashWithSalt` body

instance Format exp => Format (Stm exp) where
  format Move { moveDst = dst, moveSrc = src } =
    format dst <+> string "<-" <+> format src
  format (Do e) = format e

instance Format Phi where
  format Phi { phiName = name, phiVals = vals } =
    let
      mapfun (l, v) = format l <> colon <+> format v
      choicedocs = map mapfun (HashMap.toList vals)
    in
      format name <+> string "<- phi" <+> list choicedocs

instance Format exp => Format (Bind exp) where
  format Bind { bindName = dst, bindVal = src } =
    format dst <+> string "<-" <+> format src
  format (Effect e) = format e

instance Format exp => Format (Block exp ([Phi], [Bind exp])) where
  format Block { blockBody = (phis, binds), blockXfer = xfer } =
    vcat (map format phis ++ map format binds ++ [format xfer])

instance Format exp => Format (Block exp [Stm exp]) where
  format Block { blockBody = stms, blockXfer = xfer } =
    vcat (map format stms ++ [format xfer])

instance FormatM m exp => FormatM m (Stm exp) where
  formatM Move { moveDst = dst, moveSrc = src } =
    do
      dstdoc <- formatM dst
      srcdoc <- formatM src
      return $! dstdoc <+> string "<-" <+> srcdoc
  formatM (Do e) = formatM e

instance FormatM m exp => FormatM m (Bind exp) where
  formatM Bind { bindName = dst, bindVal = src } =
    do
      srcdoc <- formatM src
      return $! format dst <+> string "<-" <+> srcdoc
  formatM (Effect e) = formatM e

instance FormatM m exp => FormatM m (Block exp ([Phi], [Bind exp])) where
  formatM Block { blockBody = (phis, binds), blockXfer = xfer } =
    do
      binddocs <- mapM formatM binds
      xferdoc <- formatM xfer
      return $! vcat (map format phis ++ binddocs ++ [xferdoc])

instance FormatM m exp => FormatM m (Block exp [Stm exp]) where
  formatM Block { blockBody = stms, blockXfer = xfer } =
    do
      stmdocs <- mapM formatM stms
      xferdoc <- formatM xfer
      return $! vcat (stmdocs ++ [xferdoc])

movePickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler [NodeG [] tag text] exp) =>
              PU [NodeG [] tag text] (Stm exp)
movePickler =
  let
    revfunc Move { moveDst = dst, moveSrc = src, movePos = pos } =
      (src, dst, pos)
    revfunc _ = error "can't convert"
  in
    xpWrap (\(src, dst, pos) -> Move { moveSrc = src, moveDst = dst,
                                       movePos = pos }, revfunc)
           (xpElemNodes (gxFromString "Move")
                        (xpTriple (xpElemNodes (gxFromString "src") xpickle)
                                  (xpElemNodes (gxFromString "dst") xpickle)
                                  (xpElemNodes (gxFromString "pos") xpickle)))

doPickler :: (GenericXMLString tag, Show tag,
              GenericXMLString text, Show text,
              XmlPickler [NodeG [] tag text] exp) =>
             PU [NodeG [] tag text] (Stm exp)
doPickler =
  let
    revfunc (Do e) = e
    revfunc _ = error "can't convert"
  in
    xpWrap (Do, revfunc) (xpElemNodes (gxFromString "Do") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] exp) =>
         XmlPickler [NodeG [] tag text] (Stm exp) where
  xpickle =
    let
      picker Move {} = 0
      picker (Do _) = 1
    in
      xpAlt picker [movePickler, doPickler]

bindPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               XmlPickler [NodeG [] tag text] exp) =>
              PU [NodeG [] tag text] (Bind exp)
bindPickler =
  let
    revfunc Bind { bindName = name, bindVal = val, bindPos = pos } =
      (name, (val, pos))
    revfunc _ = error "can't convert"
  in
    xpWrap (\(name, (val, pos)) -> Bind { bindName = name, bindVal = val,
                                          bindPos = pos }, revfunc)
           (xpElem (gxFromString "Bind") xpickle
                   (xpPair (xpElemNodes (gxFromString "val") xpickle)
                           (xpElemNodes (gxFromString "pos") xpickle)))

effectPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text,
                  XmlPickler [NodeG [] tag text] exp) =>
                 PU [NodeG [] tag text] (Bind exp)
effectPickler =
  let
    revfunc (Effect e) = e
    revfunc _ = error "can't convert"
  in
    xpWrap (Effect, revfunc) (xpElemNodes (gxFromString "Effect") xpickle)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text,
          XmlPickler [NodeG [] tag text] exp) =>
         XmlPickler [NodeG [] tag text] (Bind exp) where
  xpickle =
    let
      picker Bind {} = 0
      picker (Effect _) = 1
    in
      xpAlt picker [bindPickler, effectPickler]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Phi where
  xpickle =
    let
      optionPickler = (xpList (xpElemAttrs (gxFromString "val")
                                           (xpPair xpickle xpickle)))
    in
      xpWrap (\(name, (vals, pos)) -> Phi { phiName = name, phiPos = pos,
                                            phiVals = HashMap.fromList vals },
              \Phi { phiName = name, phiPos = pos, phiVals = vals } ->
                (name, (HashMap.toList vals, pos)))
             (xpElem (gxFromString "Phi") xpickle
                     (xpPair (xpElemNodes (gxFromString "vals") optionPickler)
                             (xpElemNodes (gxFromString "pos") xpickle)))

phiBlockPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text,
                    XmlPickler [NodeG [] tag text] exp) =>
                   PU [NodeG [] tag text] (Label, Block exp ([Phi], [Bind exp]))
phiBlockPickler =
  let
    revfunc (l, Block { blockBody = (phis, binds), blockXfer = xfer,
                        blockPos = pos }) =
      (l, (phis, binds, xfer, pos))
  in
    xpWrap (\(l, (phis, binds, xfer, pos)) ->
             (l, Block { blockBody = (phis, binds), blockXfer = xfer,
                         blockPos = pos }), revfunc)
           (xpElem (gxFromString "Block") xpickle
                   (xp4Tuple (xpElemNodes (gxFromString "phis")
                                          (xpList xpickle))
                             (xpElemNodes (gxFromString "binds")
                                          (xpList xpickle))
                             (xpElemNodes (gxFromString "xfer") xpickle)
                             (xpElemNodes (gxFromString "pos") xpickle)))

stmBlockPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text,
                    XmlPickler [NodeG [] tag text] exp) =>
                   PU [NodeG [] tag text] (Label, Block exp [Stm exp])
stmBlockPickler =
  let
    revfunc (l, Block { blockBody = stms, blockXfer = xfer,
                        blockPos = pos }) =
      (l, (stms, xfer, pos))
  in
    xpWrap (\(l, (stms, xfer, pos)) ->
             (l, Block { blockBody = stms, blockXfer = xfer,
                         blockPos = pos }), revfunc)
           (xpElem (gxFromString "Block") xpickle
                   (xpTriple (xpElemNodes (gxFromString "stms")
                                          (xpList xpickle))
                             (xpElemNodes (gxFromString "xfer") xpickle)
                             (xpElemNodes (gxFromString "pos") xpickle)))


bodyPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text,
                Graph gr, XmlPickler [NodeG [] tag text] exp) =>
               PU [NodeG [] tag text] (Label, Block exp elems) ->
               PU [NodeG [] tag text] (Body exp elems gr)
bodyPickler elemsPickler =
  let
    buildCFG :: Graph gr =>
                [(Label, Block exp elems)] -> gr (Block exp elems) ()
    buildCFG blocks =
      let
        getEdges :: (Label, Block exp elems) -> [(Node, Node, ())]
        getEdges (src, Block { blockXfer = Goto { gotoLabel = dst } }) =
          [(fromEnum src, fromEnum dst, ())]
        getEdges (src, Block { blockXfer = Case { caseDefault = def,
                                                  caseCases = cases } }) =
          (fromEnum src, fromEnum def, ()) :
          map (\(_, dst) -> (fromEnum src, fromEnum dst, ())) cases
        getEdges (_, Block { blockXfer = Ret {} }) = []
        getEdges (_, Block { blockXfer = Unreachable _ }) = []
      in
        mkGraph (map (\(l, b) -> (fromEnum l, b)) blocks)
                (concatMap getEdges blocks)

    extractNode (l, b) = (toEnum l, b)
  in
    xpWrap (\(l, blocks) -> Body { bodyEntry = l, bodyCFG = buildCFG blocks },
            \Body { bodyEntry = l, bodyCFG = cfg } ->
              (l, map extractNode (labNodes cfg)))
           (xpElem (gxFromString "Body") xpickle
                   (xpElemNodes (gxFromString "blocks")
                                (xpList elemsPickler)))

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text,
          Graph gr, XmlPickler [NodeG [] tag text] exp) =>
         XmlPickler [NodeG [] tag text] (Body exp ([Phi], [Bind exp]) gr) where
  xpickle = bodyPickler phiBlockPickler

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text,
          Graph gr, XmlPickler [NodeG [] tag text] exp) =>
         XmlPickler [NodeG [] tag text] (Body exp [Stm exp] gr) where
  xpickle = bodyPickler stmBlockPickler

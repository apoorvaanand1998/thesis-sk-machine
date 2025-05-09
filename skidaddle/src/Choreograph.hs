module Choreograph where

import WAT
import Identifiers
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe )

data Comb  = PrimComb String | PrimOp String | CRef Identifier | CRec GraphInstr
data Val   = PrimVal Int | VRef Identifier | VRec GraphInstr
data Field = LeftF | RightF

data GraphInstr = MkNode Comb Val
                | Ancestor Int
                | Store Field Identifier
                | NodeSet Field
                | Check String [MixedInstr]              

-- x = MkNode (CRec (MkNode (CRef "$x") (VRec (MkNode (CRef "$y") (VRef "$z")))))
-- an example of how CRec and CRef are used

-- stupid hack 
-- because I would like to insert instructions in between graph instructions
-- sometimes... like with lasModify and modAnc
data MixedInstr = GI GraphInstr | WI Instr  

toInstr :: [MixedInstr] -> [Instr]
toInstr []     = []
toInstr (x:xs) = case x of
    GI gi -> toWatInstr gi ++ toInstr xs
    WI wi -> wi : toInstr xs

toWatInstr :: GraphInstr -> [Instr]
toWatInstr (Ancestor i) = ancestor i
toWatInstr (Store f s)  = store f s
toWatInstr (NodeSet f)  = nodeSet f
toWatInstr (Check s gs) = check s gs
toWatInstr (MkNode c v) = fromComb c ++ fromVal v ++ [I32Const 42, StructNew AppNodeType]
-- 42 is the default name

-- based on the enum order of node_tag in the eval.c file
combs :: [String]
combs = [ "S", "K", "I", "B", "C",
          "A", "Y", "SS", "BB", "CC", "P", "R", "O", "U", "Z",
          "K2", "K3", "K4", "CCB",
          "ADD", "SUB", "MUL", "QUOT", "REM", "SUBR", "UQUOT", "UREM", "NEG",
          "AND", "OR", "XOR", "INV", "SHL", "SHR", "ASHR",
          "EQ", "NE", "LT", "LE", "GT", "GE", "ULT", "ULE", "UGT", "UGE", "ICMP", "UCMP" ]

fromComb :: Comb -> [Instr]
fromComb (PrimComb x) = [prim x]
fromComb (PrimOp x)   = [prim x]
fromComb (CRef s)     = [LocalGet s]
fromComb (CRec g)     = toWatInstr g

fromVal :: Val -> [Instr]
fromVal (PrimVal i) = [RefI31 i]
fromVal (VRef s)    = [LocalGet s]
fromVal (VRec g)    = toWatInstr g

prim :: String -> Instr
prim x = maybe (error "Unknown PrimComb or PrimOp") RefI31 (elemIndex x combs)

-- stores non-reduced fields in variables
store :: Field -> Identifier -> [Instr]
store anf varIden = [ StructGet AppNodeType fn
                    , LocalSet varIden ]
    where
        fn = fieldName anf

nodeSet :: Field -> [Instr]
nodeSet anf = [StructSet AppNodeType (fieldName anf)]

fieldName :: Field -> Identifier
fieldName LeftF  = LeftField
fieldName RightF = RightField

-- gets the ith ancestor of the top of the LAS
ancestor :: Int -> [Instr]
ancestor i =
        [ LocalGet LASType
        , LocalGet LasIdx ]                           ++
        (if i == 0 then [] else [I32Const i, I32Sub]) ++
        [ ArrayGet StackType ]

---
-- step function scaffolding and helpers
---

check :: String -> [MixedInstr] -> [Instr]
check i gs = [ LocalGet LocalCombIdx
             , I32Const (fromMaybe (error e) (elemIndex i combs))
             , I32Eq
             , If (toInstr gs ++ [end]) ]
    where
        e   = "PrimComb or PrimOp not recognized during code gen"
        end = Br CombCase

-- precondition : i >= 0 and i <= 5
stores :: Int -> [GraphInstr]
stores i =
    let
        vars = map MV [P, Q, R, S, T]
        f x  = [Ancestor x, Store RightF (vars !! x)]
    in
        concatMap f [0..i-1]

modAnc :: Int -> GraphInstr -> GraphInstr -> [MixedInstr]
modAnc i leftNode rightNode = [ GI (Ancestor i), GI leftNode, GI (NodeSet LeftF)
                              , GI leftNode, WI (LocalSet TempVar)
                              , GI (Ancestor i), GI rightNode, GI (NodeSet RightF) ]
                              ++
                              map WI (lasModify i leftNode)

lasModify :: Int -> GraphInstr -> [Instr]
lasModify n g@(MkNode _ _) = concatMap (\x -> las x ++ arraySetVal ++ checkAndLeft) is ++ nextIdx
    where
        nextIdx = [LocalGet LasIdx, I32Const n, I32Sub, I32Const (leftSpineLen g), I32Add, LocalSet ReturnVar]

        is = [1..leftSpineLen g] -- you got from (lasLength - redRuleN + 1) to (lasLength - redRuleN + lefSpineLenOfNewNode)

        las :: Int -> [Instr]
        las i = [LocalGet LASType, LocalGet LasIdx, I32Const n, I32Sub, I32Const i, I32Add] -- hardcoded for now

        -- the assumption of the below functions is that there exists a variable 
        -- called temp, that stores the value of ln in it
        arraySetVal :: [Instr]
        arraySetVal = [LocalGet TempVar, ArraySet StackType]

        checkAndLeft :: [Instr]
        checkAndLeft = [LocalGet TempVar, StructGet AppNodeType LeftField, RefTest AppNodeType,
                        If [LocalGet TempVar, StructGet AppNodeType LeftField, RefCast AppNodeType, LocalSet TempVar]]

lasModify _ _             = error "lasModify should only be getting MkNode"

leftSpineLen :: GraphInstr -> Int
leftSpineLen (MkNode (CRec g) _) = 1 + leftSpineLen g
leftSpineLen (MkNode  _       _) = 1
leftSpineLen  _                  = error "leftSpineLen can only be measured for MkNode"

redRuleS :: [MixedInstr]
redRuleS = map GI (stores n) ++ modAnc (n-1) ln rn
    where
        ln = MkNode (CRef (MV P)) (VRef (MV R))
        rn = MkNode (CRef (MV Q)) (VRef (MV R))
        n  = 3
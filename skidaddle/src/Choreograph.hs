module Choreograph where

import WAT
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe )

data Comb  = PrimComb String | PrimOp String | CRef String | CRec GraphInstr
data Val   = PrimVal Int | VRef String | VRec GraphInstr
data Field = LeftF | RightF

data GraphInstr = MkNode Comb Val
                | Ancestor Int
                | Store Field String
                | NodeSet Field
                | Check String [GraphInstr]

-- x = MkNode (CRec (MkNode (CRef "$x") (VRec (MkNode (CRef "$y") (VRef "$z")))))
-- an example of how CRec and CRef are used

toWatInstr :: GraphInstr -> [Instr]
toWatInstr (Ancestor i) = ancestor i
toWatInstr (Store f s)  = store f s
toWatInstr (NodeSet f)  = nodeSet f
toWatInstr (Check s gs) = check s gs
toWatInstr (MkNode c v) = fromComb c ++ fromVal v ++ [I32Const 42, StructNew "$appNode"]
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
store :: Field -> String -> [Instr]
store anf varIden = [ StructGet "$appNode" fn
                    , LocalSet varIden ]
    where
        fn = fieldName anf

nodeSet :: Field -> [Instr]
nodeSet anf = [StructSet "$appNode" (fieldName anf)]

fieldName :: Field -> String
fieldName LeftF  = "$left"
fieldName RightF = "$right"

-- gets the ith ancestor of the top of the LAS
ancestor :: Int -> [Instr]
ancestor i =
    let
        las       = "$las"
        lasIdx    = "$n"
        stackType = "$stack"
    in
        [ LocalGet las
        , LocalGet lasIdx ]                         ++
        if i == 0 then [] else [I32Const i, I32Sub] ++
        [ ArrayGet stackType ]

---
-- Step Functions and helpers
---

check :: String -> [GraphInstr] -> [Instr]
check i gs = [ LocalGet "$ascii"
             , I32Const (fromMaybe (error e) (elemIndex i combs))
             , I32Eq
             , If (concatMap toWatInstr gs ++ lasModify gs ++ [end]) ]
    where
        e   = "PrimComb or PrimOp not recognized during code gen"
        end = Br "combCase"

-- precondition : i >= 0 and i <= 5
stores :: Int  -> [GraphInstr]
stores i =
    let
        vars = ["$p", "$q", "$r", "$s", "$t"]
        f x  = [Ancestor x, Store RightF (vars !! x)]
    in
        concatMap f [0..i]

modAnc :: Int -> GraphInstr -> GraphInstr -> [GraphInstr]
modAnc i leftNode rightNode = [ Ancestor i, leftNode, NodeSet LeftF
                              , Ancestor i, rightNode, NodeSet RightF ]

lasModify :: GraphInstr -> [Instr]
lasModify g@(MkNode _ _) = undefined
    where
        n  = leftSpineLen g
        ns = [1..n]

        las :: Int -> [Instr]
        las i = [LocalGet "$las", LocalGet "$n", I32Const i, I32Sub] -- hardcoded for now

        -- the assumption of the below function is that there exists a variable 
        -- called temp, that stores the value of ln in it
        arraySetVal :: [Instr]
        arraySetVal = [LocalGet "$temp", ArraySet "$stack"]

        checkAndLeft :: [Instr]
        checkAndLeft = [LocalGet "$temp", StructGet "$appNode" "$left", RefTest "$appNode",
                        If [LocalGet "$temp", StructGet "$appNode" "$left", RefCast "$appNode", LocalSet "$temp"]]

lasModify    _           = error "lasModify should only be getting MkNode"

leftSpineLen :: GraphInstr -> Int
leftSpineLen (MkNode (CRec g) _) = 1 + leftSpineLen g
leftSpineLen (MkNode  _       _) = 1
leftSpineLen  _                  = error "leftSpineLen can only be measured for MkNode"

redRuleS :: [GraphInstr]
redRuleS =   stores n ++ modAnc (n-1) ln rn
    where
        ln = MkNode (CRef "$p") (VRef "$r")
        rn = MkNode (CRef "$q") (VRef "$r")
        n  = 3

-- make a datatype for variables to remove implicit variable name use
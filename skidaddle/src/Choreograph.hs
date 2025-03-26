module Choreograph where

import WAT

data Comb     = PrimComb Char | PrimOp Char | CRef String
data Val      = PrimVal Int | VRef String
data Field    = LeftF | RightF 

data GraphInstr = MkGraph Comb Val
                | TopStack
                | Ancestor Int
                | StoRed Field String
                | Store Field String

toWatInstr :: GraphInstr -> [Instr]
toWatInstr TopStack     = topLAS
toWatInstr (Ancestor i) = ancestor i
toWatInstr (StoRed f s) = stoRed f s
toWatInstr (Store f s)  = store f s

-- assumed appNode is on top of the stack
-- value to be stored is fully reduced i.e., it is an i31 ref
-- left represents comb/prim and right a constant
stoRed :: Field -> String -> [Instr]
stoRed anf varIden = [ StructGet "appNode" fn
                     , RefCastI31
                     , I31Get
                     , LocalSet varIden ]
    where
        fn = fieldName anf

-- stores non-reduced fields in variables
store :: Field -> String -> [Instr]
store anf varIden = [ StructGet "appNode" fn
                    , LocalSet varIden ]
    where
        fn = fieldName anf

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

-- leaves an appNode on top of the WASM stack
topLAS :: [Instr]
topLAS = ancestor 0
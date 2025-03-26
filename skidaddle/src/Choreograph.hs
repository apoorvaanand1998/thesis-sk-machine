module Choreograph where

import WAT

data AppNodeField = LeftANF | RightANF deriving (Show, Eq)

data WatGraph = WatGraph Comb Val
data Comb     = PrimComb | PrimOp | CRef String
data Val      = PrimVal  | VRef String

data GraphInstr = MkNode Comb Val 

-- assumed appNode is on top of the stack
-- value to be stored is fully reduced i.e., it is an i31 ref
-- left represents comb/prim and right a constant
stoRed :: AppNodeField -> String -> [Instr]
stoRed anf varIden = [ StructGet "appNode" fn
                     , RefCastI31
                     , I31Get
                     , LocalSet varIden ]
    where
        fn = fieldName anf

-- stores non-reduced fields in variables
store :: AppNodeField -> String -> [Instr]
store anf varIden = [ StructGet "appNode" fn
                    , LocalSet varIden ]
    where
        fn = fieldName anf

fieldName :: AppNodeField -> String
fieldName LeftANF  = "$left"
fieldName RightANF = "$right"

-- gets the ith ancestor of the top of the LAS
getAncestor :: Int -> [Instr]
getAncestor i =
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
topLAS = getAncestor 0
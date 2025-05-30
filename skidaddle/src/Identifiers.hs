{-# LANGUAGE InstanceSigs #-}
module Identifiers where

data MetaVars = X | Y | Z | W | V deriving Enum

data Identifier = LASVar | LasIdx | StackType | AppNodeType 
                | LeftField | RightField | LocalCombIdx -- currently "$ascii", but that doesn't fit anymore
                | CombCase | TempVar | ReturnVar
                | MV MetaVars | FnReduce

instance Show MetaVars where
    show :: MetaVars -> String
    show X = "$x"
    show Y = "$y"
    show Z = "$z"
    show W = "$w"
    show V = "$v"

instance Show Identifier where
    show :: Identifier -> String
    show LASVar       = "$las"
    show LasIdx       = "$n"
    show StackType    = "$stack"
    show AppNodeType  = "$appNode"
    show LeftField    = "$left"
    show RightField   = "$right"
    show LocalCombIdx = "$ascii"
    show CombCase     = "$combCase"
    show TempVar      = "$temp"
    show ReturnVar    = "$r"
    show (MV mv)      = show mv
    show FnReduce     = "$i31OrReduce"
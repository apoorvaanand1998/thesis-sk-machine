{-# LANGUAGE InstanceSigs #-}
module Identifiers where

data MetaVars = P | Q | R | S | T deriving Enum

data Identifier = LASType | LasIdx | StackType | AppNodeType 
                | LeftField | RightField | LocalCombIdx -- currently "$ascii", but that doesn't fit anymore
                | CombCase | TempVar | ReturnVar
                | MV MetaVars

instance Show MetaVars where
    show :: MetaVars -> String
    show P = "$p"
    show Q = "$q"
    show R = "$r"
    show S = "$s"
    show T = "$t"

instance Show Identifier where
    show :: Identifier -> String
    show LASType      = "$las"
    show LasIdx       = "$n"
    show StackType    = "$stack"
    show AppNodeType  = "$appNode"
    show LeftField    = "left"
    show RightField   = "$right"
    show LocalCombIdx = "$ascii"
    show CombCase     = "$combCase"
    show TempVar      = "$temp"
    show ReturnVar    = "$r"
    show (MV mv)      = show mv
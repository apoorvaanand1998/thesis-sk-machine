module WAT where

import Data.List ( intercalate )
import Identifiers

data Instr = I32Const Int
           | I32Eq
           | I32Sub
           | I32Add
           | LocalGet Identifier
           | LocalSet Identifier
           | LocalTee Identifier
           | ArrayGet Identifier
           | ArraySet Identifier
           | StructNew Identifier
           | StructGet Identifier Identifier
           | StructSet Identifier Identifier
           | RefI31 Int
           | RefI31' -- called by itself without supplying an arg
           | RefCastI31
           | RefCast Identifier
           | RefTest Identifier
           | I31Get
           | If [Instr]
           | Br Identifier
           | Call Identifier
           | Nop
           | Comment String
           deriving Show

toWat :: Instr -> String
toWat (I32Const n)    = "(i32.const " ++ show n ++ ")"
toWat I32Eq           = "(i32.eq)"
toWat I32Sub          = "(i32.sub)"
toWat I32Add          = "(i32.add)"
toWat (LocalGet i)    = "(local.get " ++ show i ++ ")"
toWat (LocalSet i)    = "(local.set " ++ show i ++ ")"
toWat (LocalTee i)    = "(local.tee " ++ show i ++ ")"
toWat (ArrayGet i)    = "(array.get " ++ show i ++ ")"
toWat (ArraySet i)    = "(array.set " ++ show i ++ ")"
toWat (StructNew i)   = "(struct.new " ++ show i ++ ")"
toWat (StructGet t f) = "(struct.get " ++ show t ++ " " ++ show f ++ ")"
toWat (StructSet t f) = "(struct.set " ++ show t ++ " " ++ show f ++ ")"
toWat (RefI31 i)      = "(i32.const " ++ show i ++ ")" ++ "(ref.i31)"
toWat RefI31'         = "(ref.i31)"
toWat RefCastI31      = "(ref.cast i31ref)"
toWat (RefCast i)     = "(ref.cast (ref null " ++ show i ++ "))"
toWat (RefTest i)     = "(ref.test (ref null " ++ show i ++ "))"
toWat I31Get          = "(i31.get_s)"
toWat (If is)         = "(if (then\n" ++ emit is ++ "))"
toWat (Br i)          = "(br " ++ show i ++ ")"
toWat (Call i)        = "(call " ++ show i ++ ")"
toWat Nop             = "(nop)"
toWat (Comment s)     = ";; " ++ s

emit :: [Instr] -> String
emit is = intercalate "\n" $ map toWat is
module WAT where
import Data.List (intercalate)

type Ident = String

data Instr = I32Const Int
           | I32Sub
           | LocalGet Ident
           | LocalSet Ident
           | ArrayGet Ident
           | StructNew Ident
           | StructGet Ident Ident
           | RefI31 Int
           | RefCastI31
           | I31Get
           | Nop
           deriving Show

toWat :: Instr -> String
toWat (I32Const n)    = "(i32.const " ++ show n ++ ")"
toWat I32Sub          = "(i32.sub)"
toWat (LocalGet i)    = "(local.get " ++ show i ++ ")"
toWat (LocalSet i)    = "(local.set " ++ show i ++ ")"
toWat (ArrayGet i)    = "(array.get " ++ show i ++ ")"
toWat (StructNew i)   = "(struct.new" ++ show i ++ ")"
toWat (StructGet t f) = "(struct.get " ++ show t ++ " " ++ show f ++ ")"
toWat (RefI31 i)      = "(i32.const " ++ show i ++ ")" ++ "(ref.i31)"
toWat RefCastI31      = "(ref.cast i31ref)"
toWat I31Get          = "(i31.get_s)"
toWat Nop             = "(nop)"

emit :: [Instr] -> String
emit is = intercalate "\n" $ map toWat is
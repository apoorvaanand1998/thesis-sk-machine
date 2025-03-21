module WAT where
import Data.List (intercalate)

type Ident = String

data Instr = I32Const Int
           | I32Sub
           | LocalGet Ident
           | LocalSet Ident
           | ArrayGet Ident
           | StructGet Ident Ident
           | Nop
           deriving Show

toWat :: Instr -> String
toWat (I32Const n)    = "(i32.const " ++ show n ++ ")"
toWat I32Sub          = "(i32.sub)"
toWat (LocalGet i)    = "(local.get " ++ show i ++ ")"
toWat (LocalSet i)    = "(local.set " ++ show i ++ ")"
toWat (ArrayGet i)    = "(array.get " ++ show i ++ ")"
toWat (StructGet t f) = "(struct.get " ++ show t ++ " " ++ show f ++ ")"
toWat Nop             = "(nop)"

emit :: [Instr] -> String
emit is = intercalate "\n" $ map toWat is
module WAT where
import Data.List ( intercalate )

type Ident = String

data Instr = I32Const Int
           | I32Eq
           | I32Sub
           | I32Add
           | LocalGet Ident
           | LocalSet Ident
           | ArrayGet Ident
           | ArraySet Ident
           | StructNew Ident
           | StructGet Ident Ident
           | StructSet Ident Ident
           | RefI31 Int
           | RefCastI31
           | RefCast Ident
           | RefTest Ident
           | I31Get
           | If [Instr]
           | Br Ident
           | Nop
           deriving Show

toWat :: Instr -> String
toWat (I32Const n)    = "(i32.const " ++ show n ++ ")"
toWat I32Eq           = "(i32.eq)"
toWat I32Sub          = "(i32.sub)"
toWat I32Add          = "(i32.add)"
toWat (LocalGet i)    = "(local.get " ++ i ++ ")"
toWat (LocalSet i)    = "(local.set " ++ i ++ ")"
toWat (ArrayGet i)    = "(array.get " ++ i ++ ")"
toWat (ArraySet i)    = "(array.set " ++ i ++ ")"
toWat (StructNew i)   = "(struct.new" ++ i ++ ")"
toWat (StructGet t f) = "(struct.get " ++ t ++ " " ++ f ++ ")"
toWat (StructSet t f) = "(struct.set " ++ t ++ " " ++ f ++ ")"
toWat (RefI31 i)      = "(i32.const " ++ show i ++ ")" ++ "(ref.i31)"
toWat RefCastI31      = "(ref.cast i31ref)"
toWat (RefCast i)     = "(ref.cast (ref null " ++ i ++ "))"
toWat (RefTest i)     = "(ref.test (ref null " ++ i ++ "))"
toWat I31Get          = "(i31.get_s)"
toWat (If is)         = "(if (then\n" ++ emit is ++ "))"
toWat (Br i)          = "(br " ++ i ++ ")"
toWat Nop             = "(nop)"

emit :: [Instr] -> String
emit is = intercalate "\n" $ map toWat is
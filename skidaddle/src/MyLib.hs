module MyLib where
import WAT

-- precondition - n is a non-negative integer
loadArgs :: Int -> [Instr]
loadArgs n = concatMap loadArg [0..n-1]

-- precondition - n is a non-negative integer
loadArg :: Int -> [Instr]
loadArg n = 
    let
        las          = "$las"
        lasIdx       = "$n"
        stackType    = "$stack"
        appNodeType  = "$appNode"
        argFieldName = "$right"
        vars         = ["$p", "$q", "$r", "$s", "$t"]
        
        nVars :: Int -> [Instr]
        nVars 0 = [Nop]
        nVars x = [I32Const x, I32Sub] 

        nthVar :: Int -> Instr
        nthVar x = LocalSet (vars !! x)
    in  
        [LocalGet las, LocalGet lasIdx]                          ++ 
         nVars n                                                 ++
        [ArrayGet stackType, StructGet appNodeType argFieldName] ++
        [nthVar n]
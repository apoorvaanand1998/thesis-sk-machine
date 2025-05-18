module ReductionRules where

import Choreograph
import Identifiers
import WAT

redRules :: [[MixedInstr]]
redRules = [ redRuleS, redRuleK, redRuleI, redRuleB, redRuleC,
              redRuleA, redRuleY, redRuleS', redRuleB', redRuleC',
              redRuleP, redRuleR, redRuleO, redRuleU, redRuleZ,
              redRuleK2, redRuleK3, redRuleK4, redRuleC'B ]
              
redRule :: Int -> MixedInstr -> MixedInstr -> [MixedInstr]
redRule n ln rn = map GI (stores n) ++ modifyAncestor (n-1) ln rn

redRuleS :: [MixedInstr]
redRuleS = redRule n ln rn
    where
        n  = 3
        ln = GI (MkNode (CRef (MV X)) (VRef (MV Z)))
        rn = GI (MkNode (CRef (MV Y)) (VRef (MV Z)))

redRuleK :: [MixedInstr]
redRuleK = redRule n ln rn
    where
        n  = 2
        ln = WI (prim "I")
        rn = WI (LocalGet (MV X))

-- different from sk.wat, but essentially doing the same thing
redRuleI :: [MixedInstr]
redRuleI = redRule n ln rn
    where
        n  = 2
        ln = WI (LocalGet (MV X))
        rn = WI (LocalGet (MV Y)) -- so Y is left as it is
        -- and X is kept on the ancestor above it, on the left

redRuleB :: [MixedInstr]
redRuleB = redRule n ln rn
    where
        n  = 3
        ln = WI (LocalGet (MV X))
        rn = GI (MkNode (CRef (MV Y)) (VRef (MV Z)))

redRuleC :: [MixedInstr]
redRuleC = redRule n ln rn
    where
        n  = 3
        ln = GI (MkNode (CRef (MV X)) (VRef (MV Z)))
        rn = WI (LocalGet (MV Y))

redRuleS' :: [MixedInstr]
redRuleS' = redRule n ln rn
    where
        n  = 4
        ln = GI (MkNode (CRef (MV X))
                        (VRec (MkNode (CRef (MV X))
                                      (VRef (MV Y)))))
        rn = GI (MkNode (CRef (MV Z)) (VRef (MV W)))

redRuleB' :: [MixedInstr]
redRuleB' = redRule n ln rn
    where
        n  = 4
        ln = GI (MkNode (CRef (MV X)) (VRef (MV Y)))
        rn = GI (MkNode (CRef (MV Z)) (VRef (MV W)))

redRuleC' :: [MixedInstr]
redRuleC' = redRule n ln rn
    where
        n  = 4
        ln = GI (MkNode (CRef (MV X)) 
                        (VRec (MkNode (CRef (MV Y))
                                      (VRef (MV W)))))
        rn = WI (LocalGet (MV Z))

redRuleA :: [MixedInstr]
redRuleA = redRule n ln rn
    where
        n  = 2
        ln = WI (prim "I")
        rn = WI (LocalGet (MV Y))

redRuleU :: [MixedInstr]
redRuleU = redRule n ln rn
    where
        n  = 2
        ln = WI (LocalGet (MV Y))
        rn = WI (LocalGet (MV X))

redRuleY :: [MixedInstr]
redRuleY = map GI (stores 1)                                 ++
           [ GI (Ancestor 0), ln, GI (NodeSet LeftF), -- first part of modifyAncestor
             GI (Ancestor 0), WI (LocalSet (MV Y)), -- store the node itself into MV Y
             GI (Ancestor 0), WI (LocalGet (MV Y)), GI (NodeSet RightF), -- Set right to be Y
             -- i.e., last two lines are done for self-reference
             WI (LocalGet LasIdx), WI (LocalSet ReturnVar) ] -- no change in index
    where
        ln = WI (LocalGet (MV X))

-- red indeed does rule
redRuleZ :: [MixedInstr]
redRuleZ = redRule n ln rn
    where
        n  = 3
        ln = WI (LocalGet (MV X))
        rn = WI (LocalGet (MV Y))

redRuleP :: [MixedInstr]
redRuleP = redRule n ln rn
    where
        n  = 3
        ln = GI (MkNode (CRef (MV Z)) (VRef (MV X)))
        rn = WI (LocalGet (MV Y))

redRuleR :: [MixedInstr]
redRuleR = redRule n ln rn
    where
        n  = 3
        ln = GI (MkNode (CRef (MV Y)) (VRef (MV Z)))
        rn = WI (LocalGet (MV X))

redRuleO :: [MixedInstr]
redRuleO = redRule n ln rn
    where
        n  = 3
        ln = GI (MkNode (CRef (MV W)) (VRef (MV X)))
        rn = WI (LocalGet (MV Y))

redRuleK2 :: [MixedInstr]
redRuleK2 = redRule n ln rn
    where
        n  = 3
        ln = WI (prim "I")
        rn = WI (LocalGet (MV X))

redRuleK3 :: [MixedInstr]
redRuleK3 = redRule n ln rn
    where
        n  = 4
        ln = WI (prim "I")
        rn = WI (LocalGet (MV X))

redRuleK4 :: [MixedInstr]
redRuleK4 = redRule n ln rn
    where
        n  = 5
        ln = WI (prim "I")
        rn = WI (LocalGet (MV X))

redRuleC'B :: [MixedInstr]
redRuleC'B = redRule n ln rn
    where
        n  = 4
        ln = GI (MkNode (CRef (MV X)) (VRef (MV Z)))
        rn = GI (MkNode (CRef (MV Y)) (VRef (MV W)))
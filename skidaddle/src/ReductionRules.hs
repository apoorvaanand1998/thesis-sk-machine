module ReductionRules where

import Choreograph
import Identifiers

redRuleS :: [MixedInstr]
redRuleS = map GI (stores n) ++ modAnc (n-1) ln rn
    where
        ln = MkNode (CRef (MV P)) (VRef (MV R))
        rn = MkNode (CRef (MV Q)) (VRef (MV R))
        n  = 3
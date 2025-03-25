{-# LANGUAGE DataKinds #-}

module TypedWAT where

data PWType = PWTInt

data TInstr bef af where
   Const :: Int -> TInstr stack (PWTInt : stack)
   Add :: TInstr (PWTInt : PWTInt : stack) (PWTInt : stack)

data TInstrs bef af where
   Empty :: TInstrs stack stack
   (:>) :: TInstr bef af -> TInstrs af af' -> TInstrs bef af'

infixr 5 :>

x = Const 42 :> Add :> Empty
















{-
Why would you like to do a PhD?

Technical reasons:
    1. I enjoy working with Haskell. Working with abstractions is
       enjoyable. A PhD gives me the opportunity to learn higher abstractions.
    2. Abstractions are wonderful but they don't pain the entire picture.
       To really fully understand what is going on, you've got to understand
       the entire stack, top to bottom. I've enjoyed doing the low level stuff
       (web assembly, to a certain extent) and the assembly project going on with Lawrence
    3. Security aspect. I like to overthink things. Usually not a good thing but
       in security, is actually good. Lots of intersection with the kind of math I enjoy,
       opportunity to learn and grow

Non-technical reasons:
    1. Marco is a great professor. His excitement and enthusiasm rubs off on me.
       His willingness to start explaining things on the whiteboard is just an absolute win.
    2. I like to learn. I would like to learn as many things as possible and a PhD is fundamentally that.
       Because of this, I would be a great PhD candidate. Always trying to improve.
    3. I love the colleagues at Utrecht University that I might have. Generally speaking, academics are
       more my kind of people!
-}
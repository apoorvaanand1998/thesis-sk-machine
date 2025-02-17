(module
    (type $appNode
        (struct (field $left (mut anyref))
                (field $right (mut anyref))
                (field $name i32))) ;; name only for debugging, shall be removed later
    
    ;; i31 represents constants
    ;; the combinators and primitives are represented using ASCII values
    ;; that are stored in structs

    (type $comb
        (struct (field $asciiTag i32))) ;; 8 bits enough to store ascii values
                                        ;; making it 32 bit rn for convenience
                                        ;; TODO: Change to 8-bit and create function i8 -> i32

    (type $stack
        (array (mut anyref)))

    ;; LAS - Left Ancestor Stack
    (func $createLAS (export "createLAS")
     (param $an (ref null $appNode)) (param $treeHeight i32) (result (ref null $stack))
        ;; treeHeight here is always 1 more than the number of indices needed by the LAS
        (local $las (ref null $stack))
        (local $i i32) ;; stack variable
        (local $curr (ref null $appNode)) ;; current pointer to node while travelling down tree
                                          ;; probably don't need this. TODO: Reuse $an as $curr

        (local.get $an)
        (ref.is_null)
        (if (result (ref null $stack))
            (then
                (ref.null $stack)
            )
            (else
                (ref.null any)
                (local.get $treeHeight)
                (array.new $stack)
                (local.set $las)

                (i32.const 0)
                (local.set $i)

                (local.get $an)
                (local.set $curr)
                
                (block $settingI
                    (loop $setI
                        (local.get $las)
                        (local.get $i)
                        (local.get $curr)
                        (array.set $stack)
                        ;; set current node in the las, after which we check
                
                        (local.get $curr)
                        (struct.get $appNode $left)
                        (ref.test (ref null $appNode))
                        (i32.eqz)
                        (br_if $settingI) ;; it was not an appNode end this loop

                        (local.get $i)
                        (i32.const 1)
                        (i32.add)
                        (local.set $i)
                        ;; ;; increment i
                        (local.get $curr)
                        (struct.get $appNode $left)
                        (ref.cast (ref null $appNode)) 
                        (local.set $curr)
                        ;; set curr to left of curr
                        (br $setI)
                        ;; loop
                    )
                )
                (local.get $las)
            )
        )
    )

    ;; supported combinators and primitives
    ;; S, K, Y, C, B, I, cond, true, false, plus, minus, times, divide, intEq, intGt, intLt, intGte, intLte, and, or, not
    
    (func $step (export "step") 
     (param $las (ref null $stack)) (param $n i32) (result i32)
     ;; function returns the next index of the las we should work with
     ;; n is the index of the las we are currently working with
        (local $f anyref)
        (local $g anyref)
        (local $x anyref)
        (local $ascii i32) ;; i8 can work here too
        ;; get the nth element of the las
        (local.get $las)
        (local.get $n)
        (array.get $stack)
        ;; take the left and check what kind of combinator it is
        (ref.cast (ref null $appNode))
        (struct.get $appNode $left)
        (ref.cast (ref null $comb))
        (struct.get $comb $asciiTag)
        (local.tee $ascii)
        ;; the nth argument of the combinator will be the right subtree 
        ;; of the object n places behind it on the stack

        ;; do a case split here based on the combinator we have
        ;; load args into locals
        ;; use locals to create new struct
        ;; connect newly created struct to the previous structs
        ;; return new index of las to work with (and also modify las)

        (block $combCase (param i32) (result i32)
            ;; check if it's C
            (i32.const 67)
            (i32.eq)
            (if (result i32)
            (then
                ;; load onto f, g and x the right values
                (local.get $las)
                (local.get $n)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (struct.get $appNode $right)
                (local.set $f)

                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (struct.get $appNode $right)
                (local.set $g)

                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (struct.get $appNode $right)
                (local.set $x)

                ;; create a new struct that represents the C rule
                ;; i.e. C f g x = (f x) g

                ;; See Figure 5 in Turner's paper
                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
                (ref.cast (ref null $appNode)) ;; 1st argument of struct.set coming up
                (local.get $f)
                (local.get $x)
                (i32.const 42) ;; unnecessary name - will possibly use for debugging
                (struct.new $appNode) ;; 2nd argument of struct.set
                (local.tee $f) ;; reusing f as a variable, we will use this new value for setting up the new las
                (struct.set $appNode $left)
                ;; now right
                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
                (ref.cast (ref null $appNode)) ;; 1st argument of struct.set coming up
                (local.get $g)
                (struct.set $appNode $right)
                ;; both left and right have been set
                ;; return the new index of las after modifying las
                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (local.get $f)
                (array.set $stack)
                ;; las has been modified
                (local.get $n)
                (i32.const 1)
                (i32.sub)
            )
            ;; odd indentation to make it seem like a case match
            (else
            ;; check if it is I
            (local.get $ascii)
            (i32.const 73)
            (i32.eq)
            (if (result i32)
            (then
                (local.get $las)
                (local.get $n)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (struct.get $appNode $right)
                (local.set $x)
                
                ;; I x = x
                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (local.get $x)
                (struct.set $appNode $left)

                ;; modify las
                (local.get $las)
                (local.get $n)
                (local.get $x)
                (array.set $stack)
                ;; now return new index
                (local.get $n)
            )
            (else
            ;; check if it is plus    
            (local.get $ascii)
            (i32.const 112)
            (i32.eq)
            (if (result i32)
            (then
                (local.get $las)
                (local.get $n)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (struct.get $appNode $right)
                (local.set $f)

                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (struct.get $appNode $right)
                (local.set $g)

                ;; QUESTION - Do you think the arguments of plus will always be reduced?
                ;; if not - how do you do (+) (reduce f) (reduce g)?
                ;; Okay, the first argument of plus is always reduced, this is because if
                ;; it wasn't reduced, it would be further along the LAS and would be reduced first
                ;; but what about the second argument?
                
            )   
            (else
                (i32.const 100)
            )
            )
            )
            )
            )
        )
        )
    )

    (func $reduce (export "reduce")
        (param $an (ref null $appNode)) (param $treeHeight i32) (result i32) 
        ;; I am going to support a result type of only i32 for now, would work with bools as well
        ;; later on, perhaps f64 is a better type to encompass everything?
        ;; treeHeight = max index of LAS + 1
        (local $las (ref null $stack))
        (local $curr (ref null $appNode))

        (local.get $an)
        (local.get $treeHeight)
        (call $createLAS)
        (local.set $las)
        
        ;; an expression is fully reduced when step returns 0
        ;; this would mean there is nothing to do, except look at the 
        ;; hopefully I combinator on the left and return the i32 on the right as result
        ;; ah no, K x y at the height of 1 is also a fully reduced expression
        ;; but what I can do there is to reduce it to (I x) instead of just x
        ;; same thing when it comes to other primOps
        ;; so that's what Turner meant with the whole (I x) thing!

        ;; now reduce treeHeight to go from 1-indexed to 0-indexed
        (local.get $treeHeight)
        (i32.const 1)
        (i32.sub)
        (local.tee $treeHeight)

        (block $untilStepFully
            (i32.eqz)
            (if (result i32)
            (then
                (local.get $las)
                (i32.const 0)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (local.tee $curr)
                (struct.get $appNode $left)
                (ref.cast (ref null $comb))
                (struct.get $comb $asciiTag)
                (i32.const 73)
                (i32.eq)
                ;; checking if it is an I

                (if (result i32)
                (then
                    (local.get $curr)
                    (struct.get $appNode $right)
                    ;; hopefully right is an i32
                )
                (else
                    (unreachable)
                    ;; why the FUCK is it not an I?
                )
                )
            )
            (else
                (local.get $las)
                (local.get $treeHeight)
                (call $step) ;; this returns 0 if we are done, otherwise redo this with newly set treeHeight
                (local.tee $treeHeight)
                (br $untilStepFully)
            )
            )
        )
    )

    (func $main (export "main") (result i32)
        (local $las (ref null $stack))
        (local $n i32)
        (i32.const 67) ;; C
        (struct.new $comb)
        (i32.const 73) ;; I
        (struct.new $comb)
        (i32.const 1) ;; name 1
        (struct.new $appNode)
        (i32.const 2) ;; constant 2
        (ref.i31)
        (i32.const 2) ;; name 2
        (struct.new $appNode)
        (i32.const 112) ;; plus
        (struct.new $comb)
        (i32.const 1) ;; constant 1
        (ref.i31)
        (i32.const 3) ;; name 3
        (struct.new $appNode)
        (i32.const 4) ;; name 4
        (struct.new $appNode)
        (i32.const 3) ;; tree height plus one
        (call $createLAS)
        (local.tee $las)
        (i32.const 2) ;; index of final element of LAS
        (call $step)
        (local.set $n)
        (local.get $las)
        (local.get $n)
        (call $step)
        (local.set $n)
        (local.get $las)
        (local.get $n)
        (call $step)
        ;; (drop)
        ;; (local.get $las)
        ;; (i32.const 1)
        ;; (array.get $stack)
        ;; (ref.cast (ref null $appNode))
        ;; (struct.get $appNode $left)
        ;; (ref.cast (ref null $comb))
        ;; (struct.get $comb $asciiTag)
    )
)
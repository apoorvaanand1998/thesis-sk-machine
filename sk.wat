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

    (func $leftSpineLength (export "leftSpineLength")
     (param $an (ref null $appNode)) (result i32)
        ;; how many times can you keep going left?
        ;; compatible with createLAS (i.e. if you can go left once, it returns 2)
        (local $n i32)
        (i32.const 1)
        (local.set $n)

        (loop $keepLeft (result i32)
            (local.get $an)
            (struct.get $appNode $left)
            (ref.test (ref null $appNode))
            (if
            (then
                (local.get $n)
                (i32.const 1)
                (i32.add)
                (local.set $n) ;; increase n by 1

                (local.get $an)
                (struct.get $appNode $left)
                (ref.cast (ref null $appNode))
                (local.set $an)
                (br $keepLeft)
            )
            (else
                (nop) ;; just fall out of the loop block
            )
            )
            (local.get $n)
        )
    )

    ;; LAS - Left Ancestor Stack
    (func $createLAS (export "createLAS")
     (param $an (ref null $appNode)) (param $treeHeight i32) (result (ref null $stack))
        ;; treeHeight here is always 1 more than the number of indices needed by the LAS
        ;; $an will refer to the current pointer, previously used another variable called $curr
        (local $las (ref null $stack))
        (local $i i32) ;; stack variable

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
                
                (block $settingI
                    (loop $setI
                        (local.get $las)
                        (local.get $i)
                        (local.get $an)
                        (array.set $stack)
                        ;; set current node in the las, after which we check
                
                        (local.get $an)
                        (struct.get $appNode $left)
                        (ref.test (ref null $appNode))
                        (i32.eqz)
                        (br_if $settingI) ;; it was not an appNode end this loop

                        (local.get $i)
                        (i32.const 1)
                        (i32.add)
                        (local.set $i)
                        ;; ;; increment i
                        (local.get $an)
                        (struct.get $appNode $left)
                        (ref.cast (ref null $appNode)) 
                        (local.set $an)
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
        (local.set $ascii)
        ;; the nth argument of the combinator will be the right subtree 
        ;; of the object n places behind it on the stack

        ;; do a case split here based on the combinator we have
        ;; load args into locals
        ;; use locals to create new struct
        ;; connect newly created struct to the previous structs
        ;; return new index of las to work with (and also modify las)

        (block $combCase (result i32)
            ;; check if it's C
            (local.get $ascii)
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
                (array.get $stack) ;; this cannot gave out-of-bounds because the case with n <= 1 is (hopefully) taken care of by reduce
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
                
                (local.get $f)
                (call $i31OrReduce)
                (local.get $g)
                (call $i31OrReduce)
                (i32.add)
                (ref.i31)
                (call $mkI)
                (local.set $x)
                
                ;; changed struct connections to point to new struct
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (i32.const -1)
                (i32.gt_s)
                (if
                (then
                    (local.get $las)
                    (local.get $n)
                    (i32.const 2)
                    (i32.sub)
                    (array.get $stack)
                    (ref.cast (ref null $appNode))
                    (local.get $x)
                    (struct.set $appNode $right)
                )
                (else
                    (nop)
                )
                )
                ;; modify las
                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (local.get $x)
                (array.set $stack)
                ;; return new index
                (local.get $n)
                (i32.const 1)
                (i32.sub)
            )
            (else
            ;; check if it is S
            (local.get $ascii)
            (i32.const 83)
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

                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (struct.get $appNode $right)
                (local.set $x)
                
                (local.get $f)
                (local.get $x)
                (i32.const 42)
                (struct.new $appNode)
                (local.tee $f)
                (local.get $g)
                (local.get $x)
                (i32.const 42)
                (struct.new $appNode)
                (i32.const 42)
                (struct.new $appNode)
                (local.set $x)

                (local.get $n)
                (i32.const 3)
                (i32.sub)
                (i32.const -1)
                (i32.gt_s)
                (if
                (then
                    (local.get $las)
                    (local.get $n)
                    (i32.const 3)
                    (i32.sub)
                    (array.get $stack)
                    (ref.cast (ref null $appNode))
                    (local.get $x)
                    (struct.set $appNode $right)
                )
                (else
                    (nop)
                )
                )
                ;; modify las
                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (local.get $f)
                (ref.cast (ref null $appNode))
                (array.set $stack)
                ;; two modifications to las, first one above, second one below
                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (local.get $x)
                (ref.cast (ref null $appNode))
                (array.set $stack)
                ;; modified las, now return index
                (local.get $n)
                (i32.const 1)
                (i32.sub)
            )
            (else
            ;; check if it is K
            (local.get $ascii)
            (i32.const 75)
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

                (local.get $f)
                (call $mkI)
                (local.set $f)

                ;; connect newly created struct to the previous struct
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (i32.const -1)
                (i32.gt_s)
                (if
                (then
                    (local.get $las)
                    (local.get $n)
                    (i32.const 2)
                    (i32.sub)
                    (array.get $stack)
                    (ref.cast (ref null $appNode))
                    (local.get $f)
                    (struct.set $appNode $right)
                )
                (else
                    (nop)
                )
                )
                ;; modify las
                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (local.get $f)
                (ref.cast (ref null $appNode))
                (array.set $stack)
                ;; index return
                (local.get $n)
                (i32.const 1)
                (i32.sub)
            )
            (else
            ;; check if it is B
            (local.get $ascii)
            (i32.const 66)
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

                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (struct.get $appNode $right)
                (local.set $x)

                (local.get $f)
                (local.get $g)
                (local.get $x)
                (i32.const 42)
                (struct.new $appNode)
                (local.tee $g)
                (i32.const 42)
                (struct.new $appNode)
                (local.set $x)
                
                ;; doing things similar to C comb
                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (local.get $f)
                (struct.set $appNode $left)

                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (local.get $g)
                (struct.set $appNode $right)

                ;; modifying las
                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (local.get $x)
                (array.set $stack)

                (local.get $n)
                (i32.const 2)
                (i32.sub)
            )
            (else
            ;; check if it is Y combinator
            ;; wait, what's a start-up incubator doing here?
            (local.get $ascii)
            (i32.const 89)
            (i32.eq)
            (if (result i32)
            (then
                (local.get $las)
                (local.get $n)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (struct.get $appNode $right)
                (local.set $f)

                (local.get $f)
                (local.get $f) 
                ;; this is the temp right node I am setting
                ;; it needs to be a ref to this appNode itself
                ;; which will be set a little bit below
                (i32.const 42)
                (struct.new $appNode)
                (local.tee $x)
                ;; now setting it right ;)
                (ref.cast (ref null $appNode))
                (local.get $x)
                (struct.set $appNode $right)
                ;; x now holds a self-referential node

                ;; again similar to B, C combinator
                ;; TODO: Check if other combinators can be written this way?
                ;; I think you could probably do it the "if" way as well
                (local.get $las)
                (local.get $n)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (local.get $x)
                (ref.cast (ref null $appNode))
                (struct.get $appNode $left)
                (struct.set $appNode $left) ;; set las[n].left = x.left, similarly
                (local.get $las)
                (local.get $n)
                (array.get $stack)
                (ref.cast (ref null $appNode))
                (local.get $x)
                (ref.cast (ref null $appNode))
                (struct.get $appNode $right)
                (struct.set $appNode $right)
                ;; didn't need to do "if" check
                ;; "newly" created struct already correctly connected to previous structs
                ;; not necessary to modify las either
                (local.get $n) ;; just return the index
            )
            (else
            (local.get $ascii)
            (unreachable)
            )
            )
            )
            )
            )
            )
            )
            )
            )
            )
            )
            )
            )
        )
        )
    )

    (func $i31OrReduce (export "i31OrReduce")
     ;; looks at the anyref given to it and if it's an i31, converts it to i32
     ;; otherwise assumes it is an appNode and calls reduce on it
     (param $x anyref) (result i32)
        (local $temp (ref null $appNode))
        (local $n i32)

        (local.get $x)
        (ref.test i31ref)
        (if (result i32)
        (then
            (local.get $x)
            (ref.cast i31ref)
            (i31.get_s)
        )
        (else
            (local.get $x)
            (ref.cast (ref null $appNode))
            (local.tee $temp)
            (call $leftSpineLength)
            (local.set $n)
            (local.get $temp)
            (local.get $n)
            (call $reduce)
        )
        )
    )

    (func $mkI (export "mkI")
     ;; helper function that puts things into an appNode with the left being the I combinator
     (param $x anyref) (result (ref null $appNode))

        (i32.const 73)
        (struct.new $comb)
        (local.get $x)
        (i32.const 42) ;; same default name
        (struct.new $appNode)
    )

    (func $reduce (export "reduce")
        (param $an (ref null $appNode)) (param $treeHeight i32) (result i32) 
        ;; I am going to support a result type of only i32 for now, would work with bools as well
        ;; later on, perhaps f64 is a better type to encompass everything?
        ;; treeHeight = max index of LAS + 1
        (local $las (ref null $stack))
        (local $curr (ref null $appNode))
        (local $result i32)

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
        ;; so that's what Turner meant with the whole (I x) thing! (look at func mkI)

        ;; now reduce treeHeight to go from 1-indexed to 0-indexed
        (local.get $treeHeight)
        (i32.const 1)
        (i32.sub)
        (local.set $treeHeight)
        
        (loop $untilStepFully
            (local.get $treeHeight)
            (i32.eqz)
            (if
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

                (if
                (then
                    (local.get $curr)
                    (struct.get $appNode $right)
                    (ref.cast i31ref)
                    (i31.get_s)
                    (local.set $result)
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
                (local.set $treeHeight)
                (br $untilStepFully)
            )
            )
        )
        (local.get $result)
    )

    (func $main (export "main") (result i32)
        (local $n i32)
        (local $an (ref null $appNode))
        (local $s (ref null $stack))

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
        (local.tee $an)
        (call $leftSpineLength)
        (local.set $n)
        (local.get $an)
        (local.get $n)
        ;; (call $createLAS)
        ;; (local.tee $s)
        ;; (i32.const 2)
        ;; (call $step)
        ;; (drop)
        ;; (local.get $s)
        ;; (i32.const 1)
        ;; (call $step)
        ;; (drop)
        ;; (local.get $s)
        ;; (i32.const 1)
        ;; (array.get $stack)
        ;; (ref.cast (ref null $appNode))
        ;; (struct.get $appNode $left)
        ;; (ref.cast (ref null $comb))
        ;; (struct.get $comb $asciiTag)
        ;; (drop)
        ;; (local.get $s)
        ;; (i32.const 1)
        ;; (call $step)
        ;; (drop)
        ;; (local.get $s)
        ;; (i32.const 0)
        ;; (array.get $stack)
        ;; (ref.cast (ref null $appNode))
        ;; (struct.get $appNode $left)
        ;; (ref.cast (ref null $comb))
        ;; (struct.get $comb $asciiTag)
        (call $reduce)
        (drop)
        (struct.new $appNode (struct.new $appNode (struct.new $comb (i32.const 112))
                                                  (struct.new $appNode (struct.new $appNode (struct.new $comb (i32.const 112))
                                                                                            (ref.i31 (i32.const 1))
                                                                                            (i32.const 1))
                                                                       (ref.i31 (i32.const 2))
                                                                       (i32.const 2))
                                                  (i32.const 3)) 
                             (struct.new $appNode (struct.new $appNode (struct.new $comb (i32.const 112))
                                                                       (ref.i31 (i32.const 3))
                                                                       (i32.const 42))
                                                  (ref.i31 (i32.const 4))
                                                  (i32.const 42))
                             (i32.const 42))
        (local.tee $an)
        (call $leftSpineLength)
        (local.set $n)
        (local.get $an)
        (local.get $n)
        (call $reduce)
    )
)
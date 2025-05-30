(module
    (type $appNode
        (struct (field $left (mut anyref))
                (field $right (mut anyref))
                (field $name i32))) ;; name only for debugging, shall be removed later
    
    ;; Based on Marco's suggestion, refer to combinators and primitives with i31s too
    ;; i31s on the left node refer to prims and combs
    ;; i31s on the right node refer to constants
    ;; the explicit structs are the appNodes themselves

    (type $stack
        (array (mut (ref null $appNode))))

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
                (ref.null $appNode)
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
        (struct.get $appNode $left)
        (ref.cast i31ref)
        (i31.get_s)
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
                (struct.get $appNode $right)
                (local.set $f)

                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (array.get $stack)
                (struct.get $appNode $right)
                (local.set $g)

                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
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
                (local.get $g)
                (struct.set $appNode $right)
                ;; both left and right have been set
                ;; return the new index of las after modifying las
                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (local.get $f)
                (ref.cast (ref null $appNode))
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
                (struct.get $appNode $right)
                (local.set $x)
                
                ;; I x = x
                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (array.get $stack) ;; this cannot gave out-of-bounds because the case with n <= 1 is (hopefully) taken care of by reduce
                (local.get $x)
                (struct.set $appNode $left) ;; prove that only left is required, the right case will never happen
                ;; Informal proof?
                ;; Everything on the left node has to eventually reduce to a primitive operation
                ;; Everything on the right node has to eventually reduce to a primitive value
                ;; This only reduces the I's on the left nodes
                ;; Any I's that appear on the right node are left alone
                ;; Why is that okay?
                ;; Assuming that after all reductions everything on the left has reduced to a primitive operation
                ;; We will now have to make sure and reduce the thing on the right node (the argument of the primitive operation)
                ;; because primitive operations require their arguments to be fully reduced
                ;; SO, even if the right nodes are not reduced
                ;; Eventually, they will have to be (and when that happens, they will be treated as left nodes in that new LAS created)
                ;; QED? :D

                ;; modify las
                (local.get $las)
                (local.get $n)
                (local.get $x)
                (ref.cast (ref null $appNode))
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
                (struct.get $appNode $right)
                (local.set $f)

                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (array.get $stack)
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
                (local.set $x)

                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (array.get $stack)
                (i32.const 73)
                (ref.i31)
                (struct.set $appNode $left)

                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (array.get $stack)
                (local.get $x)
                (struct.set $appNode $left)
                ;; modify las

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
                (struct.get $appNode $right)
                (local.set $f)

                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (array.get $stack)
                (struct.get $appNode $right)
                (local.set $g)

                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
                (struct.get $appNode $right)
                (local.set $x)
                
                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
                (local.get $f)
                (local.get $x)
                (i32.const 42)
                (struct.new $appNode)
                (local.tee $f)
                (struct.set $appNode $left)

                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
                (local.get $g)
                (local.get $x)
                (i32.const 42)
                (struct.new $appNode)
                (struct.set $appNode $right)

                ;; modify las
                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (local.get $f)
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
                (local.set $x)

                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (array.get $stack)
                (i32.const 73)
                (ref.i31)
                (struct.set $appNode $left)

                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (array.get $stack)
                (local.get $x)
                (struct.set $appNode $right)

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
                (struct.get $appNode $right)
                (local.set $f)

                (local.get $las)
                (local.get $n)
                (i32.const 1)
                (i32.sub)
                (array.get $stack)
                (struct.get $appNode $right)
                (local.set $g)

                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
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
                (local.get $f)
                (struct.set $appNode $left)

                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (array.get $stack)
                (local.get $g)
                (struct.set $appNode $right)

                ;; modifying las
                (local.get $las)
                (local.get $n)
                (i32.const 2)
                (i32.sub)
                (local.get $x)
                (ref.cast (ref null $appNode))
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
                (local.tee $x)
                (ref.cast (ref null $appNode))
                (struct.get $appNode $right)
                (local.set $f)

                (local.get $las)
                (local.get $n)
                (array.get $stack)
                (local.get $f)
                (struct.set $appNode $left)

                (local.get $las)
                (local.get $n)
                (array.get $stack)
                (local.get $x)
                (struct.set $appNode $right)
                
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
                (local.tee $curr)
                (struct.get $appNode $left)
                ;; (ref.cast (ref null $comb))
                ;; (struct.get $comb $asciiTag)
                (ref.cast i31ref)
                (i31.get_s)
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
        ;; (C I 2 (plus 1))
        (i32.const 67)
        (ref.i31) ;; C
        (i32.const 73)
        (ref.i31) ;; I
        (i32.const 0)
        (struct.new $appNode)
        (i32.const 2)
        (ref.i31) ;; 2
        (i32.const 1)
        (struct.new $appNode)
        (i32.const 112)
        (ref.i31) ;; +
        (i32.const 1)
        (ref.i31) ;; 1
        (i32.const 2)
        (struct.new $appNode)
        (i32.const 3)
        (struct.new $appNode)
        (local.tee $an)
        (call $leftSpineLength)
        (local.set $n)
        (local.get $an)
        (local.get $n)
        (call $reduce)
        (drop)
        (struct.new $appNode (struct.new $appNode (ref.i31 (i32.const 112))
                                                  (struct.new $appNode (struct.new $appNode (ref.i31 (i32.const 112))
                                                                                            (ref.i31 (i32.const 1))
                                                                                            (i32.const 1))
                                                                       (ref.i31 (i32.const 2))
                                                                       (i32.const 2))
                                                  (i32.const 3)) 
                             (struct.new $appNode (struct.new $appNode (ref.i31 (i32.const 112))
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
(module
    (type $appNode
        (struct (field $left anyref)
                (field $right anyref)
                (field $name i32))) ;; name only for debugging, shall be removed later
    
    ;; i31 represents constants
    ;; the combinators and primitives are represented using ASCII values
    ;; that are stored in structs

    (type $comb
        (struct (field $asciiTag i8))) ;; 8 bits enough to store ascii values

    (type $stack
        (array (mut anyref)))

    ;; LAC - Left Ancestor Stack
    (func $createLAS (export "createLAS")
     (param $an (ref null $appNode)) (param $treeHeight i32) (result (ref null $stack))
        (local $las (ref null $stack))
        (local $i i32) ;; stack variable
        (local $curr (ref null $appNode)) ;; current pointer to node while travelling down tree

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

    ;; S, K, Y, C, B, I, cond, true, false, plus, minus, times, divide, intEq, intGt, intLt, intGte, intLte, and, or, not
    
    (func $step (export "step") 
     (param $las (ref null $stack)) (param $n i32) (result i32)
     ;; function doesn't return a result, just manipulates the references given to it
     ;; n is the pointing to the index of the las we are currently working with
        (local $f (ref null $appNode))
        (local $g (ref null $appNode))
        (local $x (ref null $appNode))

        ;; get the nth element of the las
        (local.get $las)
        (local.get $n)
        (array.get $stack)
        ;; take the left and check what kind of combinator it is
        (ref.cast (ref null $appNode))
        (struct.get $appNode $left)
        ;; (ref.test (ref null $appNode))
        ;; (ref.test (ref null $comb))
        (ref.cast (ref null $comb))
        (struct.get_s $comb $asciiTag)
        (i32.extend8_s)
    )

    (func $main (export "main") (result i32)
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
    )
)
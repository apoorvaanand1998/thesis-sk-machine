(module
    (type $appNode
        (struct (field $left anyref)
                (field $right anyref)
                (field $name i32)))
    
    ;; i31 represents constants
    ;; the combinators and primitives are represented using ASCII values
    ;; that are stored in structs

    (type $comb
        (struct (field $asciiTag i8)))

    (type $stack
        (array (mut anyref)))

    ;; S 
    ;; K
    ;; Y
    ;; C
    ;; B
    ;; I
    ;; cond
    ;; true
    ;; false
    ;; plus
    ;; minus
    ;; times
    ;; divide
    ;; intEq
    ;; intGt
    ;; intLt
    ;; intGte
    ;; intLte
    ;; and
    ;; or
    ;; not
    
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
                ;; ;; (i32.const 1) ;; index
                ;; ;; (i32.const 69) ;; thing to change
                ;; ;; (ref.i31)
                ;; ;; (array.set $stack) ;; instruction with type of array to change
                
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
                        (br $setI)
                    )
                )

                (local.get $las)
            )
        )
    )

    ;; if left is a combinator or i31, print it
    (func $help (export "help") (param $p (ref $appNode)) (result i32)
        (local.get $p)
        (struct.get $appNode $left)
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
        (i32.const 3) ;; height of tree
        (call $createLAS)
        (i32.const 2)
        (array.get $stack)
        (ref.cast (ref null $appNode))
        (struct.get $appNode $name)
        ;; (i32.const 1)
        ;; (array.get $stack)
        ;; (ref.cast (ref null i31))
        ;; (i31.get_s)
    )
)
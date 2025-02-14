(module
    (type $appNode 
        (struct (field $left anyref)
                (field $right anyref)))
    
    (type $comb (struct (field $asciiTag i32)))

    (func $prob1 (export "prob1") (param $p (ref null $appNode)) (result i32)
        (local.get $p)
        (struct.get $appNode $left)
        (ref.cast (ref null $appNode))
        (struct.get $appNode $right)
        (ref.test (ref null $comb))
    )

    (func $prob2 (export "prob2") (param $p (ref null $appNode)) (result i32)
        (local.get $p)
        (struct.get $appNode $left)
        (ref.cast (ref null $appNode))
        (struct.get $appNode $left)
        (ref.test (ref null $comb))
    )

    (func $main (export "main") (result i32)
        (struct.new $appNode (struct.new $appNode (struct.new $comb (i32.const 67))
                                                  (struct.new $comb (i32.const 73)))
                             (ref.i31 (i32.const 2)))
        (call $prob2)                             
    )
)
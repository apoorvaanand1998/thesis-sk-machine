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

    (func $help (export "help") (param $p (ref $appNode)) (result i32)
        (local.get $p)
        (struct.get $appNode $left)
        (ref.cast (ref null $comb))
        (struct.get_s $comb $asciiTag)
        (i32.extend8_s)
    )

    (func $main (export "main") (result i32)
        (i32.const 75)
        (struct.new $comb)
        (i32.const 420)
        (ref.i31)
        (i32.const 42)
        (struct.new $appNode)
        (call $help)
    )
)
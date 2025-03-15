(module 
    (type $appNode
        (struct (field $left (mut anyref))
                (field $right (mut anyref))
                (field $name i32)))

    (func $test (export "test")
        (param $an (ref null $appNode)) (result (ref null $appNode))

        (local $x anyref)

        (local.get $an)
        (local.tee $x)
        (ref.cast (ref null $appNode))
    )

    (func $main (export "main")
        (result i32)

        (i32.const 1)
        (ref.i31)
        (i32.const 2)
        (ref.i31)
        (i32.const 42)
        (struct.new $appNode)
        (call $test)
        (struct.get $appNode $name)
    )
)
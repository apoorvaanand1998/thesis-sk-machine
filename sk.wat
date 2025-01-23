(module
    (type $node (struct (field $x i32) (field $y i32)))

    (func $helper (export "helper") (param $p (ref $node)) (result i32)
        (local.get $p)
        (struct.get $node $y)
    )

    (func $main (export "main") (result i32)
        (i32.const 4)
        (i32.const 2)
        (struct.new $node)
        (call $helper)
    )
)
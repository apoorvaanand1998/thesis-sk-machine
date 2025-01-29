(module
    (type $arrayElems (array i32))

    (func $main (export "main")
     (result i32)
        (i32.const 5)
        (i32.const 3)
        (array.new $arrayElems)
        (i32.const 1)
        (array.get $arrayElems)
    )
)
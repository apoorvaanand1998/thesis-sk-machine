(module 
    (type $appNode
        (struct (field $left (mut anyref))
                (field $right (mut anyref))
                (field $name i32)))

    ;; (func $test (export "test")
    ;;     (param $an (ref null $appNode)) (result (ref null $appNode))

    ;;     (local $x anyref)

    ;;     (local.get $an)
    ;;     (local.tee $x)
    ;;     (ref.cast (ref null $appNode))
    ;; )

    (func $test2 (export "test2")
        (result i32)

        (local $var i32)
        (i32.const -1)
        (local.set $var)

        (i32.const 2)
        (i32.const 2)
        (i32.eq)
        (if
        (then
          (i32.const 42)
          (local.set $var)
        )
        )

        (i32.const 3)
        (i32.const 2)
        (i32.eq)
        (if
        (then
          (i32.const 22)
          (local.set $var)
        )
        )

        (local.get $var)
    )

    (func $main (export "main")
        (result i32)

        (call $test2)
    )
)
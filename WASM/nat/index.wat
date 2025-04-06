(module
  (memory $memory 1)
  (export "memory" (memory $memory))
  (global $allocated (mut i32) (i32.const 0))
  (global $i32+1 i64 (i64.const 0x100000000))
  (global $i32 i64 (i64.const 0xFFFFFFFF))
  (start $start)
  (func $start
    (global.set $allocated (i32.const 4))
    (i32.store (i32.const 0) (i32.const 1))
  )
  ;; simplest bump allocator
  (func $alloc (export "alloc") (param $bytes i32) (result i32)
    global.get $allocated
    local.get $bytes
    (i32.add (global.get $allocated))
    global.set $allocated
  )
  ;; nat_zero does not need an allocation
  (func $nat_zero (export "nat_zero") (result i32 i32 i32)
    i32.const 0 i32.const 0 i32.const 0
  )
  ;; nat_one is statically allocated
  (func $nat_one (export "nat_one") (result i32 i32 i32)
	i32.const 1
    i32.const 1
    i32.const 0
  )
  ;; allocate a new nat with $limbs number of limbs
  ;; returns a struct: $size $used $ptr
  ;; where $size is what was allocated, $used is how many
  ;; limbs have digits, and $ptr is the pointer to the
  ;; allocated array of u32 limbs
  (func $nat_alloc (export "nat_alloc") (param $limbs i32) (result i32 i32 i32)
    (local $ptr i32)
    local.get $limbs
    (i32.mul (i32.const 4))

    call $alloc
    local.set $ptr

    local.get $limbs
    i32.const 0
    local.get $ptr
  )
  ;; zero out a natural number
  (func $nat_zero_out (export "nat_zero_out") (param $size i32) (param $used i32) (param $ptr i32)
    local.get $ptr
    i32.const 0
    local.get $used
    (i32.mul (i32.const 4))
    memory.fill
  )
  ;; clear unused digits of the natural number
  (func $nat_clean (export "nat_clean") (param $size i32) (param $used i32) (param $ptr i32)
    local.get $ptr
    local.get $used
    (i32.mul (i32.const 4))
    i32.add
    i32.const 0
    (i32.sub (local.get $size) (local.get $used))
    (i32.mul (i32.const 4))
    memory.fill
  )
  ;; copy limbs from one nat to another (takes the smaller size)
  (func $nat_copy (export "nat_copy")
    (param $size1 i32) (param $used1 i32) (param $ptr1 i32)
    (param $size2 i32) (param $used2 i32) (param $ptr2 i32)
    local.get $ptr2
    local.get $ptr1

    local.get $used1
    local.get $used2
    i32.lt_u
    (if (result i32)
      (then local.get $used1)
      (else local.get $used2)
    )
    (i32.mul (i32.const 4))
    memory.copy
  )
  ;; allocate a new $ptr for the nat, and copy it over
  (func $nat_realloc (export "nat_realloc")
    (param $size i32) (param $used i32) (param $ptr i32) (param $new_size i32)
    (result i32 i32 i32)
    local.get $size
    local.get $used
    local.get $ptr

    local.get $new_size
    local.get $used

    local.get $new_size
	(i32.mul (i32.const 4))
    call $alloc
    local.tee $ptr

    call $nat_copy
    local.get $new_size
    local.get $used
    local.get $ptr
  )
  ;; realloc if $size_at_least < $size
  (func $nat_ensure (export "nat_ensure")
    (param $size i32) (param $used i32) (param $ptr i32) (param $size_at_least i32)
    (result i32 i32 i32)
    local.get $size
    local.get $size_at_least
    i32.lt_u
    (if
      (then
        local.get $size local.get $used local.get $ptr
        local.get $size_at_least
        call $nat_realloc
        local.set $ptr local.set $used local.set $size
      )
    )
    local.get $size local.get $used local.get $ptr
  )
  ;; get digit $digit of limbs pointer $ptr
  (func $nat_get (export "nat_get") (param $ptr i32) (param $digit i32) (result i32)
    local.get $ptr
    local.get $digit
    (i32.mul (i32.const 4))
    i32.add
    i32.load
  )
  ;; set digit $digit of limbs pointer $ptr to $value
  (func $nat_set (export "nat_set") (param $ptr i32) (param $digit i32) (param $value i32)
    local.get $ptr
    local.get $digit
    (i32.mul (i32.const 4))
    i32.add
    local.get $value
    i32.store
  )
  (func $overflow (param i64) (result i32) local.get 0 global.get $i32+1 i64.ge_u)
  ;; increase the value of <$size $used $ptr> by $bump
  (func $nat_bump (export "nat_bump")
    (param $bump i32)
    (param $size i32) (param $used i32) (param $ptr i32)
    (result i32 i32 i32)

    (local $carry i64)
    (local $i i32)

    (local.set $carry (i64.extend_i32_u (local.get $bump)))
    (loop $de-loop
      local.get $carry
      (i64.ne (i64.const 0))
      local.get $i
      local.get $used
      i32.lt_u
      i32.and
      (if
        (then
          local.get $carry

          local.get $ptr
          local.get $i
          call $nat_get
          i64.extend_i32_u
          i64.add
          local.set $carry

          local.get $ptr
          local.get $i
          local.get $carry
          i32.wrap_i64
          call $nat_set

          local.get $carry
          i64.const 32
          i64.shr_u
          local.set $carry

          local.get $i
          (i32.add (i32.const 1))
          local.set $i

          br $de-loop
        )
      )
    )
    local.get $carry
    (i64.ne (i64.const 0))
    (if
      (then
        local.get $size local.get $used local.get $ptr
        (i32.add (local.get $used) (i32.const 1))
        call $nat_ensure
        local.set $ptr local.set $used local.set $size

        local.get $used
        (i32.add (i32.const 1))
        local.set $used

        local.get $ptr
        local.get $i
        local.get $carry
        i32.wrap_i64
        call $nat_set
      )
    )
    local.get $size local.get $used local.get $ptr
  )
)

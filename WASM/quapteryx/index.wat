(module
  (func $dbg_import (import "imports" "dbg") (param i32) (param i64))
  (global $debug (export "debug") (mut i32) (i32.const 0))
  (func $dbg (param i32) (param i64)
    (if (global.get $debug)
      (then (call $dbg_import (local.get 0) (local.get 1)))
    )
  )

  (memory $input 1)
  (memory $stack 1)
  (memory $shared 1)
  (memory $output 1)
  (export "input" (memory $input))
  (export "stack" (memory $stack))
  (export "shared" (memory $shared))
  (export "output" (memory $output))
  (global $allocated_input (export "allocated_input") (mut i32) (i32.const 0))
  (global $allocated_stack (export "allocated_stack") (mut i32) (i32.const 0))
  (global $allocated_shared (export "allocated_shared") (mut i32) (i32.const 0))
  (global $allocated_output (export "allocated_output") (mut i32) (i32.const 0))

  ;; input, stop, and output pointers in **bytes**
  (global $iptr (export "iptr") (mut i32) (i32.const 0))
  (global $sptr (export "sptr") (mut i32) (i32.const 0))
  (global $optr (export "optr") (mut i32) (i32.const 0))
  ;; input, stop, and output sub-word pointers in **bits**
  (global $ibit (export "ibit") (mut i64) (i64.const 0))
  (global $sbit (export "sbit") (mut i64) (i64.const 0))
  (global $obit (export "obit") (mut i64) (i64.const 0))

  ;; safety
  (global $fuel (export "fuel") (mut i32) (i32.const 10000))
  (global $fuel_copy (export "fuel_copy") (mut i32) (i32.const 100))
  (global $fuel_scan (export "fuel_scan") (mut i32) (i32.const 100))

  ;; number of bits in a word
  (global $word_size i64 (i64.const 64))
  ;; minus 2
  (global $word_sizem2 i64 (i64.const 62))
  ;; number of crumbs in a word
  (global $word_crumbs i64 (i64.const 32))
  ;; maximum word (~0 = -1)
  (global $word_max i64 (i64.const 0xFFFFFFFFFFFFFFFF))
  ;; mask off the lowest bit
  (global $mask_even i64 (i64.const 0xFFFFFFFFFFFFFFFE))
  ;; lower bit of each crumb 0b01010101…
  (global $lower i64 (i64.const 0x5555555555555555))
  ;; upper bit of each crumb 0b10101010…
  (global $upper i64 (i64.const 0xAAAAAAAAAAAAAAAA))
  ;; filler pattern (applied identity combinator) 0b00010001…
  (global $filler i64 (i64.const 0x1111111111111111))

  ;; `nonzeros`: 0b01 for each nonzero crumb of `input=crumbs`
  ;;   0b00 => 0b00
  ;;   0b__ => 0b01
  (func $nonzeros (param $input i64) (result i64)
    ;; gather upper and lower bits for each crumb
    local.get $input
    local.get $input
    (i64.shr_u (i64.const 1))
    i64.or
    ;; and take the lower bits
    global.get $lower
    i64.and
  ) ;; $nonzeros

  ;; Scan a whole word (64 bits / 32 crumbs) for redexes at once, in 32 operations
  ;; `crumbs`: a word worth of crumbs encoding SKI combinators and composition
  ;;   0b00000011 => 0b00000011 "3" = S
  ;;   0b000010   => 0b000010   "2" = K
  ;;   0b0001     => 0b0001     "1" = I
  ;;   0b__ => 0b00
  (func $redexes (export "redexes") (param $leadingZeroBits i64) (param $crumbs i64) (result i64)
    (local $I i64)
    (local $K i64)
    (local $S i64)

    ;; 11+9 operations
      ;; zero if there is a run of $arity zeros by that crumb
      (local.set $I (i64.shr_u (local.get $crumbs) (i64.const 2)))
      (local.set $K (i64.or (local.get $I) (i64.shr_u (local.get $crumbs) (i64.const 4))))
      (local.set $S (i64.or (local.get $K) (i64.shr_u (local.get $crumbs) (i64.const 6))))

      ;; handle high-order crumbs
      block $L6 block $L4 block $L2 block $L0
      (i32.wrap_i64 (local.get $leadingZeroBits))
      br_table $L0 $L0 $L2 $L2 $L4 $L4 $L6 $L6
      end $L0
      (local.set $I (i64.or (local.get $I) (i64.const 0xC000000000000000)))
      (local.set $K (i64.or (local.get $K) (i64.const 0xF000000000000000)))
      (local.set $S (i64.or (local.get $S) (i64.const 0xFC00000000000000)))
      br $L6
      end $L2
      (local.set $K (i64.or (local.get $K) (i64.const 0xC000000000000000)))
      (local.set $S (i64.or (local.get $S) (i64.const 0xF000000000000000)))
      br $L6
      end $L4
      (local.set $S (i64.or (local.get $S) (i64.const 0xC000000000000000)))
      br $L6
      end $L6

      ;; zero if there is also the corresponding arity crumb there
      ;; (that is, if the combinator is saturated)
      (local.set $S (i64.or (local.get $S) (i64.xor (local.get $crumbs) (global.get $word_max))))
      (local.set $K (i64.or (local.get $K) (i64.xor (local.get $crumbs) (global.get $upper))))
      (local.set $I (i64.or (local.get $I) (i64.xor (local.get $crumbs) (global.get $lower))))
    ;; 12 operations
      ;; still working in negated logic, detect the crumbs where any is zero
      (i64.and
        (i64.and
          (global.get $lower)
          (i64.or (local.get $I) (i64.shr_u (local.get $I) (i64.const 1))))
        (i64.and
          (i64.or (local.get $K) (i64.shr_u (local.get $K) (i64.const 1)))
          (i64.or (local.get $S) (i64.shr_u (local.get $S) (i64.const 1)))))
      ;; finally negate it, so that nonzero crumbs are where redexes _are_
      (i64.xor (global.get $lower))
      ;; widen it so each full crumb is true or false
      (i64.mul (i64.const 3)) ;; 0b11
      ;; and use it to mask the original crumbstring, to recover the redex types
      (i64.and (local.get $crumbs))
  ) ;; $redexes

  ;; Returns the bitshift to isolate the redex head, and the value of that
  ;; redex head if any
  (func $redex (export "redex") (param $redexes i64) (result i32 i64)
    (local $pos i64)
    (i64.and (global.get $mask_even) (i64.clz (local.get $redexes)))
    local.set $pos

    local.get $redexes
    global.get $word_sizem2
    local.get $pos
    i64.sub
    i64.shr_u
    i64.const 3
    i64.and
    i32.wrap_i64

    local.get $pos
  ) ;; $redex

  ;; 2 * popcount(nonzeros(crumbs)) - word_crumbs
  ;; 6 operations
  (func $deltaExpecting (export "deltaExpecting") (param $crumbs i64) (result i64)
    ;; func $nonzeros
      local.get $crumbs
      local.get $crumbs
      (i64.shr_u (i64.const 1))
      i64.or
      global.get $lower
      i64.and
    ;; 2 * popcount(nonzeros) - word_crumbs
    i64.popcnt
    i64.const 1
    i64.shl
    global.get $word_crumbs
    i64.sub
  ) ;; $deltaExpecting


  ;; compute the bit length (2 * crumb length) where it reaches zero after
  ;; starting at $expecting, or 0 if it does not
  (func $reachesZero (export "reachesZero") (param $expecting i64) (param $crumbs i64) (result i64)
    (local $nonzeros i64)
    (local $deltaExpecting i64)
    ;; variables for the loop
    ;; current bit index
    (local $bit i64)

    local.get $crumbs
    local.get $crumbs
    (i64.shr_u (i64.const 1))
    i64.or
    global.get $lower
    i64.and
    local.set $nonzeros
    ;;local.get $nonzeros
    ;;i64.popcnt
    ;;global.get $word_crumbs
    ;;i64.const 1
    ;;i64.shr_u
    ;;i64.sub
    ;;local.set $deltaExpecting

    ;; no zero crossing in the word if
    ;;   expecting > deltaExpecting + ctz(crumbs)/2
    ;; (since each trailing zero either means that nonzeros started earlier or
    ;; was less spread out)
    (i64.gt_s
      (i64.add
        (local.get $expecting)
        (i64.shr_u (i64.clz (local.get $nonzeros)) (i64.const 1))
      )
      (i64.popcnt (local.get $nonzeros))
    )
    (if (then i64.const 0 return))
    ;; could do a kind of binary search, but for now, just loop
    (local.set $bit (global.get $word_sizem2))
    (loop $de-loop
      (select
        (i64.const -1) ;; select true
        (i64.const 1)  ;; select false
        ;; shift to the bit
        (local.get $nonzeros)
        (local.get $bit)
        (i64.shr_u)
        ;; see if it is set
        (i64.const 1)
        (i64.and)
        (i32.wrap_i64)
      )
      local.get $expecting
      i64.add
      local.tee $expecting
      i64.eqz i32.eqz ;; coerce to boolean
      (if (then
        local.get $bit
        i64.eqz
        (if (then i64.const 0 return))
        local.get $bit
        i64.const 2
        i64.sub
        local.set $bit
        br $de-loop
      ))
    )
    (i64.sub (global.get $word_size) (local.get $bit))
  ) ;; $reachesZero


  ;; Slowest interpreter

  ;; Takes the number of crumbs (combinators) in the input, and returns the
  ;; number of crumbs in the output. Internally these are split into a memory
  ;; pointer (in bytes) and a bit offset.
  (func $slowest (export "slowest") (param $crumbs_length i64) (result i64)
    ;; the current word we are looking at
    (local $crumbs i64)
    ;; the redexes from it,
    (local $redexes i64)
    ;; from $redex($redexes)
    (local $shift i64)
    (local $arity i32)
    (local $arity2 i64)
    ;; carry over trailing zeros as leading zeros
    (local $leadingZeroBits i64)
    ;; temporaries for tracking y in Sxyz
    (local $yptr i32)
    (local $ybit i64)
    ;; temporaries for reverting a reduction without enough arguments
    (local $riptr i32)
    (local $ribit i64)
    (local $roptr i32)
    (local $robit i64)
    ;; tmp
    (local $tmp i64)

    ;; set the stop pointer
    (global.set $sbit (i64.and (i64.const 0x3F) (i64.shl (local.get $crumbs_length) (i64.const 1))))
    (global.set $sptr (i32.wrap_i64 (i64.mul (i64.const 8) (i64.shr_u (local.get $crumbs_length) (i64.const 5)))))

    ;; loop until it is normalize (no more redexes)
    block $normalized
    (global.set $fuel (i32.add (global.get $fuel) (i32.const 1)))
    (loop $until-normalized
      ;; infinite loop protection
      (global.set $fuel (i32.sub (global.get $fuel) (i32.const 1)))
      (global.get $fuel) i32.eqz br_if $normalized

      ;; read from beginning
      (global.set $iptr (i32.const 0))
      (global.set $ibit (i64.const 0))
      ;; reset/reuse output tape
      (global.set $optr (i32.const 0))
      (global.set $obit (i64.const 0))

      ;; awawawa
      (local.set $leadingZeroBits (i64.const 0))

      loop $reverted-redex
      block $found-redex
      (global.set $fuel_scan (i32.add (global.get $fuel_scan) (i32.const 1)))
      (loop $find-redex
        ;; infinite loop protection
        (global.set $fuel_scan (i32.sub (global.get $fuel_scan) (i32.const 1)))
        (global.get $fuel_scan) i32.eqz br_if $normalized

        ;; inspect the next word
        (i64.load $input (global.get $iptr))
        (i64.shr_u (global.get $word_max) (global.get $ibit))
        global.get $word_max i64.xor i64.or
        local.set $crumbs
        (call $dbg (i32.const 0x2324) (local.get $crumbs))
        ;; (call $dbg (i32.const 0x2000) (local.get $leadingZeroBits))

        local.get $leadingZeroBits
        local.get $crumbs
        call $redexes
        local.tee $redexes
        (call $dbg (i32.const 0x122) (local.get $redexes))
        i64.eqz i32.eqz
        br_if $found-redex

        (if
          (i32.lt_u (global.get $iptr) (global.get $sptr))
          (then
            ;; onto the next whole word
            local.get $crumbs
            i64.ctz
            local.set $leadingZeroBits
            ;; (call $dbg (i32.const 0x565) (global.get $sbit))
            global.get $word_size
            call $slowCopySubWord
            br $find-redex
          )
          (else
            ;; (call $dbg (i32.const 0x555) (global.get $sbit))
            global.get $sbit
            call $slowCopySubWord
            br $normalized
          )
        )
      )
      end $found-redex

      local.get $redexes
      call $redex
      local.set $shift
      local.set $arity

      ;; 2 * $arity
      (local.set $arity2
        (i64.shl (i64.extend_i32_u (local.get $arity)) (i64.const 1)))

      ;; edge case: we found a redex beyond the bounds. this is not "supposed"
      ;; to happen (if the rest of the word is zeroed out), but with all of the
      ;; full-word copies we use, we end up with junk there.
      (if
        (i32.and
          (i32.ge_u (global.get $iptr) (global.get $sptr))
          (i64.ge_s (i64.add (local.get $shift) (local.get $arity2)) (global.get $sbit))
        )
        (then
          ;; copy to the end
          global.get $sbit
          global.get $ibit
          i64.sub
          call $slowCopySubWord
          br $normalized
        )
      )

      ;; (call $dbg (i32.const 0x12121212) (local.get $arity2))
      (i64.sub (local.get $shift) (local.get $arity2))
      local.tee $tmp
      i64.const 0
      i64.gt_s
      (if (then
        ;; (call $dbg (i32.const 0xb0a0a0) (local.get $tmp))
        ;; copy up to the start of the redex
        local.get $tmp
        call $slowCopySubWord ;;
        ;; scan over the head of the redex
        (global.set $ibit (i64.add (local.get $shift) (i64.const 2)))
      ) (else
        ;; roll back $obit to overwrite the the zeros
        ;; (call $dbg (i32.const 0xb0a000) (local.get $arity2))
        ;; (call $dbg (i32.const 0xb0a010) (global.get $obit))
        (global.set $obit (i64.sub (global.get $obit) (local.get $arity2)))
        ;; (call $dbg (i32.const 0xb0a020) (global.get $obit))
        ;; focus the crumb after the redex head
        (global.set $ibit (i64.add (local.get $shift) (i64.const 2)))
      ))

      ;; if we overrun, we need to be able to revert
      block $revert
      (local.set $riptr (global.get $iptr))
      (local.set $ribit (global.get $ibit))
      (local.set $roptr (global.get $optr))
      (local.set $robit (global.get $obit))

      ;; (call $dbg (i32.or (i32.const 0xCDB00000) (global.get $iptr)) (global.get $ibit))
      ;; (call $dbg (i32.or (i32.const 0xCDC00000) (global.get $iptr)) (local.get $shift))

      ;; (call $dbg (local.get $arity) (i64.const 0x999))

      block $cases block $S block $K block $I block $None
      local.get $arity
      br_table $None $I $K $S $None
      end $None unreachable

      ;; Ix = x
      end $I
      call $slowCopy1
      (br_if $revert (i64.eq (global.get $ibit) (i64.const -1)))
      br $cases

      ;; Kxy = x
      end $K
      call $slowCopy1
      (br_if $revert (i64.eq (global.get $ibit) (i64.const -1)))
      call $slowScan1
      (br_if $revert (i64.eq (global.get $ibit) (i64.const -1)))
      br $cases

      ;; Sxyz = 00xz0yz
      end $S
      call $writeZero
      call $writeZero
      call $slowCopy1 ;; x
      (br_if $revert (i64.eq (global.get $ibit) (i64.const -1)))
      ;; save the location of y
      (local.set $yptr (global.get $iptr))
      (local.set $ybit (global.get $ibit))
      call $slowScan1 ;; y (skipping)
      (br_if $revert (i64.eq (global.get $ibit) (i64.const -1)))
      call $slowCopy1 ;; z
      (br_if $revert (i64.eq (global.get $ibit) (i64.const -1)))
      call $writeZero
      ;; restore the location of y
      (global.set $iptr (local.get $yptr))
      (global.set $ibit (local.get $ybit))
      call $slowCopy1 ;; y, for real
      (br_if $revert (i64.eq (global.get $ibit) (i64.const -1)))
      call $slowCopy1 ;; z, again
      (br_if $revert (i64.eq (global.get $ibit) (i64.const -1)))
      br $cases

      end $cases

      (call $dbg (i32.add (i32.const 0x666000) (global.get $iptr)) (global.get $ibit))
      (loop $copy-rest
        (if
          (i32.lt_u (global.get $iptr) (global.get $sptr))
          (then
            (call $dbg (i32.add (i32.const 0x767000) (global.get $iptr)) (global.get $sbit))
            global.get $word_size
            global.get $ibit
            i64.sub
            call $slowCopySubWord
            br $copy-rest
          )
          (else
            (call $dbg (i32.add (i32.const 0x676000) (global.get $iptr)) (global.get $sbit))
            global.get $sbit
            global.get $ibit
            i64.sub
            call $slowCopySubWord
          )
        )
      )
      (call $dbg (i32.add (i32.const 0x777000) (global.get $iptr)) (global.get $ibit))

      (memory.copy $input $output (i32.const 0) (i32.const 0) (i32.const 65536))
      (global.set $sptr (global.get $optr))
      (global.set $sbit (global.get $obit))
      br $until-normalized

      end $revert
      (global.set $iptr (local.get $riptr))
      (global.set $ibit (local.get $ribit))
      (global.set $optr (local.get $roptr))
      (global.set $obit (local.get $robit))
      ;; write the zeros back out
      (loop $arities
        call $writeZero
        (local.tee $arity (i32.sub (local.get $arity) (i32.const 1)))
        br_if $arities
      )
      ;; write the head back out
      (global.set $ibit (i64.sub (local.get $ribit) (i64.const 2)))
      i64.const 2
      call $slowCopySubWord
      ;; where we will start looking for reductions from again
      (global.set $ibit (local.get $ribit))
      br $reverted-redex end $reverted-redex
    )
    end $normalized

    (i64.or
      (i64.shr_u (global.get $obit) (i64.const 1))
      (i64.shl (i64.extend_i32_u (global.get $optr)) (i64.const 2))
    )
  )

  (func $slowScan1 (export "slowScan1")
    (local $_optr i32)
    (local $_obit i64)
    (local.set $_optr (global.get $optr))
    (local.set $_obit (global.get $obit))

    call $slowCopy1

    (global.set $optr (local.get $_optr))
    (global.set $obit (local.get $_obit))
  )

  ;; copy one expression from input to output
  (func $slowCopy1 (export "slowCopy1")
    (local $exp i64)
    (local $crumbs i64)
    (local $tmp i64)
    ;; adjust the expected number of nodes by the leading nonzeros *crumbs*
    ;; we will add (could use leading zeros, but this seems more robust to
    ;; optimizations in $reachesZero)
    (local.set $exp (i64.add (i64.const 1) (i64.shr_u (global.get $ibit) (i64.const 1))))
    (call $dbg (i32.const 0xAAAA) (global.get $ibit))
    ;; (call $dbg (i32.const 0xAAAB) (local.get $exp))

    (loop $de-loop
      (if (i32.eqz (global.get $fuel_copy)) (then return))
      (global.set $fuel_copy (i32.sub (global.get $fuel_copy) (i32.const 1)))
      ;; (call $dbg (i32.const 0xAAAC) (local.get $exp))

      local.get $exp

      (i64.load $input (global.get $iptr))
      (i64.shr_u (global.get $word_max) (global.get $ibit))
      ;; leading nonzeros
      global.get $word_max i64.xor i64.or
      local.tee $crumbs
      (call $dbg (i32.const 0xACCC) (local.get $crumbs))

      call $reachesZero
      local.tee $tmp
      i64.eqz i32.eqz ;; coerce to boolean
      (if
        (then ;; copy that many bits, then return
          local.get $tmp
          global.get $ibit
          i64.sub
          call $slowCopySubWord
          return
        )
        (else ;; copy the rest of the word, advance
          global.get $word_size
          global.get $ibit
          i64.sub
          call $slowCopySubWord ;; advances $iobit and $ioptr
          ;; exp -= deltaExpecting(crumbs)
          local.get $exp
          local.get $crumbs
          call $deltaExpecting
          i64.sub
          local.set $exp
          (if
            (i32.or
              (i32.lt_u
                (global.get $iptr)
                (global.get $sptr)
              )
              (i64.lt_u
                (global.get $ibit)
                (global.get $sbit)
              )
            )
            (then br $de-loop)
            (else
              ;; signal our error condition
              (global.set $ibit (i64.const -1))
              return
              ;; assert iptr == sptr && ibit == sbit || iptr == sptr+1 && ibit == 0
              ;; (global.set $iptr (global.get $sptr))
              ;; (if
              ;;   (i64.eqz (global.get $ibit))
              ;;   (then
              ;;     ;; overshot
              ;;     (local.set $tmp (i64.sub (i64.const 64) (global.get $sbit)))
              ;;     (if
              ;;       (i64.gt_u
              ;;         (global.get $obit)
              ;;         (local.get $tmp)
              ;;       )
              ;;       ;; not across a word boundary
              ;;       (then (global.set $obit (i64.sub (global.get $obit) (local.get $tmp))))
              ;;       ;; across a word boundary
              ;;       (else
              ;;         (global.set $optr (i32.sub (global.get $optr) (i32.const 1)))
              ;;         (global.set $obit (i64.sub (i64.add (i64.const 64) (global.get $obit)) (local.get $tmp)))
              ;;       )
              ;;     )
              ;;     (global.set $ibit (i64.const 0))
              ;;   )
              ;; )
              ;; return
            )
          )
        )
      )
    )
  )


  (func $writeZero (export "writeZero")
    (local $tmp i64)
    (local $dbg i64)

    (if (i64.eqz (global.get $obit))
      (then
        (global.get $optr)
        (i64.const 0)
        i64.store $output
      )
      (else
        (global.get $optr)
          (i64.load $output (global.get $optr))
          (i64.shl (global.get $word_max) (i64.sub (global.get $word_size) (global.get $obit)))
          i64.and
        ;; local.tee $dbg (call $dbg (i32.const 0xDD00) (local.get $dbg))
        i64.store $output
      )
    )

    global.get $obit
    i64.const 2
    i64.add
    local.tee $tmp
    global.get $word_size
    i64.ge_u
    (if (then
      global.get $optr
      i32.const 8
      i32.add
      global.set $optr
    ))
    local.get $tmp
    global.get $word_size
    i64.rem_u
    global.set $obit
  )

  ;; copy $bits within one input word to output (possibly across output word
  ;; boundaries, not input word boundaries). always copies a full word, but
  ;; advances the pointers by $bits.
  (func $slowCopySubWord (export "slowCopySubWord") (param $bits i64)
    (local $tmp i64)
    (local $offset i64)
    (local $dbg i64)
    (local $_iptr i32)
    (local $_ibit i64)
    (local $_optr i32)
    (local $_obit i64)
    (local.set $_iptr (global.get $iptr))
    (local.set $_ibit (global.get $ibit))
    (local.set $_optr (global.get $optr))
    (local.set $_obit (global.get $obit))

    (if (i64.eqz (local.get $bits)) (then return))

    (global.set $fuel_copy (i32.sub (global.get $fuel_copy) (i32.const 1)))
    (if (i32.eqz (global.get $fuel_copy)) (then return))

    (local.set $offset (i64.sub (global.get $obit) (global.get $ibit)))
    ;; (call $dbg (i32.const 0x1000) (local.get $offset))
    ;; (call $dbg (i32.const 0x2000) (i64.load $input (global.get $iptr)))
    ;; (call $dbg (i32.const 0xC100) (i64.extend_i32_u (global.get $iptr)))
    ;; (call $dbg (i32.const 0xC200) (global.get $ibit))
    ;; (call $dbg (i32.const 0xC300) (local.get $bits))

    ;; mask off input (low bits)
    (i64.load $input (global.get $iptr))
    (i64.shr_u (global.get $word_max) (global.get $ibit))
    ;; local.tee $dbg (call $dbg (i32.const 0xAA000) (local.get $dbg))
    i64.and
    ;; local.tee $dbg (call $dbg (i32.const 0xA000) (local.get $dbg))
    local.set $tmp
    local.get $offset
    i64.const 0
    i64.ge_s
    (if
      (then
        local.get $tmp
        local.get $offset
        i64.shr_u
        local.set $tmp
      )
      (else
        local.get $tmp
        i64.const 0
        local.get $offset
        i64.sub
        i64.shl
        local.set $tmp
      )
    )
    local.get $tmp
    ;; local.tee $dbg (call $dbg (i32.const 0xB000) (local.get $dbg))

    ;; mask off output
    (i64.load $output (global.get $optr))
    (i64.shr_u (global.get $word_max) (global.get $obit))
    (i64.xor (global.get $word_max))
    ;; local.tee $dbg (call $dbg (i32.const 0xCC000) (local.get $dbg))
    i64.and
    ;; local.tee $dbg (call $dbg (i32.const 0xC000) (local.get $dbg))

    ;; join and save
    i64.or
    ;; local.tee $dbg (call $dbg (i32.const 0xD000) (local.get $dbg))
    local.set $tmp
    (i64.store $output (global.get $optr) (local.get $tmp))

    ;; if we are across an output word boundary:
    local.get $offset
    i64.const 0
    i64.gt_s
    (if
      (then
        ;; mask off input (high bits)
        (i64.load $input (global.get $iptr))
        i64.const 64
        local.get $offset
        i64.sub
        i64.shl
        ;; local.tee $dbg (call $dbg (i32.const 0xE000) (local.get $dbg))
        ;; save
        local.set $tmp
        (i64.store $output (i32.add (i32.const 8) (global.get $optr)) (local.get $tmp))
      )
    )

    ;; increment pointers
    global.get $ibit
    local.get $bits
    i64.add
    local.tee $tmp
    global.get $word_size
    i64.ge_u
    (if (then
      global.get $iptr
      i32.const 8
      i32.add
      global.set $iptr
    ))
    local.get $tmp
    global.get $word_size
    i64.rem_u
    global.set $ibit

    global.get $obit
    local.get $bits
    i64.add
    local.tee $tmp
    global.get $word_size
    i64.ge_u
    (if (then
      global.get $optr
      i32.const 8
      i32.add
      global.set $optr
    ))
    local.get $tmp
    global.get $word_size
    i64.rem_u
    global.set $obit
  )
)

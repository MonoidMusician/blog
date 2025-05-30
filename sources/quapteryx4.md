---
title: "Efficient Quapteryx Evaluator"
subtitle: "Quapteryx Part IV"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/05/11
---

:::centered
_Continued from [Quapteryx Part III: Quaternary Combinators](quapteryx3.html), where I was writing WASM by hand and then ported it to C and got it working._
:::

The next step was rewriting it from scratch for an efficient evaluator, and this is where I realized that having C structs and pointers (and malloc!) was really what I needed.
I used [walloc.c](https://wingolog.org/archives/2020/10/13/malloc-as-a-service) for the malloc implementation, also to keep it lightweight and WASMy.

Thus the stack is clang + LLVM + walloc.c + WASM builtins + a JS wrapper to run it.

## Evaluation in Practice

Letʼs talk about what this new evaluator looks like.

### Operand datatypes

The meaning (the denotation) of an operand is always given by the crumbstring it represents, even though we use thunking and other representations for efficiency.

Specifically an operand may be a singular word (<32 crumbs, since a well-formed operand is always an odd number of crumbs), called an immediate operand.
If it is not an immediate operand, it is a shared operand, which are our version of thunks.

These thunks are managed with ref counting: ref counting fits very well for this application^[as opposed to garbage collectors] because references are acyclic, thunks only ever form a [DAG]{t=}, not a graph with cycles.^[This can be seen in the fact that each operand represents a crumbstring, which is a finite data structure, and introducing sharing between equal substrings does not introduce cycles!]

Shared operands come in two flavors: arbitrary-length crumbstrings allocated on the heap, and synthetic application nodes created during evaluation (to replace tthe burden of copying crumbstrings with mere pointer references (and refcounting)).

```C
​////////////////////////////////////////////////////////////////////////////////
// Data structures for operands: immediate or shared (heap or synthetic)      //
////////////////////////////////////////////////////////////////////////////////

// An operand is either shared (heap or synthetic), or an inline word (immediate).
typedef struct operand {
  union {
    struct shared_operand* shared;
    word crumbs;
  };
  bool immediate; // false: shared, true: crumbs
} operand;

// A shared operand is ref-counted and used for sharing work from lazy evaluation.
// The pointer it stores is updated after it is evaluated, and `work` increased.
typedef struct shared_operand {
  int ref_count;
  word work;
  union {
    struct synthetic_operand* synth;
    struct heap_operand* heap;
  };
  bool on_heap; // false: synth, true: heap
  // These bound indices of weakrefs `was_evaling` on the stack, so they can
  // be zeroed out when this `shared_operand` is freed. If they are already
  // zeroed out or otherwise replaced, this is fine!
  int weakref_stack_min;
  int weakref_stack_max;
} shared_operand;

// An operand on the heap. As the `shared_operand` that owns this `heap_operand`
// is evaluated, it will (eventually) free this `heap_operand` and replace
// it with a new one so that the evaluation is shared.
//
// Terms that are immediate but too large to fit into a word also end up
// as a `heap_operand`.
typedef struct heap_operand {
  int length; // length in words
  word crumbstring[];
} heap_operand;

// A synthetic apply node, produced during evaluation of `S`. It counts as a
// single reference to its operands while it is alive. It represents the
// crumbstring `0(fun)(arg)`.
typedef struct synthetic_operand {
  operand fun;
  operand arg;
} synthetic_operand;
```

:::{.Details box-name="Memory management"}
Each shared operand owns its heap or synthetic application node.

In rare circumstances, when a thunk is evaluated, they can be transferred from one shared operand to another, but generally this does not happen: they stay with their shared operand.
:::

So, in my view, one of the most important functions is `write_out(operand op)`{.c}: it gives us the *denotation* – its meaning as a crumbstring.

```C
// Write out an operand into the output buffer. This is an important operation
// because it gives the semantics of operands as crumbstrings! ^^
void write_out(const operand op) {
  goodptrbit(output_at);
  if (op.immediate) {
    const u8 len = reachesZero(1, op.crumbs);
    copy_to_unaligned(from_ptr(output_words, output_at), &op.crumbs, output_at.bit, 1, false);
    output_at = ptrbit_incr_bits(output_at, len);
    drop(op);
  } else if (op.shared->on_heap) {
    const word len = bitlength(op.shared->heap->crumbstring, op.shared->heap->length);
    copy_to_unaligned(from_ptr(output_words, output_at), op.shared->heap->crumbstring, output_at.bit, len % word_size, false);
    output_at = ptrbit_incr_bits(output_at, len);
    drop(op);
  } else {
    // Write a zero crumb `0q0` (application)
    // (we may have written other bits to the word already, from hapless copying)
    word_of(output_words, output_at) &= mask_hi(output_at.bit);
    output_at = ptrbit_incr_bits(output_at, 2);
    // Write each operand
    write_out(op.shared->synth->fun);
    write_out(op.shared->synth->arg);
    drop(op);
  }
}
```

Thatʼs, uh … thatʼs kind of ugly. Thanks, C!

Letʼs try this again, in [pseudo-Haskell](https://www.explainxkcd.com/wiki/index.php/1312:_Haskell).

```haskell
data Operand
  = Immediate Word
  | Shared (IORef Shared)
data Shared
  = OnHeap [Word]
  | Synthetic (Operand, Operand)

operandToCrumbs :: Operand -> IO Text
operandToCrumbs (Immediate crumbs) = do
  pure (wordToCrumbs crumbs)
operandToCrumbs (Shared ref) = do
  lastEvaluatedTo <- readIORef ref
  case lastEvaluatedTo of
    OnHeap crumbstring -> do
      pure (wordsToCrumbs crumbstring)
    Synthetic (fun, arg) -> do
      pure "0"
        <> operandToCrumbs fun
        <> operandToCrumbs arg

wordsToCrumbs :: [Word] -> Text
wordsToCrumbs = go 1
  where
    go exp [] = undefined
    go exp [final] =
      wordToCrumbs (Just (reachesZero exp final)) final
    go exp (head : tail) =
      wordToCrumbs Nothing head
        <> go (exp - deltaExpecting head) tail

wordToCrumbs :: Maybe Int -> Word -> Text
-- Convert the full word to text, 32 crumbs
wordToCrumbs Nothing word =
  let
    shown = showIntAtBase 4 showInt word
    width = (bitSize word) / 2
  in
    replicate (width - length shown) "0" <> shown
-- Truncate to the leading crumbs of the word
wordToCrumbs (Just numberOfCrumbs) word =
  take numberOfCrumbs (wordToCrumbs Nothing word)
```

### Stack

The role of the stack is simple: it queues arguments (in whatever form they show up in).

This is exactly the metaphorical stack that weʼve talked about in the grammar of Ojo and now Quaternary Combinators – reified as a data structure now.

Semantically speaking, the stack always represents a function application: the top of the stack is applied to all the remaining items: `fun arg_1 arg_2 ... arg_n`, which needs to be left-nested in binary applications like `(((fun arg_1) arg2) ...) arg_n`.

However, the stack is also where we need to keep track of the thunks that we are evaluating.

```C
////////////////////////////////////////////////////////////////////////////////
// Stack, for evaluation                                                      //
////////////////////////////////////////////////////////////////////////////////

#define SZ 1024

// When you evaluate an application node `0xy`, `y` goes onto the stack (first)
// and `x` is the new top of stack. Reaching `I`, `K`, or `S` will pop from the
// stack and push back onto it: `Sxyz = xz(yz)` will create a synthetic operand
// for its last value `(yz)` in doing so.
//
// The interpreter wants to maintain one thing on the stack (mostly to keep
// the `was_evaling` pointer for simplicity). If it reaches zero for real, that
// means it is time to stop evaluation: then the value of `was_evaluting` would
// theoretically be the whole input/output node.
struct stack_frame {
  operand to_eval;
  shared_operand* was_evaling; // weak ref! must be zeroed out when freed
} stack[SZ];
u32 stack_top = 0;
u32 stack_endstop = 0; // used in whnf_op

// Create a new stack frame, without `was_evaling`.
#define FRAME(value) ((struct stack_frame) { .to_eval = (value) })
```

You should think of this stack as an interleaving of `was_evaling`{.c} with `to_eval`{.c}.

```C
// stack[stack_top - 1].was_evaling should always be NULL
stack[stack_top - 1].was_evaling = NULL;
// since we *are* evaluating the top argument
stack[stack_top - 1].to_eval // the head of the expression

// Once it is a lone combinator, its value goes in
stack[stack_top - 2].was_evaling
  // (i.e. WHNF_I0, WHNF_K0, WHNF_S0)
// And then you have this argument
stack[stack_top - 2].to_eval

// Their synthetic application goes into
stack[stack_top - 3].was_evaling
  // (i.e. WHNF_K1, WHNF_S1)
// And the next argument
stack[stack_top - 3].to_eval

// The last thunk for the head is
stack[stack_top - 4].was_evaling
  // (i.e. WHNF_S2)
// And the last argument immediately available to `S`
stack[stack_top - 4].to_eval

// Then the rest of the stack is merely pending,
// waiting for the head to reduce enough to
// continue on with them, if it ever does!
```

So the goal of the [WHNF]{t=} evaluator is to unpack all function applications, saving their arguments onto the stack along the way, in order to get down to a lone combinator on the left: this is the first nonzero digit in the crumbstring.

Then the [WHNF]{t=} evaluator keeps performing reductions on the top of this stack.
While the top operand is `1`{.st} \(I\), `2`{.st} \(K\), or `3`{.st} \(S\), it can perform a reduction.
Otherwise it needs to unpack more applications: save *those* arguments to the stack, and recurse into the function side.

When the stack underflows, it needs to parse a new operand from the input buffer, advancing its state.
^[You could just pre-scan the whole input and put it onto the stack, reversed, but that seems less elegant to me than lazily consuming the input.]
If the input buffer is already at the end, the [WHNF]{t=} evaluation is complete: it is in Weak Head Normal Form!

In this state, the top of the stack is a combinator `1`{.st} \(I\), `2`{.st} \(K\), or `3`{.st} \(S\), and the total stack size (including this combinator) is less than or equal to its digit value.
The [WHNF]{t=} expression can be then read off of the stack like this.

Note that this is not a complete normal form: just because reductions stopped at the head does not mean that every operand is normalized – the rest of the stack can still be arbitrary operands!

So a [NF]{t=} evaluator needs to restart [WHNF]{t=} evaluation for each sub-expression, track their results, and re-assemble the tree.
However, once each subexpression from left-to-right stops in [WHNF]{t=}, then its *head* can be written out to the output buffer: that portion of the crumbstring is already set in stone.

### Input and output

I already alluded to input and output buffers earlier.

They are not too complicated, just long crumbstrings that maintain some state:

```C
ptrbit input_at = {};
ptrbit stop_at = {};
ptrbit output_at = {};

// read once
word input_words[SZ];
// write once
word output_words[SZ];
```

I just think it is neat that this evaluation algorithm is “streaming”, in the sense that it reads each crumb of the input buffer once when it is otherwise stuck on evaluations, and it writes crumbs to the output buffer once when it really *is* stuck on evaluations.

### Beta Reductions

This is the core of the evaluation algorithm: beta reductions for each combinator.

More special cases could be added for other combinators, like \(C\) and \(B\) and so on!

<details class="Details">
<summary>Code</summary>

```C
// Perform a reduction according to whatever the head combinator is.
//
// `Ix = x`
// `Kxy = x`
// `Sxyz = 00xz0yz = ((xz)(yz))`
static bool reduction_rule(word crumbs) {
  const u8 crumb = crumbs >> (word_size - 2);
  assert(crumb > 0);
  assert(crumb <= 3);

  // This is the procedure to pop an item from the stack: if the stack is
  // empty, we try to refill it from the input buffer, and if we cannot, then
  // we have to return. If it succeeded, we save it to the given variable.
  #define POP_OR(lvalue, stmt) BLOCK( \
    assert(stack[stack_top].was_evaling == NULL); \
    if (stack_top <= stack_endstop && !refill()) { \
      stmt; __builtin_trap(); \
    }; \
    lvalue = stack[--stack_top].to_eval; \
    assert(stack_top >= stack_endstop); \
    goodop(stack[stack_top].to_eval); \
  )
  // Now we handle the particular opcodes: I=1, K=2, S=3
  if (crumb == 1) {
    // Check that there is an argument available; otherwise, we need to restore
    // the head operand back to the stack
    if (stack_top <= stack_endstop && !refill()) { stack_top += 1; return false; }

    // Just drop this identity node `op`, good riddance
    // (Morally we pop the argument and push it back onto the stack.)
    work++;
    return true;
  } else if (crumb == 2) {
    // Get two arguments
    operand x, y;
    POP_OR(x, { stack_top += 1; return false; });
    // TODO: tell `refill()` that it does not need to actually copy this operand?
    POP_OR(y, { stack_top += 2; return false; });

    // Drop `y` and put `x` back onto the stack: `Kxy = x`
    drop(y);
    stack[stack_top++] = FRAME(x);
    assert(stack_top < SZ);
    work++;
    return true;
  } else if (crumb == 3) {
    // Get three arguments
    operand x, y, z;
    POP_OR(x, { stack_top += 1; return false; });
    POP_OR(y, { stack_top += 2; return false; });
    POP_OR(z, { stack_top += 3; return false; });

    // `Sxyz = 00xz0yz = ((xz)(yz))`
    // Increase the ref count on `z`
    z = dup(z);
    // Put `x`, `z`, and the synthetic `(yz)` on the stack
    operand yz = apply(y, z);
    stack[stack_top++] = FRAME(yz);
    stack[stack_top++] = FRAME(z);
    stack[stack_top++] = FRAME(x);
    assert(stack_top < SZ);
    work++;
    // Evaluation will continue with `x` on the next `step()`!
    return true;
  }
}
```

</details>

### Unpacking applications

On the other hand, sometimes we have to unpack the synthetic applications we created while reducing \(S\):

<details class="Details">
<summary>Code</summary>

```C
// Synthetic application nodes represent an argument to go on the stack, and
// the function, which is the new top of the stack. We can recursively unpack
// synth nodes until the arguments are all on the stack and the last function
// is a heap or immediate operand.
operand unpack_synth_onto_stack(const operand op) {
  // We might as well walk the spine of synthetic applications here in a tight
  // loop, instead of waiting for the next `step();`
  operand spine = op;
  // #pragma unfold(2)
  while (!spine.immediate && !spine.shared->on_heap) {
    shared_operand* this = spine.shared;
    if (stack_top > stack_endstop && this->ref_count > 1) {
      stack_was_evaling(stack_top - 1, this);
    }
    // Now we add it onto the stack
    stack[stack_top++] = FRAME(soft_dup(this->synth->arg));
    // And proceed down the spine
    spine = this->synth->fun;
  }
  // Finally, whatever we ended up with is at the top of the stack.
  const operand result = soft_dup(spine);
  // And free the spine (all the leaves were dup'ed, so are safe)
  drop(op);
  return result;
}
```

</details>

A little more complicated, but still the same idea, is unpacking a crumbstring from a shared heap value.

<details class="Details">
<summary>Code</summary>

```C
// A heap or immediate operand is unpacked onto the stack: the number of leading
// zeros in the crumbstring indicates the number of arguments that will be
// unpacked, and the first crumb is the combinator that will control the
// reduction.
operand unpack_crumbstring_onto_stack(operand op) {
  if (op.immediate) {
    if (op.crumbs >> (word_size - 2)) return op;
    op = shared(as_tmp(op));
  }
  assert(!op.immediate && op.shared->on_heap);

  // Cache the number of bytes
  const word bytes = sizeof(word) * op.shared->heap->length;
  // Here is the crumbstring
  const word* crumbstring = op.shared->heap->crumbstring;
  // Start at the first crumb
  ptrbit i = (ptrbit) {};

  // Count the leading zero crumbs
  {
    // Whole words
    while (i.ptr < bytes && word_of(crumbstring, i) == 0) {
      i.ptr += sizeof(word);
    }
    // Remaining crumbs
    i.bit = clz(word_of(crumbstring, i)) & ~1;
  }

  // This is now our arity
  const word arity = ptrbit2crumbs(i);
  assert(arity);

  // Increase stack_top already, and then we will walk back and fill in the
  // arguments on the stack.
  stack_top += arity + 1;
  for (int stacked = 1; stacked <= arity + 1; stacked++) {
    // Scan ahead one item
    const struct ptrbitop scanned = scan1op(crumbstring, i, (ptrbit) { .ptr = bytes, .bit = 0 });
    i = scanned.next;
    stack[stack_top - stacked] = FRAME(scanned.op);
  }

  // This is now consumed
  drop(op);

  // Return the head of the applications
  return stack[--stack_top].to_eval;
}
```

</details>

### Handling evaluated thunks

<details class="Details">
<summary>Code</summary>

```C
// Now that the head is a known combinator, the underapplied nodes formed by
// it and its first couple arguments on the stack (not meeting its arity) are
// in Weak Head Normal Form, so we need to update `was_evaling` for them.
static void save_whnf_on_stack(word crumbs) {
  const u8 crumb = crumbs >> (word_size - 2);
  assert(crumb > 0);
  assert(crumb <= 3);

  // Scan the stack to see how many we need to save, up to one less than
  // the arity of this node (since we are evaling it applied to the arity)
  u8 was_evaled = 0;
  for (u8 i=1; i <= stack_top - stack_endstop && i < crumb; i++) {
    if (stack[stack_top - i].was_evaling != NULL) {
      // If ref_count was zero, it should have been freed and `was_evaling`
      // removed from the stack
      assert(stack[stack_top - i].was_evaling->ref_count > 0);
      was_evaled = i;
    }
  }
  if (!was_evaled) return;

  operand evaled = direct(crumbs);
  // Now we walk it and build up the WHNF term as we go
  for (u8 i=1; i <= was_evaled; i++) {
    if (stack[stack_top - i].was_evaling != NULL) {
      // Shared, so we want to swap out what we computed to be its
      // WHNF value `evaled` for whatever it has currently. And we
      // snowball from there.
      //
      // If `evaled` has a heap, for example, then ownership of it is
      // transferred to `was_evaling`. Whatever `was_evaling` has
      // is deleted, unless it is a small heap that can be reused
      // by an immediate `evaled`.
      evaled = have_evaled(stack[stack_top - i].was_evaling, evaled);
      stack[stack_top - i].was_evaling = NULL;
    }
    // Create a new apply node only if it will be transferred, otherwise
    // it is just a leak/immediately dropped.
    if (i < was_evaled) {
      evaled = apply(evaled, stack[stack_top - i].to_eval);
    } else {
      break;
    }
  }
}
```

</details>

### Altogether

<details class="Details">
<summary>Code</summary>

```C
// Evaluate one operand at the top of the stack. If it is a known combinator
// `S`, `K`, or `I` then we can apply it and compute a reduction (and if it
// ends up being underapplied and thus we are in WHNF). If not, it is an
// application node that we have to unpack onto the stack and recurse into the
// function side to continue evaluating on the next `step();`
bool step(void) {
  if (!stack_top && !refill()) return false; // all done

  operand op = stack[--stack_top].to_eval;
  assert(stack[stack_top].was_evaling == NULL);

  // Do some setup for shared nodes
  if (!op.immediate) {
    // If it is `S`, `K`, or `I` that ended up on the heap for some reason
    // (e.g. because it was shared at one point), unpack it.
    if (
      op.shared->on_heap &&
        (op.shared->heap->crumbstring[0] >> (word_size - 2)) != 0
    ) {
      const word crumb = op.shared->heap->crumbstring[0];
      drop(op);
      // We set it back on the stack in the case that we are out of input crumbs
      // and are thus in WHNF, so we revert the stack to its original size before
      // this function was called
      stack[stack_top].to_eval = op = direct(crumb);
    }

    // Try to keep at least one thing on the stack to make sure we have
    // somewhere to pin `was_evaled`. But if we ran out of input, that is fine:
    // it is the final node to evaluate anyways.
    else if (op.shared->ref_count > 1 && (stack_top <= stack_endstop && refill())) {
      stack_was_evaling(stack_top - 1, op.shared);
    }
  }

  // Walk down the tree of synth nodes until we end up at a heap or immediate
  op = unpack_synth_onto_stack(op);

  assert(op.immediate || op.shared->on_heap);
  stack[stack_top].to_eval = op;

  // Unpack applications from a crumbstring, leaving the combinator at the head
  op = unpack_crumbstring_onto_stack(op);

  // What is left should be a bare combinator
  assert(op.immediate);
  const u8 crumb = op.crumbs >> (word_size - 2);
  assert(crumb > 0);
  assert(crumb <= 3);

  // The first `0 .. crumb-1` applications are now evaluated to WHNF, so their
  // evaluations on the stack need to be handled
  save_whnf_on_stack(op.crumbs);

  // Finally we apply the reduction indicated by the combinator, I=1, K=2, S=3
  return reduction_rule(op.crumbs);
}
```

</details>

## Appendix: Links

- https://en.wikipedia.org/wiki/Iota_and_Jot
- https://en.wikipedia.org/wiki/SKI_combinator_calculus#SKI_expressions
- https://crypto.stanford.edu/~blynn/lambda/cl.html
- https://crypto.stanford.edu/~blynn/lambda/crazyl.html
- https://web.archive.org/web/20160823182917/http://semarch.linguistics.fas.nyu.edu/barker/Iota
- https://doisinkidney.com/posts/2020-10-17-ski.html
- https://www.angelfire.com/tx4/cus/combinator/birds.html
- https://cs.stackexchange.com/questions/57361/combinator-equivalent-to-eta-conversion#57371
- https://olydis.medium.com/one-point-bases-for-%CE%BB-calculus-4163b1b326ad
- http://www.chriswarbo.net/blog/2024-05-10-sk_logic_in_egglog_4.html
- https://okmij.org/ftp/tagless-final/ski.pdf
- https://cs.stackexchange.com/questions/57476/what-functions-can-combinator-calculus-expressions-compute


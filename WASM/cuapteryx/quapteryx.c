#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include "cuapteryx.h"

void *malloc(size_t size) __attribute__((warn_unused_result, alloc_size(1), noinline));
void free(void *p) __attribute__((noinline));

#define ROUNDUPDIV(num, denom) ((num + (denom - 1)) / denom)

word mask_hi(u8 hi_bits) {
  return ~(word_max >> hi_bits);
}
word mask_lo(u8 lo_bits) {
  return ~(word_max << lo_bits);
}
word word_cat(u8 hi_bits, word hi, u8 lo_bits, word lo) {
  assert(hi_bits + lo_bits <= word_size);
  return (hi & mask_hi(hi_bits)) | ((lo & mask_hi(lo_bits)) >> hi_bits);
}

// Copy all words of the source `src[bits: 0 : word_size*words]` to the
// destination at some positive offset, zeroing the first word if `clobber` is
// set (otherwise preserving those first `offset` bits):
// ```
// if (clobber) dst[words: 0] = (word) 0;
// dst[bits: offset : offset + 64*words] =
//   src[bits: 0 : 64*words]
// ```
void copy_to_unaligned(word* dst, const word* src, u8 offset, word words, bool clobber) {
  if (words == 0) return;
  assert(offset < word_size);
  if ((offset % 8) == 0) {
    __builtin_memcpy(dst + (offset / 8), src, sizeof(word) * words);
    return;
  }
  word hanging = clobber ? 0 : *dst;
  while (words--) {
    word tmp = words ? *src : 0;
    *dst = word_cat(offset, hanging, word_size - offset, tmp);
    hanging = tmp << offset;
    dst++;
    src++;
  }
}
void copy_from_unaligned(word* dst, const word* src, u8 offset, word words) {
  if (words == 0) return;
  assert(offset < word_size);
  if ((offset % 8) == 0) {
    __builtin_memcpy(dst, src + (offset / 8), sizeof(word) * words - (offset / 8));
    return;
  }
  word last = *src;
  while (words--) {
    word this = (last << offset);
    if (words) {
      last = *++src;
      *dst++ = this | (last >> (word_size - offset));
    } else {
      *dst = this;
    }
  }
}

// Calculate the length of the passed crumbstring, which is more efficient if
// `known_words != 0` is passed (otherwise it has to examine each crumb).
ptrbit ptrbitlength(const word* crumbstring, word known_words) {
  word i = 0; word exp = 1; u8 offset;
  if (known_words) {
    // Stop one word early, just in case.
    while (i < known_words - 2) {
      exp -= deltaExpecting(crumbstring[i]);
      assert(exp != 0);
      i++;
    }
    offset = reachesZero(exp, crumbstring[i]);
  }
  // Fall through, in case `known_words - 1` was too short.
  // (This is also used in `heap_cat`.)
  if (!offset) {
    do {
      offset = reachesZero(exp, crumbstring[i]);
      if (offset) {
        known_words = i + 1;
      } else {
        exp += deltaExpecting(crumbstring[i]);
      }
    } while (!offset);
  }
  assert((offset & 1) == 0);
  return (ptrbit) { .ptr = sizeof(word) * known_words, .bit = offset };
}
// Length in bits.
word bitlength(const word* crumbstring, word known_words) {
  return 2 * ptrbit2crumbs(ptrbitlength(crumbstring, known_words));
}

ptrbit input_at = {};
ptrbit stop_at = {};
ptrbit output_at = {};

#define SZ 1024
// read once
word input_words[SZ];
// write once
word output_words[SZ];

// An operand is either shared (heap or synthetic), or an inline word (immediate).
typedef struct operand {
  union {
    struct shared_operand* shared;
    word crumbs;
  };
  bool immediate;
} operand;

// A ref-counted shared operand used for sharing work from lazy evaluation.
// The pointer it stores is updated after it is evaluated, and `work` increased.
typedef struct shared_operand {
  int ref_count;
  word work;
  union {
    struct synthetic_operand* synth;
    struct heap_operand* heap;
  };
  bool on_heap;
} shared_operand;

// An operand on the heap. As the `shared_operand` that owns this `heap_operand`
// is evaluated, it will (eventually) free this `heap_operand` and replace
// it with a new one so that the evaluation is shared.
//
// Terms that are immediate but too large to fit into a word also end up
// as a `heap_operand`.
typedef struct heap_operand {
  int length;
  word crumbstring[];
} heap_operand;

// A synthetic apply node, produced during evaluation of `S`. It counts as a
// single reference to its operands while it is alive. It represents the
// crumbstring `0(fun)(arg)`.
typedef struct synthetic_operand {
  operand fun;
  operand arg;
} synthetic_operand;


operand direct(const word crumbs) {
  return (operand) { .immediate = true, .crumbs = crumbs };
}
operand shared(shared_operand* shared) {
  return (operand) { .immediate = false, .shared = shared };
}

// Some temporaries, used for greasing the wheels of algorithms (avoiding
// special cases)

heap_operand heap_tmp1 = { .length = 1, .crumbstring = {0} };
heap_operand heap_tmp2 = { .length = 1, .crumbstring = {0} };
shared_operand shared_tmp = { .ref_count = ~1, .work = 0, .on_heap = true, .heap = &heap_tmp1 };

heap_operand* as_tmp1(operand l) {
  if (l.immediate) {
    heap_tmp1.crumbstring[0] = l.crumbs;
    return &heap_tmp1;
  }
  assert(l.shared->on_heap);
  return l.shared->heap;
}
heap_operand* as_tmp2(operand r) {
  if (r.immediate) {
    heap_tmp2.crumbstring[0] = r.crumbs;
    return &heap_tmp2;
  }
  assert(r.shared->on_heap);
  return r.shared->heap;
}
shared_operand* as_tmp(operand term) {
  as_tmp1(term);
  shared_tmp.ref_count = ~1; // never de-allocate it
  shared_tmp.work = 0; // reset
  shared_tmp.on_heap = true; // just in case
  return &shared_tmp;
}

// Create a new shared heap operand, given a wordlength to allocate.
__attribute__((noinline))
shared_operand* alloc_shared_heap(const int wordlength) {
  heap_operand* on_heap = malloc(sizeof(heap_operand) + sizeof(word[wordlength]));
  on_heap->length = wordlength;
  shared_operand* shared = malloc(sizeof(shared_operand));
  shared->ref_count = 1;
  shared->work = 0;
  shared->on_heap = true;
  shared->heap = on_heap;
  return shared;
}

bool drop(operand op); // forward declaration

// Decrement the reference count and de-allocate it if it reached 0
bool drop_shared(shared_operand* shared) {
  if (!shared->ref_count || !--shared->ref_count) {
    if (shared->on_heap) {
      free(shared->heap);
    } else {
      drop(shared->synth->fun);
      drop(shared->synth->arg);
      free(shared->synth);
    }
    free(shared);
    return true;
  }
  return false;
}

// Drop a reference if it is a shared operand. Immediate operands are not
// ref counted.
__attribute__((noinline))
bool drop(operand op) {
  if (op.immediate) return false;
  shared_operand* shared = op.shared;
  return drop_shared(shared);
}

// If `evaled` represents the result of evaluating shared node `was_evaling`,
// then we want to swap its value into `was_evaling`. Unfortunately this is not
// a trivial task with refcounting and manual memory management.
__attribute__((noinline))
operand have_evaled(shared_operand* was_evaling, const operand evaled) {
  // Not shared
  if (was_evaling == NULL) return evaled;
  // Not shared anymore
  if (drop_shared(was_evaling)) return evaled;

  if (evaled.immediate) {
    if (was_evaling->on_heap) {
      // Might as well reuse small buffers, but reallocate large ones
      if (was_evaling->heap->length > 64) {
        free(was_evaling->heap);
        was_evaling->heap = malloc(sizeof(heap_operand) + sizeof(word[1]));
      }
    } else {
      was_evaling->on_heap = true;
      was_evaling->heap = malloc(sizeof(heap_operand) + sizeof(word[1]));
    }
    was_evaling->heap->length = 1;
    was_evaling->heap->crumbstring[0] = evaled.crumbs;
  } else {
    // Will not actually happen but that is okay
    if (evaled.shared == was_evaling) return evaled;

    // Free the stale data (unevaluated/less evaluated value)
    if (was_evaling->on_heap) {
      free(was_evaling->heap);
    } else {
      drop(was_evaling->synth->fun);
      drop(was_evaling->synth->arg);
      free(was_evaling->synth);
    }

    was_evaling->ref_count = was_evaling->ref_count; // does not change
    was_evaling->work += evaled.shared->work; // add the work (TODO?)

    // Transfer ownership from `evaled` to `was_evaling`
    if ((was_evaling->on_heap = evaled.shared->on_heap)) {
      was_evaling->heap = evaled.shared->heap;
    } else {
      was_evaling->synth = evaled.shared->synth;
    }
  }
  return shared(was_evaling);
}

// Increase the ref count. If the node is immediate, it will copy it to not
// lose work...
__attribute__((noinline))
operand dup(operand term) {
  if (term.immediate) {
    // An irreducible direct operand is safe to be duplicated
    if (!redexes(0, term.crumbs)) return term;
    // Otherwise we need to make a shared operand
    shared_operand* sh = alloc_shared_heap(1);
    sh->heap->crumbstring[0] = term.crumbs;
    sh->ref_count = 2;
    return shared(sh);
  }
  // If it is already shared, we just need to increment the `ref_count`
  term.shared->ref_count++;
  return term;
}

// Just increment the refcount.
__attribute__((noinline))
operand softdup(operand term) {
  if (!term.immediate) {
    term.shared->ref_count++;
  }
  return term;
}

// Concatenate two heap operands into a new shared operand of the two applied.
// That is, it starts with a zero crumb (for the application operator), and
// then the crumbs of the first followed by the crumbs of the second, `0{l}{r}`.
__attribute__((noinline))
shared_operand* ap_heap_cat(heap_operand* l, heap_operand* r) {
  // FIXME: decrement length if overallocated?
  int wordlength = l->length + r->length + 1;
  shared_operand* result = alloc_shared_heap(wordlength);
  result->heap->length = wordlength;
  word* out = result->heap->crumbstring;
  copy_to_unaligned(out, l->crumbstring, 2, l->length, true);
  ptrbit reallength = ptrbitlength(l->crumbstring, l->length);
  assert(reallength.bit != 0);
  assert(reallength.ptr == sizeof(word) * l->length);
  reallength = ptrbit_incr_bits(reallength, 2);
  copy_to_unaligned(out + (reallength.ptr / sizeof(word)), r->crumbstring, reallength.bit, r->length, false);
  return result;
}

// Construct a new operand with `fun` applied to `arg`, via whatever
// representation we guessed is best.
__attribute__((noinline))
operand apply(const operand fun, const operand arg) {
  // If we have two direct operands, we can just merge them, if they are small
  // enough.
  if (fun.immediate && arg.immediate && !redexes(0, fun.crumbs) && !redexes(0, arg.crumbs)) {
    u8 fun_len = reachesZero(1, fun.crumbs);
    u8 arg_len = reachesZero(1, arg.crumbs);
    if (fun_len + arg_len <= 64) {
      return direct(word_cat(fun_len, fun.crumbs, arg_len, arg.crumbs));
    } // else fallthrough
  }
  // If we have two nodes that are not otherwise shared, we can delete them
  // and merge them.
  if (
    (fun.immediate || (fun.shared->on_heap && fun.shared->ref_count == 1))
    &&
    (arg.immediate || (arg.shared->on_heap && arg.shared->ref_count == 1))
  ) {
    operand result = shared(ap_heap_cat(as_tmp1(fun), as_tmp2(arg)));
    // free the shared nodes
    drop(fun); drop(arg);
    return result;
  }
  // Otherwise we need to synthesize a new application node to share the result
  // of evaluating both of them.
  synthetic_operand* ap_node = malloc(sizeof(synthetic_operand));
  ap_node->fun = fun;
  ap_node->arg = arg;
  shared_operand* sh = malloc(sizeof(shared_operand));
  sh->ref_count = 1;
  sh->work = 0;
  sh->on_heap = false;
  sh->synth = ap_node;
  return shared(sh);
}

// When you evaluate an application node `0xy`, `y` goes onto the stack (first)
// and `x` is the new top of stack. Reaching `I`, `K`, or `S` will pop from the
// stack and push back onto it: `S` will create up to three synthetic operands
// in doing so.
//
// The interpreter wants to maintain one thing on the stack (mostly to keep
// the `was_evaling` pointer for simplicity). If it reaches zero for real, that
// means it is time to return (after writing to output): then the value of
// `was_evaluting` would theoretically be the whole input/output node.
//
// `(01031) =: was_evaling -> (1)(031) -> (031) =: *was_evaling`
struct stack_frame {
  operand to_eval;
  shared_operand* was_evaling; // weak ref! must be zeroed out when freed
} stack[SZ];
u32 stack_top = 0;

#define FRAME(value) (struct stack_frame) { .to_eval = (value) }

// Fuel limits the explicit work done, in the evaluation loop
u64 fuel asm("fuel") = 10000;
// Work is the number of reductions that it takes to get to what was produced,
// possibly out of order of the nominal lazy evaluation
u64 work asm("work") = 0;

__attribute__((always_inline))
ptrbit _scan1(const word crumbstring[], ptrbit _input_at, const ptrbit _stop_at) {
  goodptrbit(_input_at);
  ptrbit failed = _input_at;
  // adjust the expected number of nodes by the leading nonzeros *crumbs*
  // we will add (could use leading zeros, but this seems more robust to
  // optimizations in `reachesZero()`)
  word exp = 1 + (_input_at.bit >> 1);

  #pragma unroll(2)
  while (input_at.ptr < stop_at.ptr || input_at.bit < stop_at.bit) {
    goodptrbit(_input_at);
    // Fill the leading bits with ones (first loop iteration only)
    word crumbs = word_of(crumbstring, _input_at) | ~(word_max >> _input_at.bit);
    if ((_input_at.bit = reachesZero(exp, crumbs))) {
      if (input_at.ptr < stop_at.ptr || input_at.bit < stop_at.bit) {
        return _input_at;
      }
    }
    _input_at.ptr += 8;
    _input_at.bit = 0;
    s8 delta = deltaExpecting(crumbs);
    assert(delta <= 0 || exp > delta);
    exp -= delta;
    assert(exp);
  }
  return failed;
}

__attribute__((noinline))
ptrbit scan1(const word crumbstring[], ptrbit _input_at, const ptrbit _stop_at) {
  return _scan1(crumbstring, _input_at, _stop_at);
}

__attribute__((noinline))
bool scan1input() {
  const ptrbit failed = input_at;
  const ptrbit result = _scan1(input_words, input_at, stop_at);
  return result.ptr == failed.ptr && result.bit == failed.bit;
}

__attribute__((noinline))
bool refill() {
  assert(stack_top == 0);

  goodptrbit(input_at);
  ptrbit start_at = input_at;
  if (!scan1input()) return false;
  goodptrbit(input_at);
  const word crumblen = ptrbit2crumbs(input_at) - ptrbit2crumbs(start_at);
  operand op;
  // Already fit into one word
  if (start_at.ptr == input_at.ptr || start_at.bit == 0) {
    word crumbs = leftbits_of(input_words, start_at);
    op = direct(mask_hi(2*crumblen) & crumbs);
  // Stored over adjacent words but fits into one word
  } else if (2*crumblen <= word_size) {
    assert(input_at.bit == start_at.ptr + sizeof(word));
    assert(input_at.bit <= start_at.bit);
    const word crumbs = word_cat(
      word_size - start_at.bit,
        leftbits_of(input_words, start_at),
      (2*crumblen) - (word_size - start_at.bit),
        nextword_of(input_words, start_at)
    );
    op = direct(mask_hi(2*crumblen) & crumbs);
  } else {
    op = shared(alloc_shared_heap(ROUNDUPDIV(crumblen, 32)));
    copy_from_unaligned(
      op.shared->heap->crumbstring, from_ptr(input_words, start_at),
      start_at.bit, input_at.ptr/sizeof(word) - start_at.ptr/sizeof(word)
    );
    // TODO: zero out last of bits
  }
  // TODO: trivial reductions
  if (op.immediate && redexes(0, op.crumbs)) {
    shared_operand* tmp = alloc_shared_heap(1);
    tmp->heap->crumbstring[0] = op.crumbs;
    op = shared(tmp);
  }

  stack[stack_top].was_evaling = NULL;
  stack[stack_top].to_eval = op;
  stack_top++;
  return true;
}

__attribute__((noinline))
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
    // (we may have written other bits already, from the hapless copying)
    word_of(output_words, output_at) &= mask_hi(output_at.bit);
    output_at = ptrbit_incr_bits(output_at, 2);
    // Write each operand
    write_out(op.shared->synth->fun);
    write_out(op.shared->synth->arg);
    drop(op);
  }
}

// Evaluate one function at the top of the stack.
bool step() {
  if (!stack_top && !refill()) return false; // all done

  operand op = stack[stack_top--].to_eval;

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
      op = direct(crumb);
    }

    // Try to keep at least one thing on the stack to make sure we have
    // somewhere to pin `was_evaled`. But if we ran out of input, that is fine:
    // it is the final node to evaluate anyways.
    else if (op.shared->ref_count > 1 && (stack_top || refill())) {
      // Increase its ref count
      stack[stack_top].was_evaling = dup(op).shared;
    }
  }

  #define POP_OR(lvalue, stmt) BLOCK(if (!stack_top && !refill()) { stmt; }; lvalue = stack[stack_top--].to_eval;)
  if (op.immediate) {
    const u8 crumb = op.crumbs >> (word_size - 2);

    // First take care of thunks that are still shared, which are now at least
    // in WHNF (weak head normal form: an underapplied S, K, or I).
    if (crumb) {
      assert(crumb <= 3);
      // Scan the stack to see how many we need to save, up to one less than
      // the arity of this node (since we are evaling it applied to the arity)
      u8 was_evaled = 0;
      for (u8 i=0; i < stack_top && i < crumb-1; i++) {
        if (stack[stack_top - i].was_evaling != NULL && stack[stack_top - i].was_evaling->ref_count > 1) {
          was_evaled = i+1;
        }
      }
      if (was_evaled) {
        operand evaled = op;
        for (u8 i=0; i < was_evaled; i++) {
          if (stack[stack_top - i].was_evaling != NULL) {
            if (stack[stack_top - i].was_evaling->ref_count == 1) {
              // Not actually shared anymore. Drop it!
              drop_shared(stack[stack_top - i].was_evaling);
            } else if (stack[stack_top - i].was_evaling->ref_count > 1) {
              // Shared, so we want to swap out what we computed to be its
              // WHNF value `evaled` for whatever it has currently. And we
              // snowball from there.
              //
              // If `evaled` has a heap, then ownership of it is transferred
              // to `was_evaling`, for example. Whatever `was_evaling` has
              // is deleted, unless it is a small heap that can be reused
              // by an immediate `evaled`.
              have_evaled(stack[stack_top - i].was_evaling, dup(evaled));
              evaled = shared(stack[stack_top - i].was_evaling);
            }
            stack[stack_top - i].was_evaling = NULL;
          }
          // Create a new apply node only if it will be transferred, otherwise
          // it is just a leak.
          if (i < was_evaled-1) {
            evaled = apply(evaled, stack[stack_top].to_eval);
          } else break;
        }
      }
    }

    // Now we handle the particular opcodes: I=1, K=2, S=3
    if (crumb == 1) {
      // Check that there is an argument available
      if (!stack_top && !refill()) return false;

      // Just drop this identity node, good riddance
      work++;
      return true;
    } else if (crumb == 2) {
      // Get two arguments
      operand x, y;
      POP_OR(x, return false);
      POP_OR(y, return false);

      // Drop `y` and put `x` back onto the stack: `Kxy = x`
      drop(y);
      stack[++stack_top] = FRAME(x);
      work++;
      return true;
    } else if (crumb == 3) {
      // Get three arguments
      operand x, y, z;
      POP_OR(x, return false);
      POP_OR(y, return false);
      POP_OR(z, return false);

      // `Sxyz = 00xz0yz = ((xz)(yz))`
      // Increase the ref count on `z`
      z = dup(z);
      // Put `x`, `z`, and the synthetic `(yz)` on the stack
      operand yz = apply(y, z);
      stack[++stack_top] = FRAME(yz);
      stack[++stack_top] = FRAME(z);
      stack[++stack_top] = FRAME(x);
      work++;
      // Evaluation will continue with `x` on the next `step()`!
      return true;
    }
    // Fall through to the next case (`if (op.shared->on_heap)`)
    op = shared(as_tmp(op));
  }
  assert(!op.immediate);
  // If we get here, it should be an application node, either packed on the heap
  // or as synthetic applications.

  // A synthetic application
  if (!op.shared->on_heap) {
    // We might as well walk the spine of synthetic applications here in a tight
    // loop, instead of waiting for the next `step();`
    operand spine = op;
    #pragma unfold(2)
    while (!spine.immediate && !spine.shared->on_heap) {
      // (Already handled `was_evaling` for `spine = op`)
      if (stack_top) stack[stack_top].was_evaling = dup(spine).shared;
      // Now we add it onto the stack
      stack[++stack_top] = FRAME(dup(spine.shared->synth->arg));
      // And proceed down the spine
      spine = spine.shared->synth->fun;
    }
    // Finally, whatever we ended up with is at the top of the stack.
    stack[++stack_top] = FRAME(spine);
    // And free the spine (all the arguments were dup'ed, so are safe)
    drop(op);
    return true;
  }

  // Otherwise we have some applications waiting to unpack from a crumbstring
  if (op.shared->on_heap) {
    // Cache the amount of bytes
    const int bytes = sizeof(word) * op.shared->heap->length;
    // Here is the crumbstring
    word* crumbstring = op.shared->heap->crumbstring;
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
    goodptrbit(i);

    // This is now our arity
    const word arity = ptrbit2crumbs(i);

    // I guess it cannot be too large to fit on the stack
    assert(arity == (int)arity);
    assert(arity);

    // Increase stack_top already, and then we will walk back and fill in the
    // arguments on the stack.
    stack_top += arity;
    for (int stacked = 0; stacked < arity; stacked++) {
      goodptrbit(i);
      const ptrbit last = i;
      // Scan ahead one item
      i = scan1(crumbstring, last, (ptrbit) { .ptr = bytes, .bit = 0 });
      const int crumblen = ptrbit2crumbs(i) - ptrbit2crumbs(last);
      // Copy it into its own heap
      shared_operand* sh = alloc_shared_heap(crumblen / word_crumbs);
      copy_from_unaligned(sh->heap->crumbstring, from_ptr(crumbstring, last), last.bit, sh->heap->length);
      // And plop it on the stack
      stack[stack_top - stacked] = FRAME(shared(sh));
    }
    return true;
  }
  return false;
}

word eval(word input_crumbs) {
  if (!input_crumbs) return 0;
  input_at = (ptrbit) {};
  stop_at = crumbs2ptrbit(input_crumbs);

  while (fuel-- && step()) {};

  return 0;
}

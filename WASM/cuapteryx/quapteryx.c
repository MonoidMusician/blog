#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include "cuapteryx.h"
#include "compilation.h"

////////////////////////////////////////////////////////////////////////////////
// Externs (using walloc.c)                                                   //
////////////////////////////////////////////////////////////////////////////////

extern void *malloc(size_t size) __attribute__((warn_unused_result, alloc_size(1), noinline));
extern void free(void *p) __attribute__((noinline));

#undef assert
#pragma clang diagnostic ignored "-Wassume"
// #define assert(c) BLOCK(if (!(c)) {print64(#c, __LINE__);__builtin_trap();})
// #define assert(c) BLOCK(if (!(c)) {__builtin_trap();})
// #define assert(c) BLOCK(if (!(c)) {__builtin_unreachable();})
#define assert(c) __builtin_assume((c))

////////////////////////////////////////////////////////////////////////////////
// More basic helpers                                                         //
////////////////////////////////////////////////////////////////////////////////

// Copy all words of the source `src[bits: 0 : word_size*words]` to the
// destination at some positive offset, zeroing the first word if `clobber` is
// set (otherwise preserving those first `offset` bits):
// ```
// if (clobber) dst[words: 0] = (word) 0;
// dst[bits: offset : offset + 64*words] =
//   src[bits: 0 : 64*words]
// ```
void copy_to_unaligned(word* dst, const word* src, const u8 offset, word words, bool clobber) {
  if (words == 0) return;
  assert(offset < word_size);
  if (offset == 0) {
    __builtin_memcpy(dst, src, sizeof(word) * words);
    return;
  }
  assert(offset < word_size);
  assert(offset != 0); // messes with bitshifts
  word hanging = clobber ? 0 : *dst;
  while (words--) {
    assert(offset < word_size);
    *dst = word_cat(offset, hanging, word_size - offset, *src);
    hanging = *src << (word_size - offset);
    dst++;
    src++;
  }
  if (hanging) *dst = hanging;
}
void copy_from_unaligned(word* dst, const word* src, u8 offset, word words) {
  if (words == 0) return;
  assert(offset < word_size);
  if (offset == 0) {
    __builtin_memcpy(dst, src, sizeof(word) * words);
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

// Calculate the length of the first operand of the passed crumbstring, which is
// more efficient if `known_words != 0` is passed (otherwise it has to examine
// each crumb).
ptrbit ptrbitlength(const word* crumbstring, word known_words) {
  word i = 0; word exp = 1; u8 offset = 0;
  // Quickly scan through `known_words - 1`, using just
  // `deltaExpecting` and not `reachesZero`
  // if (known_words) while (i < known_words - 1) {
  //   exp -= deltaExpecting(crumbstring[i++]);
  //   assert(exp != 0);
  // }
  // Fall through, in case `known_words` was too short.
  // (This is used in `ap_heap_cat`.)
  while (!offset) {
    // See if it reaches zero during the current word
    offset = reachesZero(exp, crumbstring[i]);
    if (!offset) {
      // If not, we need to continue with the next word
      // and adjust the expected number of operands
      exp -= deltaExpecting(crumbstring[i++]);
      assert(exp != 0);
      continue;
    } else break;
  }
  // It needs to be even since it is a bit index of a crumb
  assert((offset & 1) == 0);
  // `i` denotes the index of the word that reached zero
  return (ptrbit) { .ptr = sizeof(word) * i, .bit = offset };
}

// Length of an operand in bits in a crumbstring.
word bitlength(const word* crumbstring, word known_words) {
  return 2 * ptrbit2crumbs(ptrbitlength(crumbstring, known_words));
}

////////////////////////////////////////////////////////////////////////////////
// Data structures for operands: immediate or shared (heap or synthetic)      //
////////////////////////////////////////////////////////////////////////////////

// An operand is either shared (heap or synthetic), or an inline word (immediate).
// This is determined by the last bit: since an operand is always an odd number
// of crumbs, it is unused in the immediate operand so we can set it to `1` in
// `crumbs`, and memory is always aligned, so it is set to `0` in the `shared`
// pointer. Interestingly this seems to speed up WASM more than native x86_64.
typedef union operand {
  struct shared_operand* shared;
  word crumbs;
} operand;

// A shared operand is ref-counted and used for sharing work from lazy evaluation.
// The pointer it stores is updated after it is evaluated, and `work` increased.
typedef struct shared_operand {
  union {
    struct synthetic_operand* synth; // even pointer
    struct heap_operand* heap1; // odd pointer
    // (this optimization does not help much)
  };
  int ref_count;
  word work;
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
  size_t length;
  word crumbstring[];
} heap_operand;


// A synthetic apply node, produced during evaluation of `S`. It counts as a
// single reference to its operands while it is alive. It represents the
// crumbstring `0(fun)(arg)`.
typedef struct synthetic_operand {
  operand fun;
  operand arg;
} synthetic_operand;


operand nullop = {};
bool isnullop(operand op) {
  return op.crumbs == nullop.crumbs;
}
bool isimmediate(operand op) {
  return op.crumbs & 0b1;
}
operand direct(word crumbs) {
  assert(crumbs != 0);
  assert(reachesZero1(crumbs));
  assert(reachesZero1(crumbs) < word_size);
  // Clear out the irrelevant crumbs (optional, slower)
  // (Seems to impact WASM more than x86_64, for whatever reason)
  crumbs &= mask_hi(reachesZero1(crumbs));
  return (operand) { .crumbs = crumbs | 0b1 };
}
operand _direct(word crumbs) {
  return (operand) { .crumbs = crumbs | 0b1 };
}
operand shared(shared_operand* shared) {
  assert(shared != 0);
  assert(((size_t)shared & 0b1) == 0);
  return (operand) { .shared = shared };
}

bool isheap(shared_operand* op) {
  return (size_t)op->heap1 & 0b1;
}
heap_operand* getheap(shared_operand* op) {
  return (heap_operand*) ((size_t)op->heap1 & ~0b1);
}
void setheap(shared_operand* op, heap_operand* heap) {
  op->heap1 = (heap_operand*) ((size_t)heap | 0b1);
  assert(isheap(op));
}
bool issharedheap(operand op) {
  return !isimmediate(op) && isheap(op.shared);
}
heap_operand* getsharedheap(operand op) {
  return getheap(op.shared);
}

bool match_crumbs(const word match_low, const u8 crumblen, const operand op) {
  if (!isimmediate(op) && !issharedheap(op)) return false;
  const u8 len = crumblen * 2;
  const word mask = mask_hi(len);
  const word crumbs = isimmediate(op) ? op.crumbs : getsharedheap(op)->crumbstring[0];
  return (mask & crumbs) == (match_low << (word_size - len));
}

bool isI(const operand op) {return match_crumbs(0x1, 1, op);}
bool isK(const operand op) {return match_crumbs(0x2, 1, op);}
bool isS(const operand op) {return match_crumbs(0x3, 1, op);}

////////////////////////////////////////////////////////////////////////////////
// Stack, for evaluation                                                      //
////////////////////////////////////////////////////////////////////////////////

#define SZ 128*1024

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

const u32 stack_length = sizeof(stack) / sizeof(struct stack_frame);

// Create a new stack frame, without `was_evaling`.
#define FRAME(value) ((struct stack_frame) { .to_eval = (value) })


////////////////////////////////////////////////////////////////////////////////
// Input and output buffers and their state                                   //
////////////////////////////////////////////////////////////////////////////////

ptrbit input_at = {};
ptrbit stop_at = {};
ptrbit output_at = {};

// read once
word input_words[SZ];
// write once
word output_words[SZ];

const u32 input_words_length = sizeof(input_words) / sizeof(word);
const u32 output_words_length = sizeof(output_words) / sizeof(word);


////////////////////////////////////////////////////////////////////////////////
// Some temporaries, used for greasing the wheels of algorithms (avoiding     //
// special cases)                                                             //
////////////////////////////////////////////////////////////////////////////////

heap_operand heap_tmp1 = { .length = 1, .crumbstring = {0} };
heap_operand heap_tmp2 = { .length = 1, .crumbstring = {0} };
shared_operand shared_tmp = { .ref_count = ~1, .work = 0 };

heap_operand* as_tmp1(operand l) {
  if (isimmediate(l)) {
    heap_tmp1.crumbstring[0] = l.crumbs;
    return &heap_tmp1;
  }
  assert(issharedheap(l));
  return getsharedheap(l);
}
heap_operand* as_tmp2(operand r) {
  if (isimmediate(r)) {
    heap_tmp2.crumbstring[0] = r.crumbs;
    return &heap_tmp2;
  }
  assert(issharedheap(r));
  return getsharedheap(r);
}
shared_operand* as_tmp(operand term) {
  shared_tmp.ref_count = ~1; // never de-allocate it
  shared_tmp.work = 0; // reset
  setheap(&shared_tmp, as_tmp1(term));
  return &shared_tmp;
}

////////////////////////////////////////////////////////////////////////////////
// Memory management and other fundamental operations on operands             //
////////////////////////////////////////////////////////////////////////////////

void goodop(operand op) {
  assert(!isnullop(op));
  if (isimmediate(op)) {
    assert(reachesZero1(op.crumbs));
  } else {
    assert(op.shared != NULL);
    if (issharedheap(op)) {
      assert(getsharedheap(op)->length > 0);
    } else {
      // goodop(op.shared->synth->arg);
      // goodop(op.shared->synth->fun);
    }
    assert(op.shared->weakref_stack_max == ~0 || op.shared->weakref_stack_max < stack_length);
    assert(op.shared->weakref_stack_min == ~0 || op.shared->weakref_stack_min < stack_length);
  }
}

// Create a new shared heap operand, given a wordlength to allocate.
__attribute__((noinline))
shared_operand* alloc_shared_heap(const size_t wordlength) {
  assert(wordlength);
  heap_operand* on_heap = malloc(sizeof(heap_operand) + sizeof(word[wordlength]));
  assert(on_heap != NULL);
  on_heap->length = wordlength;
  on_heap->crumbstring[wordlength-1] = 0; // FIXME?
  shared_operand* shared = malloc(sizeof(shared_operand));
  assert(shared != NULL);
  shared->ref_count = 1;
  shared->work = 0;
  setheap(shared, on_heap);
  shared->weakref_stack_max = ~0;
  shared->weakref_stack_min = ~0;
  return shared;
}

// Decrement the reference count and de-allocate it if it reached 0
void drop_shared(shared_operand* shared) {
  assert(shared->ref_count != 0);
  if (!--shared->ref_count) {
    if (shared->weakref_stack_max != ~0 || shared->weakref_stack_min != ~0) {
      assert(shared->weakref_stack_max != ~0 && shared->weakref_stack_min != ~0);
      assert(shared->weakref_stack_max >= shared->weakref_stack_min);
      int i = shared->weakref_stack_max;
      if (i >= stack_top) i = stack_top - 1;
      for (; i >= shared->weakref_stack_min; i--) {
        if (stack[i].was_evaling == shared) stack[i].was_evaling = NULL;
      }
    }
    if (isheap(shared)) {
      free(getheap(shared));
      free(shared);
    } else {
      if (!isimmediate(shared->synth->fun)) {
        drop_shared(shared->synth->fun.shared);
      }
      operand arg = shared->synth->arg;
      free(shared->synth);
      free(shared);
      if (!isimmediate(arg)) {
        TAILCALL return drop_shared(arg.shared);
      }
    }
  }
}

// Drop a reference if it is a shared operand. Immediate operands are not
// ref counted.
bool drop(operand op) {
  if (isnullop(op) || isimmediate(op)) return false;
  if (op.shared->ref_count > 1) {
    op.shared->ref_count--;
    return false;
  }
  shared_operand* shared = op.shared;
  drop_shared(shared);
  return true;
}

// The way the `was_evaling` weak ref works is that we re-scan the stack to see
// which weakrefs are still there, and we keep track of the bounds of indices
// to scan (often just one).
void stack_was_evaling(int stack_idx, shared_operand* was_evaling) {
  // if (stack[stack_idx].was_evaling == was_evaling) return; // ??
  // assert(stack[stack_idx].was_evaling == NULL);
  if (was_evaling->weakref_stack_max == ~0 || was_evaling->weakref_stack_max < stack_idx) {
    was_evaling->weakref_stack_max = stack_idx;
  }
  if (was_evaling->weakref_stack_min == ~0 || was_evaling->weakref_stack_min > stack_idx) {
    was_evaling->weakref_stack_min = stack_idx;
  }
  assert(was_evaling->weakref_stack_max != ~0 && was_evaling->weakref_stack_min != ~0);
  assert(was_evaling->weakref_stack_max >= was_evaling->weakref_stack_min);
  stack[stack_idx].was_evaling = was_evaling;
}

// If `evaled` represents the result of evaluating shared node `was_evaling`,
// then we want to swap its value into `was_evaling`. Unfortunately this is not
// a trivial task with refcounting and manual memory management.
operand have_evaled(shared_operand* was_evaling, const operand evaled) {
  goodop(evaled);
  // Not shared
  if (was_evaling == NULL) return evaled;
  goodop(shared(was_evaling));

  if (isimmediate(evaled)) {
    if (isheap(was_evaling)) {
      // Might as well reuse small buffers, but reallocate large ones
      heap_operand* heap = getheap(was_evaling);
      if (heap->length > 64) {
        free(heap);
        heap = malloc(sizeof(heap_operand) + sizeof(word[1]));
        assert(heap != NULL);
      }
    } else {
      drop(was_evaling->synth->fun);
      drop(was_evaling->synth->arg);
      free(was_evaling->synth);
      setheap(was_evaling, malloc(sizeof(heap_operand) + sizeof(word[1])));
    }
    getheap(was_evaling)->length = 1;
    getheap(was_evaling)->crumbstring[0] = evaled.crumbs;
  } else {
    // Will not actually happen but that is okay
    if (evaled.shared == was_evaling) return evaled;

    // Free the stale data (unevaluated/less evaluated value)
    if (isheap(was_evaling)) {
      free(getheap(was_evaling));
    } else {
      drop(was_evaling->synth->fun);
      drop(was_evaling->synth->arg);
      free(was_evaling->synth);
    }

    was_evaling->ref_count = was_evaling->ref_count; // does not change
    was_evaling->work += evaled.shared->work; // add the work (TODO?)

    // Transfer ownership from `evaled` to `was_evaling`
    if (isheap(evaled.shared)) {
      setheap(was_evaling, getheap(evaled.shared));
    } else {
      was_evaling->synth = evaled.shared->synth;
    }
  }
  return shared(was_evaling);
}

// Increase the ref count. If the node is immediate, it will copy it to not
// lose work...
operand dup(operand term) {
  if (isimmediate(term)) {
    // An irreducible direct operand is safe to be duplicated
    if (!nontrivial_redexes(term.crumbs & ~0b11))
      return term;
    // Otherwise we need to make a shared operand
    shared_operand* sh = alloc_shared_heap(1);
    getheap(sh)->crumbstring[0] = term.crumbs;
    assert(bitlength(getheap(sh)->crumbstring, 0) <= word_size);
    sh->ref_count = 2;
    return shared(sh);
  }
  // If it is already shared, we just need to increment the `ref_count`
  term.shared->ref_count++;
  return term;
}
// Soft dup just increases the ref count (of already shared operands), it does
// not try to share immediate operands, even if they are reducible.
operand soft_dup(operand term) {
  if (isimmediate(term)) {
    return term;
  }
  // If it is already shared, we just need to increment the `ref_count`
  term.shared->ref_count++;
  return term;
}

operand fast_unconstant(const operand op) {
  if (isimmediate(op)) {
    // Match `02x` -> `x`
    if (match_crumbs(0x2, 2, op)) {
      // Shift by 4 bits to isolate `x`
      return _direct((op.crumbs & ~0b11) << 4);
    }
  } else if (issharedheap(op)) {
    if (match_crumbs(0x2, 2, op)) {
      heap_operand* heap = getsharedheap(op);
      // Do not try hard to generate a new heap
      // FIXME: loss of sharing?
      if (heap->length == 1) {
        return _direct((heap->crumbstring[0] & ~0b11) << 4);
      }
      if (op.shared->ref_count == 1) {
        // Seems to only help native x86_64 not WASM
        u8 lastBits = 0;
        for (int i = heap->length; i-- > 0; ) {
          u8 save = heap->crumbstring[i] >> (word_size - 4);
          heap->crumbstring[i] = (heap->crumbstring[i] << 4) | lastBits;
          lastBits = save;
        }
        assert(lastBits == 0x2);
        return op;
      }
    }
  } else {
    if (isK(op.shared->synth->fun)) {
      const operand arg = soft_dup(op.shared->synth->arg);
      drop(op);
      return arg;
    }
  }
  return nullop;
}

// Concatenate two heap operands into a new shared operand of the two applied.
// That is, it starts with a zero crumb (for the application operator), and
// then the crumbs of the first followed by the crumbs of the second, `0{l}{r}`.
shared_operand* ap_heap_cat(heap_operand* l, heap_operand* r) {
  // FIXME: decrement length if overallocated?
  size_t wordlength = l->length + r->length;
  shared_operand* result = alloc_shared_heap(wordlength);
  getheap(result)->length = wordlength;
  word* out = getheap(result)->crumbstring;
  copy_to_unaligned(out, l->crumbstring, 2, l->length, true);
  assert(l->crumbstring[0] >> 2 == out[0]);
  ptrbit reallength = ptrbitlength(l->crumbstring, l->length - 1);
  assert(reallength.bit != 0);
  // assert(reallength.ptr == sizeof(word) * (l->length - 1));
  reallength = ptrbit_incr_bits(reallength, 2);
  copy_to_unaligned(out + (reallength.ptr / sizeof(word)), r->crumbstring, reallength.bit, r->length, false);

  assert(bitlength(out, 0) == 2 + bitlength(l->crumbstring, 0) + bitlength(r->crumbstring, 0));
  return result;
}

operand synthetic_apply(const operand fun, const operand arg) {
  synthetic_operand* ap_node = malloc(sizeof(synthetic_operand));
  assert(ap_node != NULL);
  assert(((size_t)ap_node & 0b1) == 0);
  ap_node->fun = fun;
  ap_node->arg = arg;
  shared_operand* sh = malloc(sizeof(shared_operand));
  assert(sh != NULL);
  sh->ref_count = 1;
  sh->work = 0;
  sh->synth = ap_node;
  sh->weakref_stack_max = ~0;
  sh->weakref_stack_min = ~0;
  return shared(sh);
}

// Construct a new operand with `fun` applied to `arg`, via whatever
// representation we guessed is best.
operand apply(const operand fun, const operand arg) {
  goodop(fun); goodop(arg);
  if (isI(fun)) {
    extern u64 work; // shh
    work++;
    return arg;
  }
  // If we have two direct operands, we can just merge them, if they are small
  // enough.
  if (isimmediate(fun) && isimmediate(arg) && !nontrivial_redexes(fun.crumbs & ~0b11) && !nontrivial_redexes(arg.crumbs & ~0b11)) {
    u8 fun_len = reachesZero1(fun.crumbs);
    u8 arg_len = reachesZero1(arg.crumbs);
    if (2 + fun_len + arg_len <= word_size) {
      return _direct(word_cat(fun_len, fun.crumbs, arg_len, arg.crumbs) >> 2);
    } // else fallthrough
  }
  // If we have two nodes that are not otherwise shared, we can delete them
  // and merge them.
  if (
    (isimmediate(fun) || (issharedheap(fun) && fun.shared->ref_count == 1 && getsharedheap(fun)->length < 16))
    &&
    (isimmediate(arg) || (issharedheap(arg) && arg.shared->ref_count == 1 && getsharedheap(arg)->length < 16))
  ) {
    operand result = shared(ap_heap_cat(as_tmp1(fun), as_tmp2(arg)));
    // free the shared nodes
    drop(fun); drop(arg);
    return result;
  }
  // Otherwise we need to synthesize a new application node to share the result
  // of evaluating both of them.
  return synthetic_apply(fun, arg);
}

bool heap_reducible(heap_operand* heap) {
  word i = 0; word exp = 1; u8 offset = 0; u8 trailing = 0;
  while (i < heap->length && exp) {
    const word crumbs = heap->crumbstring[i];
    offset = reachesZero(exp, crumbs);
    if (offset) {
      return !!redexes(trailing, crumbs);
    } else {
      if (redexes(trailing, crumbs))
        return true;
    }
    exp -= deltaExpecting(crumbs);
    trailing = ctz(crumbs);
    i++;
  }
  return false;
}

bool could_be_reducible(operand op) {
  if (isimmediate(op)) {
    return !!redexes(0, op.crumbs);
  } else if (issharedheap(op)) {
    return heap_reducible(getsharedheap(op));
  }
  // For synth nodes: just assume yes rather than recursing
  return true;
}

////////////////////////////////////////////////////////////////////////////////
// Machinery for computing reductions and managing input and output           //
////////////////////////////////////////////////////////////////////////////////


// Fuel limits the explicit work done, in the evaluation loop
u64 fuel = ~4;
// Work is the number of reductions that it takes to get to what was produced,
// possibly out of order of the nominal lazy evaluation
u64 work = 0;

// Scan a crumbstring to find one operand
__attribute__((always_inline))
ptrbit _scan1(const word crumbstring[], ptrbit _input_at, const ptrbit _stop_at) {
  goodptrbit(_input_at);
  goodptrbit(_stop_at);
  const ptrbit failed = _input_at;
  // adjust the expected number of nodes by the leading nonzeros *crumbs*
  // we will add (could use leading zeros, but this seems more robust to
  // optimizations in `reachesZero()`)
  word exp = 1 + (_input_at.bit >> 1);

  #pragma unroll(2)
  while (_input_at.ptr < _stop_at.ptr || (_input_at.ptr == _stop_at.ptr && _input_at.bit <= _stop_at.bit)) {
    assert(ptrbit2crumbs(_input_at) <= ptrbit2crumbs(_stop_at));
    goodptrbit(_input_at);
    // Fill the leading bits with ones (first loop iteration only)
    word crumbs = word_of(crumbstring, _input_at) | mask_hi(_input_at.bit);
    if ((_input_at.bit = reachesZero(exp, crumbs))) {
      _input_at.ptr += sizeof(word) * (_input_at.bit / word_size);
      _input_at.bit %= word_size;
      if (_input_at.ptr < _stop_at.ptr || _input_at.bit < _stop_at.bit || (_input_at.ptr == _stop_at.ptr && _input_at.bit == _stop_at.bit)) {
        return _input_at;
      } else {
        return failed;
      }
    }
    // Advance to the start of the next word
    _input_at.ptr += sizeof(word);
    _input_at.bit = 0;
    goodptrbit(_input_at);
    s8 delta = deltaExpecting(crumbs);
    assert(delta <= 0 || exp > delta);
    exp -= delta;
    assert(exp);
  }
  return failed;
}

struct ptrbitop { ptrbit next; operand op; };

__attribute__((always_inline))
static struct ptrbitop scan1op(const word crumbstring[], ptrbit _input_at, const ptrbit _stop_at) {
  const ptrbit next = _scan1(crumbstring, _input_at, _stop_at);
  goodptrbit(next);
  if (RARELY(next.ptr == _input_at.ptr && next.bit == _input_at.bit))
    return (struct ptrbitop) { next, 0 };
  const word crumblen = ptrbit2crumbs(next) - ptrbit2crumbs(_input_at);
  assert(crumblen);
  // This conditional is load-bearing, for `stacked=1` at least
  if (crumblen < word_crumbs && (next.ptr == _input_at.ptr || next.bit == 0)) {
    assert(next.ptr <= _input_at.ptr + sizeof(word));
    const word crumbs = word_of(crumbstring, _input_at) << _input_at.bit;
    assert(reachesZero1(crumbs) == 2*crumblen);
    return (struct ptrbitop) { next, _direct(crumbs & mask_hi(2*crumblen)) };
  } else if (crumblen < word_crumbs) {
    // Saves maybe 10% of allocations, but needs the assertion to not make it
    // slower (i.e., to avoid recomputing the crumb length)
    assert(next.ptr <= _input_at.ptr + sizeof(word) || (next.ptr == _input_at.ptr + 2*sizeof(word) && next.bit == 0));
    const word crumbs = word_cat(
      word_size - _input_at.bit,
        leftbits_of(crumbstring, _input_at),
      (2*crumblen) - (word_size - _input_at.bit),
        nextword_of(crumbstring, _input_at)
    );
    assert(reachesZero1(crumbs) == 2*crumblen);
    return (struct ptrbitop) { next, _direct(crumbs & mask_hi(2*crumblen)) };
  } else {
    // Copy it into its own heap
    shared_operand* sh = alloc_shared_heap(1 + ROUNDUPDIV(crumblen, word_crumbs));
    const word words = 1 + (next.ptr - _input_at.ptr) / sizeof(word);
    copy_from_unaligned(getheap(sh)->crumbstring, from_ptr(crumbstring, _input_at), _input_at.bit, words);
    // And plop it on the stack
    goodop(shared(sh));
    return (struct ptrbitop) { next, shared(sh) };
  }
}

// Scan `input_words` to find an operand: increments `input_at` instead of
// returning anything (and respects `stop_at`).
static operand scan1input(void) {
  const ptrbit failed = input_at;
  const struct ptrbitop scanned = scan1op(input_words, input_at, stop_at);
  input_at = scanned.next;
  return scanned.op;
}

// When the stack reaches zero, we are allowed to and need to refresh it from
// the input buffer `input_words`, continuing from where we left off. We do not
// just scan the full input buffer at once because then we would have to swap
// the stack from top to bottom.
bool refill(void) {
  assert(stack_top == stack_endstop);

  if (input_at.ptr >= stop_at.ptr && input_at.bit >= stop_at.bit) return false;
  assert(stack_endstop == 0);

  // Scan one operand from `input_words`, so we know how to copy it
  goodptrbit(input_at);
  goodptrbit(stop_at);
  const ptrbit start_at = input_at;
  const operand op = scan1input();
  if (isnullop(op)) {
    // input_at = ptrbit_incr_bits(input_at, 2);
    return false;
  }

  goodop(op);
  stack[stack_top++] = FRAME(op);
  assert(stack_top < stack_length);
  return true;
}

void skip_leading_zeros(void) {
  // Start at the first crumb
  ptrbit input_at = (ptrbit) {};

  // Count the leading zero crumbs
  {
    // Whole words
    while (input_at.ptr < stop_at.ptr && word_of(input_words, input_at) == 0) {
      input_at.ptr += sizeof(word);
    }
    // Remaining crumbs
    input_at.bit = clz(word_of(input_words, input_at)) & ~1;
  }
  goodptrbit(input_at);
}

////////////////////////////////////////////////////////////////////////////////
// Machinery for evaluation (stack management, reductions, WHNF, etc.)        //
////////////////////////////////////////////////////////////////////////////////


// Synthetic application nodes represent an argument to go on the stack, and
// the function, which is the new top of the stack. We can recursively unpack
// synth nodes until the arguments are all on the stack and the last function
// is a heap or immediate operand.
PLEASE_INLINABLE
static operand unpack_synth_onto_stack(const operand op) {
  // We might as well walk the spine of synthetic applications here in a tight
  // loop, instead of waiting for the next `step();`
  operand spine = op;
  goodop(op);
  #pragma unfold(2)
  while (!isimmediate(spine) && !issharedheap(spine)) {
    shared_operand* this = spine.shared;
    if (stack_top > stack_endstop && this->ref_count > 1) {
      stack_was_evaling(stack_top - 1, this);
    }
    // Now we add it onto the stack
    stack[stack_top++] = FRAME(soft_dup(this->synth->arg));
    assert(stack_top < stack_length);
    // And proceed down the spine
    spine = this->synth->fun;
  }
  // Finally, whatever we ended up with is at the top of the stack.
  const operand result = soft_dup(spine);
  // And free the spine (all the leaves were dup'ed, so are safe)
  drop(op);
  return result;
}

// A heap or immediate operand is unpacked onto the stack: the number of leading
// zeros in the crumbstring indicates the number of arguments that will be
// unpacked, and the first crumb is the combinator that will control the
// reduction.
PLEASE_INLINABLE
static operand unpack_crumbstring_onto_stack(operand op) {
  if (isimmediate(op)) {
    if (op.crumbs >> (word_size - 2))
      return op;
    op = shared(as_tmp(op));
  }
  assert(!isimmediate(op) && issharedheap(op));

  heap_operand* heap = getsharedheap(op);
  // Cache the number of bytes
  const word bytes = sizeof(word) * heap->length;
  // Here is the crumbstring
  const word* crumbstring = heap->crumbstring;
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
  assert(arity < stack_length);
  assert(stack_top < stack_length - arity - 1);
  // Also must be nonzero
  assert(arity);

  // Increase stack_top already, and then we will walk back and fill in the
  // arguments on the stack.
  stack_top += arity + 1;
  for (int stacked = 1; stacked <= arity + 1; stacked++) {
    goodptrbit(i);
    // Scan ahead one item
    const struct ptrbitop scanned = scan1op(crumbstring, i, (ptrbit) { .ptr = bytes, .bit = 0 });
    i = scanned.next;
    assert(!isnullop(scanned.op));
    goodop(scanned.op);
    stack[stack_top - stacked] = FRAME(scanned.op);
  }

  // This is now consumed
  drop(op);

  // Return the head of the applications
  return stack[--stack_top].to_eval;
}

// Now that the head is a known combinator, the underapplied nodes formed by
// it and its first couple arguments on the stack (not meeting its arity) are
// in Weak Head Normal Form, so we need to update `was_evaling` for them.
PLEASE_INLINABLE
static void save_whnf_on_stack(word crumbs) {
  const u8 crumb = crumbs >> (word_size - 2);
  assert(crumb > 0);
  assert(crumb <= 3);

  // Scan the stack to see how many we need to save, up to one less than
  // the arity of this node (since we are evaling it applied to the arity)
  u8 was_evaled = 0;
  #pragma unroll(3)
  for (u8 i=1; i <= stack_top - stack_endstop && i < crumb; i++) {
    if (stack[stack_top - i].was_evaling != NULL) {
      // If ref_count was zero, it should have been freed and `was_evaling`
      // removed from the stack
      assert(stack[stack_top - i].was_evaling->ref_count > 0);
      was_evaled = i;
    }
  }

  operand evaled = _direct(crumbs);
  #pragma unroll(3)
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

// Perform a reduction according to whatever the head combinator is.
//
// `Ix = x`
// `Kxy = x`
// `Sxyz = 00xz0yz = ((xz)(yz))`
PLEASE_INLINABLE
static bool reduction_rule(word crumbs) {
  assert(stack_top < stack_length);

  const u8 crumb = crumbs >> (word_size - 2);
  assert(crumb > 0);
  assert(crumb <= 3);

  // This is the procedure to pop an item from the stack: if the stack is
  // empty, we try to refill it from the input buffer, and if we cannot, then
  // we have to return. If it succeeded, we save it to the given variable.
  #define POP_OR(lvalue, stmt) BLOCK( \
    assert(stack[stack_top].was_evaling == NULL); \
    if (RARELY(stack_top <= stack_endstop && !refill())) { \
      stmt; __builtin_trap(); \
    }; \
    lvalue = stack[--stack_top].to_eval; \
    assert(stack_top >= stack_endstop); \
    goodop(stack[stack_top].to_eval); \
  )
  // Now we handle the particular opcodes: I=1, K=2, S=3
  if (crumb == 3) {
    // Get three arguments
    operand x, y, z;
    POP_OR(x, { stack_top += 1; return false; });
    POP_OR(y, { stack_top += 2; return false; });
    POP_OR(z, { stack_top += 3; return false; });
    // `Sxyz = 00xz0yz = ((xz)(yz))`

    // `SKyz = Kz(yz) = z`
    if (isK(x)) {
      drop(x); drop(y);
      stack[stack_top++] = FRAME(z);
      work++; work++;
      return true;
    }
    operand yz = fast_unconstant(y);
    if (isnullop(yz)) {
      operand xz = fast_unconstant(x);
      if (isnullop(xz)) {
        // Increase the ref count on `z`
        z = dup(z);
        // Put `x`, `z`, and the synthetic `(yz)` on the stack
        yz = apply(y, z);
      } else {
        // `S(Kx)yz = Kxz(yz) = x(yz) = Bxyz`
        yz = apply(y, z);
        stack[stack_top++] = FRAME(yz);
        stack[stack_top++] = FRAME(xz);
        work++; work++;
        return true;
      }
    } else {
      // `Sx(Ky)z = (xz)(Kyz) = xzy`
      work++;
    }
    stack[stack_top++] = FRAME(yz);
    stack[stack_top++] = FRAME(z);
    if (isI(x)) {
      drop(x);
      work++;
    } else {
      stack[stack_top++] = FRAME(x);
    }
    assert(stack_top < stack_length);
    work++;
    // Evaluation will continue with `x` on the next `step()`!
    return true;
  } else if (crumb == 2) {
    // Get two arguments
    operand x, y;
    POP_OR(x, { stack_top += 1; return false; });
    // TODO: tell `refill()` that it does not need to actually copy this operand?
    POP_OR(y, { stack_top += 2; return false; });

    // Drop `y` and put `x` back onto the stack: `Kxy = x`
    drop(y);
    if (isI(x) && stack_top > stack_endstop) {
      drop(x);
      work++;
    } else {
      stack[stack_top++] = FRAME(x);
    }
    assert(stack_top < stack_length);
    work++;
    return true;
  } else if (crumb == 1) {
    // Check that there is an argument available; otherwise, we need to restore
    // the head operand back to the stack
    if (RARELY(stack_top <= stack_endstop && !refill())) { stack_top += 1; return false; }

    // Just drop this identity node `op`, good riddance
    // (Morally we pop the argument and push it back onto the stack.)
    work++;
    return true;
  }
  __builtin_unreachable();
}

// Evaluate one operand at the top of the stack. If it is a known combinator
// `S`, `K`, or `I` then we can apply it and compute a reduction (and if it
// ends up being underapplied and thus we are in WHNF). If not, it is an
// application node that we have to unpack onto the stack and recurse into the
// function side to continue evaluating on the next `step();`
bool step(void) {
  if (RARELY(!stack_top && !refill())) return false; // all done

  assert(stack_top > stack_endstop);
  assert(stack_top < stack_length);

  operand op = stack[--stack_top].to_eval;
  assert(stack[stack_top].was_evaling == NULL);
  goodop(op);

  // Do some setup for shared nodes
  if (!isimmediate(op)) {
    // If it is `S`, `K`, or `I` that ended up on the heap for some reason
    // (e.g. because it was shared at one point), unpack it.
    if (
      issharedheap(op) &&
        (getsharedheap(op)->crumbstring[0] >> (word_size - 2)) != 0
    ) {
      const word crumb = getsharedheap(op)->crumbstring[0] & mask_hi(2);
      drop(op);
      // We set it back on the stack in the case that we are out of input crumbs
      // and are thus in WHNF, so we revert the stack to its original size before
      // this function was called
      stack[stack_top].to_eval = op = _direct(crumb);
    }

    // Try to keep at least one thing on the stack to make sure we have
    // somewhere to pin `was_evaled`. But if we ran out of input, that is fine:
    // it is the final node to evaluate anyways.
    else if (op.shared->ref_count > 1 && (stack_top > stack_endstop || refill())) {
      stack_was_evaling(stack_top - 1, op.shared);
    }
  }

  PLEASE_INLINE_FOLLOWING_STATEMENT
  // Walk down the tree of synth nodes until we end up at a heap or immediate
  op = unpack_synth_onto_stack(op);

  assert(stack_top < stack_length);

  assert(isimmediate(op) || issharedheap(op));
  stack[stack_top].to_eval = op;
  if (RARELY(issharedheap(op) && op.shared->ref_count > 1 && (stack_top > stack_endstop || refill()))) {
    stack_was_evaling(stack_top - 1, op.shared);
  }

  PLEASE_INLINE_FOLLOWING_STATEMENT
  // Unpack applications from a crumbstring, leaving the combinator at the head
  op = unpack_crumbstring_onto_stack(op);

  // What is left should be a bare combinator
  assert(isimmediate(op));
  const u8 crumb = op.crumbs >> (word_size - 2);
  assert(crumb > 0);
  assert(crumb <= 3);

  PLEASE_INLINE_FOLLOWING_STATEMENT
  // The first `0 .. crumb-1` applications are now evaluated to WHNF, so their
  // evaluations on the stack need to be handled
  save_whnf_on_stack(op.crumbs);

  PLEASE_INLINE_FOLLOWING_STATEMENT
  // Finally we apply the reduction indicated by the combinator, I=1, K=2, S=3
  return reduction_rule(op.crumbs);
}

// Read a term from the stack, with an assertion that it is in Weak Head Normal
// Form (if fuel is nonzero). This means that the head term (at the top of the
// stack) is a single combinator, and the total stack size is less than or equal
// to its value.
PLEASE_INLINABLE
operand read_whnf_from_stack(void) {
  assert(stack_top > stack_endstop);

  operand op = stack[--stack_top].to_eval;
  if (fuel) {
    // WHNF
    assert(isimmediate(op));
    u8 arity = op.crumbs >> (word_size - 2);
    assert(arity);
    assert(stack_top < stack_endstop + arity);
    assert(stack[stack_top].was_evaling == NULL);
    stack[stack_top].was_evaling = NULL;
  } else {
    // if (stack[stack_top].was_evaling)
    //   op = have_evaled(stack[stack_top].was_evaling, op);
  }
  while (stack_top > stack_endstop) {
    --stack_top;
    // if (stack[stack_top].was_evaling)
    //   op = have_evaled(stack[stack_top].was_evaling, op);
    stack[stack_top].was_evaling = NULL;
    op = synthetic_apply(op, stack[stack_top].to_eval);
  }
  assert(stack_top == stack_endstop);
  goodop(op);
  return op;
}

// Evaluate an operand to Weak Head Normal Form by running it on the stack
// until the step returns no reductions (or fuel runs out). This takes over the
// stack so it must be called with an empty stack. (An alternate option would
// be to save a stack pointer as a new underflow point.)
__attribute__((always_inline))
operand whnf_op(operand op) {
  goodop(op);
  if (!fuel) return op;
  if (!could_be_reducible(op)) {
    return op;
  }
  assert(stack_top == stack_endstop);
  stack[stack_top++] = FRAME(op);
  assert(stack_top < stack_length);

  while (fuel && step()) {
    fuel--;
  };

  PLEASE_INLINE_FOLLOWING_STATEMENT
  return read_whnf_from_stack();
}

// Convert a Weak Head Normal Form term to Normal Form, by recursively
// normalizing the arguments too.
void whnf2nf_op(operand op) {
  goodop(op);
  if (!fuel || isimmediate(op) || issharedheap(op)) return;

  whnf2nf_op(op.shared->synth->fun);
  op.shared->synth->arg = whnf_op(op.shared->synth->arg);
  TAILCALL return whnf2nf_op(op.shared->synth->arg);
}

// Write out an operand into the output buffer. This is an important operation
// because it gives the semantics of operands as crumbstrings! ^^
void write_out(operand op) {
  goodop(op);
  goodptrbit(output_at);
  assert(output_at.ptr < output_words_length * sizeof(word));
  if (isimmediate(op)) {
    const u8 len = reachesZero1(op.crumbs);
    copy_to_unaligned(from_ptr(output_words, output_at), &op.crumbs, output_at.bit, 1, false);
    output_at = ptrbit_incr_bits(output_at, len);
  } else if (issharedheap(op)) {
    heap_operand* heap = getsharedheap(op);
    assert(output_at.ptr + heap->length < output_words_length * sizeof(word));
    const word len = bitlength(heap->crumbstring, heap->length);
    copy_to_unaligned(from_ptr(output_words, output_at), heap->crumbstring, output_at.bit, heap->length, false);
    assert(word_of(output_words, output_at) != 0);
    // assert(word_of(output_words, output_at) == op.shared->heap->crumbstring[0]);
    output_at = ptrbit_incr_bits(output_at, len);
    drop(op);
  } else {
    // Write a zero crumb `0q0` (application)
    // (we may have written other bits to the word already, from hapless copying)
    word_of(output_words, output_at) &= mask_hi(output_at.bit);
    output_at = ptrbit_incr_bits(output_at, 2);
    // Write each operand (while negating the effect of `write_out` dropping it)
    write_out(soft_dup(op.shared->synth->fun));
    operand arg = soft_dup(op.shared->synth->arg);
    drop(op);
    TAILCALL return write_out(arg);
  }
  assert(output_at.ptr < output_words_length * sizeof(word));
}

void write_nf_from_whnf_stack(void) {
  int our_endstop = stack_endstop;
  assert(stack_top > stack_endstop);

  assert(output_at.ptr < output_words_length * sizeof(word));
  word_of(output_words, output_at) &= mask_hi(output_at.bit);
  size_t i = output_at.ptr/sizeof(word);
  output_at = ptrbit_incr_bits(output_at, 2 * (stack_top - stack_endstop - 1));
  goodptrbit(output_at);
  for (i++; i <= output_at.ptr/sizeof(word); i++) {
    output_words[i] = 0;
  }
  write_out(stack[stack_endstop = --stack_top].to_eval);

  if (stack_endstop == our_endstop) return;
  while (stack_endstop > our_endstop) {
    stack_top = stack_endstop--;

    // stack[stack_endstop].was_evaling = NULL; // TODO?

    // This check is a wash on x86_64 and slows down WASM a lot, even though it
    // results in `write_out` being called less?
    if (could_be_reducible(stack[stack_endstop].to_eval))
    while (fuel && step()) {
      fuel--;
      assert(stack_top > stack_endstop);
      assert(stack_top < stack_length);
    };

    if (stack_endstop == our_endstop) break; // for tail call

    write_nf_from_whnf_stack();
    assert(stack_top == stack_endstop);
  }
  assert(output_at.ptr < output_words_length * sizeof(word));
  TAILCALL return write_nf_from_whnf_stack();
}


////////////////////////////////////////////////////////////////////////////////
// Main API surface: single-shot, streaming, and CLI entrypoint               //
////////////////////////////////////////////////////////////////////////////////

// The full evaluation process: read `input_crumbs` out of `input_words`,
// evaluate it (using the stack for all intermediaries), and write out the
// normalized result to `output_words`, returning the `output_crumbs` that were
// written.
word eval(const word input_crumbs) {
  if (!input_crumbs) return 0;
  input_at = (ptrbit) {};
  stop_at = crumbs2ptrbit(input_crumbs);
  output_at = (ptrbit){};

  stack_top = 0;
  skip_leading_zeros();
  if (!refill()) return 0;
  assert(stack_top);

  while (fuel && step()) {
    fuel--;
    assert(stack_top < stack_length);
  };

  write_nf_from_whnf_stack();

  return ptrbit2crumbs(output_at);
}

word eval1word(const word crumbs) {
  assert(reachesZero1(crumbs));

  input_at = (ptrbit) {};
  stop_at = input_at;
  output_at = (ptrbit){};
  stack_top = 1;
  stack[0] = FRAME(direct(crumbs));

  while (fuel && step()) {
    fuel--;
    assert(stack_top < stack_length);
  };

  operand op = read_whnf_from_stack();
  whnf2nf_op(op);
  output_at = (ptrbit){};
  write_out(op);
  assert(ptrbit2crumbs(output_at) <= word_crumbs);
  return output_words[0];
}

// Set up the streaming interface, read `input_crumbs` out of `input_words` and
// place it on the stack, but performing no evaluation.
bool setup(const word input_crumbs) {
  if (!input_crumbs) return false;
  input_at = (ptrbit) {};
  stop_at = crumbs2ptrbit(input_crumbs);
  output_at = (ptrbit) {};

  stack_top = 0;
  if (!refill()) return false;
  assert(stack_top);
  assert(input_at.ptr == stop_at.ptr && input_at.bit == stop_at.bit);

  return true;
}

// Resume evaluation, resetting the fuel. Returns true if it is finished
// (may be off by one).
bool resume(const word drip_fuel) {
  assert(drip_fuel);
  fuel = drip_fuel;
  while (fuel && step()) {
    fuel--;
    assert(stack_top < stack_length);
  };

  if (!fuel) return false;

  operand op = read_whnf_from_stack();
  whnf2nf_op(op);
  assert(stack_top == 0);
  // Place it back on the stack to remember it for the next `resume()`
  stack[0] = FRAME(op);
  stack_top = 1;

  return fuel != 0;
}

// Write out the current value of the stack.
word finalize(void) {
  if (!stack_top) return 0;
  assert(stack_endstop == 0);

  output_at = (ptrbit) {};
  operand op;
  if (stack_top == 1) {
    op = stack[0].to_eval;
    stack_top = 0;
  } else {
    op = read_whnf_from_stack();
  }
  goodop(op);
  write_out(op);

  return ptrbit2crumbs(output_at);
}

void cancel(void) {
  do {
    drop(stack[stack_top].to_eval);
  } while (stack_top && stack_top--);
}

// #ifdef CLI
// #else
int main (int argc, char *argv[]) {
  if (argc <= 1) return 2;

  word i=0;
  for (; argv[1][i] != '\0'; i++) {
    switch (argv[1][i]) {
      case '0': continue;
      case '1':
      case '2':
      case '3': {
        word offset = 2*(word_crumbs - 1 - (i % word_crumbs));
        word set_bit = ((word) (argv[1][i] - '0')) << offset;
        input_words[i / word_crumbs] |= set_bit;
        break;
      }
      default:
        return 3;
    }
  }

  char buf[SZ] = {0};
  word output = eval(i);
  word j=0;
  for (; j<output; j++) {
    buf[j] = '0' + (0b11 & (output_words[j / word_crumbs] >> 2*(word_crumbs - 1 - (j % word_crumbs))));
  }
  buf[j] = '\0';

  if (argc >= 3) {
    for (word k=0; k<SZ; k++) {
      if (argv[2][k] != buf[k]) return 4;
      if (buf[k] == '\0') break;
    }
  }

  return 0;
}
// #endif

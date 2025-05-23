#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include "cuapteryx.h"

////////////////////////////////////////////////////////////////////////////////
// Externs (using walloc.c)                                                   //
////////////////////////////////////////////////////////////////////////////////

void *malloc(size_t size) __attribute__((warn_unused_result, alloc_size(1), noinline));
void free(void *p) __attribute__((noinline));

__attribute__((import_name("trace")))
extern void trace(void*, char*, char*, word);

#undef assert
#define assert(c) BLOCK(if (!(c)) {print64("## Assert failed: " #c ", at line", __LINE__); __builtin_trap(); __builtin_unreachable();})

#define dbg(e) BLOCK(print64(#e, e);)

word _dbg_word;
word _dbg1_word;
word _dbg2_word;

////////////////////////////////////////////////////////////////////////////////
// More basic helpers                                                         //
////////////////////////////////////////////////////////////////////////////////

// Divide, rounding up
#define ROUNDUPDIV(num, denom) ((num + (denom - 1)) / denom)

// Mask of the specified high bits
word mask_hi(u8 hi_bits) {
  return ~(word_max >> hi_bits);
}
// Mask of the specified low bits
word mask_lo(u8 lo_bits) {
  return ~(word_max << lo_bits);
}
// Concatenate the high `hi_bits` of `hi` and the high `lo_bits` of `lo` into
// the high bits of a word (i.e. all left-aligned).
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
  dent(1);
  dbg(words);
  dbg(offset);
  assert(offset != 0); // messes with word_cat
  word hanging = clobber ? 0 : *dst;
  dbg(hanging);
  while (words--) {
    // dbg(*src);
    *dst = word_cat(offset, hanging, word_size - offset, *src);
    // dbg(*dst);
    hanging = *src << offset;
    // dbg(hanging);
    dst++;
    src++;
  }
  if (hanging) *dst = hanging;
  dent(-1);
}
void copy_from_unaligned(word* dst, const word* src, u8 offset, word words) {
  if (words == 0) return;
  assert(offset < word_size);
  if ((offset % 8) == 0) {
    __builtin_memcpy(dst, src + (offset / 8), sizeof(word) * words - (offset / 8));
    return;
  }
  word last = *src;
  words++; // ??
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
  dent(1);
  _dbg_word = known_words;
  // Quickly scan through `known_words - 1`, using just
  // `deltaExpecting` and not `reachesZero`
  // if (known_words) while (i < known_words - 1) {
  //   _dbg1_word = i;
  //   exp -= deltaExpecting(crumbstring[i++]);
  //   assert(exp != 0);
  // }
  // Fall through, in case `known_words` was too short.
  // (This is used in `ap_heap_cat`.)
  while (!offset) {
    dbg(exp);
    dbg(crumbstring[i]);
    assert(i < 100);
    _dbg1_word = i;
    // See if it reaches zero during the current word
    // print64("reachesZero", reachesZero(exp, crumbstring[i]));
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
  dent(-1);
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
typedef struct operand {
  union {
    struct shared_operand* shared;
    word crumbs;
  };
  bool immediate;
} operand;
operand _dbg_op1;
operand _dbg_op2;
operand _dbg_op3;

// A shared operand is ref-counted and used for sharing work from lazy evaluation.
// The pointer it stores is updated after it is evaluated, and `work` increased.
typedef struct shared_operand {
  int ref_count;
  word work;
  union {
    struct synthetic_operand* synth;
    struct heap_operand* heap;
  };
  bool on_heap;
  // These bound indices of weakrefs `was_evaling` on the stack, so they can
  // be zeroed out when this `shared_operand` is freed. If they are already
  // zeroed out or otherwise replaced, this is fine!
  int weakref_stack_min;
  int weakref_stack_max;
} shared_operand;
shared_operand* _dbg_shared;

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
heap_operand* _dbg_heap;


// A synthetic apply node, produced during evaluation of `S`. It counts as a
// single reference to its operands while it is alive. It represents the
// crumbstring `0(fun)(arg)`.
typedef struct synthetic_operand {
  operand fun;
  operand arg;
} synthetic_operand;
synthetic_operand* _dbg_synth;


operand direct(const word crumbs) {
  return (operand) { .immediate = true, .crumbs = crumbs };
}
operand shared(shared_operand* shared) {
  return (operand) { .immediate = false, .shared = shared };
}

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


////////////////////////////////////////////////////////////////////////////////
// Some temporaries, used for greasing the wheels of algorithms (avoiding     //
// special cases)                                                             //
////////////////////////////////////////////////////////////////////////////////

heap_operand heap_tmp1 = { .length = 1, .crumbstring = {0} };
heap_operand heap_tmp2 = { .length = 1, .crumbstring = {0} };
shared_operand shared_tmp = { .ref_count = ~1, .work = 0, .on_heap = true, .heap = &heap_tmp1 };

heap_operand* as_tmp1(operand l) {
  if (l.immediate) {
    heap_tmp1.crumbstring[0] = l.crumbs;
    trace(&heap_tmp1, "heap_tmp1", "", 0);
    return &heap_tmp1;
  }
  assert(l.shared->on_heap);
  return l.shared->heap;
}
heap_operand* as_tmp2(operand r) {
  if (r.immediate) {
    heap_tmp2.crumbstring[0] = r.crumbs;
    trace(&heap_tmp2, "heap_tmp2", "", 0);
    return &heap_tmp2;
  }
  assert(r.shared->on_heap);
  return r.shared->heap;
}
shared_operand* as_tmp(operand term) {
  trace(&shared_tmp, "shared_tmp", "refc was:", shared_tmp.ref_count);
  shared_tmp.ref_count = ~1; // never de-allocate it
  shared_tmp.work = 0; // reset
  shared_tmp.on_heap = 1;
  shared_tmp.heap = as_tmp1(term);
  dbg(term.immediate);
  dbg(term.crumbs);
  dbg(shared_tmp.heap->crumbstring[0]);
  return &shared_tmp;
}

////////////////////////////////////////////////////////////////////////////////
// Memory management and other fundamental operations on operands             //
////////////////////////////////////////////////////////////////////////////////

void goodop(operand op) {
  if (op.immediate) {
    assert(reachesZero(1, op.crumbs));
  } else {
    // trace(op.shared, "good", op.shared->on_heap ? "heap" : "synth", op.shared->ref_count);
    assert(op.shared != NULL);
    assert(((word)op.shared & 0xFFFF0000) != 0xDEAD0000);
    assert(op.shared->weakref_stack_max < SZ);
    assert(op.shared->weakref_stack_min < SZ);
    if (op.shared->on_heap) {
      assert(((word)op.shared->heap & 0xFFFF0000) != 0xDEAD0000);
      assert(op.shared->heap->length > 0);
    } else {
      assert(((word)op.shared->synth & 0xFFFF0000) != 0xDEAD0000);
      goodop(op.shared->synth->arg);
      goodop(op.shared->synth->fun);
    }
    assert(op.shared->ref_count < 0x6789);
    assert(op.shared->work < 0x123434223);
  }
}

word op_trace(operand op) {
  return op.immediate ? op.crumbs : (word)op.shared;
}
void trace_op(char* lbl, operand op) {
  if (op.immediate) {
    trace(0, lbl, "immediate", op.crumbs);
  } else if (op.shared->on_heap) {
    trace(op.shared, lbl, "heap", op.shared->ref_count);
    for (int i=0; i<op.shared->heap->length; i++) trace(op.shared, lbl, "crumb", op.shared->heap->crumbstring[i]);
  } else {
    trace(op.shared, lbl, "synth->fun", op_trace(op.shared->synth->fun));
    trace_op(lbl, op.shared->synth->fun);
    trace(op.shared, lbl, "synth->arg", op_trace(op.shared->synth->arg));
    trace_op(lbl, op.shared->synth->arg);
    trace(op.shared, lbl, "/synth, refc:", op.shared->ref_count);
  }
}

// Create a new shared heap operand, given a wordlength to allocate.
__attribute__((noinline))
shared_operand* alloc_shared_heap(const int wordlength) {
  assert(wordlength);
  // print64("alloc_shared_heap", wordlength);
  heap_operand* on_heap = malloc(sizeof(heap_operand) + sizeof(word[wordlength]));
  on_heap->length = wordlength;
  shared_operand* shared = malloc(sizeof(shared_operand));
  shared->ref_count = 1;
  shared->work = 0;
  shared->on_heap = true;
  shared->heap = on_heap;
  trace(shared, "alloc_shared_heap (refc 1)", "length:", wordlength);
  trace(shared, "alloc_shared_heap (refc 1)", "heap:", (word)shared->heap);
  return shared;
}

bool drop(operand op); // forward declaration

// Decrement the reference count and de-allocate it if it reached 0
bool drop_shared(shared_operand* shared) {
  goodop((operand) { shared, false });
  if (shared->ref_count == 0) {
    trace(shared, "drop(shared) ERR", shared->on_heap ? "(heap), refc:" : "(synth), refc:", shared->ref_count);
    assert(shared->ref_count != 0);
  }
  if (!--shared->ref_count) {
    // if (shared == (void*)132192) assert(0);
    trace(shared, "drop: free(shared)", shared->on_heap ? "(heap), weakref_max:" : "(synth), weakref_max:", shared->weakref_stack_max);
    if (shared->on_heap) {
      // free(shared->heap);
      // shared->heap = (void*)0xDEADBEEF;
    } else {
      if (!shared->synth->fun.immediate) trace(shared->synth->fun.shared, "recursive drop", "fun of", (word)shared);
      drop(shared->synth->fun);
      if (!shared->synth->arg.immediate) trace(shared->synth->arg.shared, "recursive drop", "arg of", (word)shared);
      drop(shared->synth->arg);
      // free(shared->synth);
      // shared->synth = (void*)0xDEADDEAD;
    }
    if (shared->weakref_stack_max != ~0 || shared->weakref_stack_min != ~0) {
      assert(shared->weakref_stack_max != ~0 && shared->weakref_stack_min != ~0);
      assert(shared->weakref_stack_max >= shared->weakref_stack_min);
      for (int i = shared->weakref_stack_max; i >= shared->weakref_stack_min; i--) {
        if (stack[i].was_evaling == shared) stack[i].was_evaling = NULL;
      }
    }
    // free(shared);
    return true;
  }
  trace(shared, "drop(shared)", shared->on_heap ? "(heap), refc:" : "(synth), refc:", shared->ref_count);
  return false;
}

// Drop a reference if it is a shared operand. Immediate operands are not
// ref counted.
__attribute__((noinline))
bool drop(operand op) {
  goodop(op);
  if (op.immediate) return false;
  shared_operand* shared = op.shared;
  return drop_shared(shared);
}

// The way the `was_evaling` weak ref works is that we re-scan the stack to see
// which weakrefs are still there, and we keep track of the bounds of indices
// to scan (often just one).
void stack_was_evaling(int stack_idx, shared_operand* was_evaling) {
  trace(was_evaling, "stack_was_evaling", was_evaling->on_heap ? "heap" : "synth", was_evaling->on_heap ? (word)was_evaling->heap : (word)was_evaling->synth);
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
__attribute__((noinline))
operand have_evaled(shared_operand* was_evaling, const operand evaled) {
  // Not shared
  if (was_evaling == NULL) return evaled;
  trace(was_evaling, "have_evaled", "refc:", was_evaling->ref_count);
  // Not shared anymore
  if (drop_shared(was_evaling)) return evaled;

  print64("have_evaled", (word)was_evaling);

  if (evaled.immediate) {
    trace(was_evaling, "have_evaled immediate", "", evaled.crumbs);
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
    trace(was_evaling, "have_evaled immediate", "heap:", (word)was_evaling->heap);
    was_evaling->heap->length = 1;
    was_evaling->heap->crumbstring[0] = evaled.crumbs;
  } else {
    // Will not actually happen but that is okay
    if (evaled.shared == was_evaling) return evaled;

    // Free the stale data (unevaluated/less evaluated value)
    if (was_evaling->on_heap) {
      trace(was_evaling, "have_evaled->heap", "", (word)was_evaling->heap);
      free(was_evaling->heap);
    } else {
      trace(was_evaling, "have_evaled->synth", "", (word)was_evaling->synth);
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
    if (!nontrivial_redexes(term.crumbs)) return term;
    // Otherwise we need to make a shared operand
    shared_operand* sh = alloc_shared_heap(1);
    trace(sh, "dup immediate", "(refc 2)", term.crumbs);
    print64("dup for shared", (word)sh);
    sh->heap->crumbstring[0] = term.crumbs;
    assert(bitlength(sh->heap->crumbstring, 0) <= word_size);
    sh->ref_count = 2;
    return shared(sh);
  }
  // If it is already shared, we just need to increment the `ref_count`
  term.shared->ref_count++;
  trace(term.shared, "dup shared", "new refc:", term.shared->ref_count);
  return term;
}

// Concatenate two heap operands into a new shared operand of the two applied.
// That is, it starts with a zero crumb (for the application operator), and
// then the crumbs of the first followed by the crumbs of the second, `0{l}{r}`.
__attribute__((noinline))
shared_operand* ap_heap_cat(heap_operand* l, heap_operand* r) {
  trace(l, "ap_heap_cat", "l[0]", l->crumbstring[0]);
  trace(r, "ap_heap_cat", "r[0]", r->crumbstring[0]);
  // FIXME: decrement length if overallocated?
  int wordlength = l->length + r->length + 1;
  shared_operand* result = alloc_shared_heap(wordlength);
  result->heap->length = wordlength;
  word* out = result->heap->crumbstring;
  copy_to_unaligned(out, l->crumbstring, 2, l->length, true);
  assert(l->crumbstring[0] >> 2 == out[0]);
  ptrbit reallength = ptrbitlength(l->crumbstring, l->length - 1);
  assert(reallength.bit != 0);
  // assert(reallength.ptr == sizeof(word) * (l->length - 1));
  reallength = ptrbit_incr_bits(reallength, 2);
  copy_to_unaligned(out + (reallength.ptr / sizeof(word)), r->crumbstring, reallength.bit, r->length, false);

  for (int i=0; i<result->heap->length; i++) trace(result, "ap_heap_cat", "crumb", out[i]);

  dbg(bitlength(out, 0));
  assert(bitlength(out, 0) == 2 + bitlength(l->crumbstring, 0) + bitlength(r->crumbstring, 0));
  return result;
}

// Construct a new operand with `fun` applied to `arg`, via whatever
// representation we guessed is best.
__attribute__((noinline))
operand apply(const operand fun, const operand arg) {
  print64("apply", 2*fun.immediate + 1*arg.immediate);
  goodop(fun); goodop(arg);
  // If we have two direct operands, we can just merge them, if they are small
  // enough.
  if (fun.immediate && arg.immediate && !nontrivial_redexes(fun.crumbs) && !nontrivial_redexes(arg.crumbs)) {
    u8 fun_len = reachesZero(1, fun.crumbs);
    u8 arg_len = reachesZero(1, arg.crumbs);
    if (2 + fun_len + arg_len <= word_size) {
      dbg(word_cat(2, 0, fun_len+arg_len, word_cat(fun_len, fun.crumbs, arg_len, arg.crumbs)));
      return direct(word_cat(2, 0, fun_len+arg_len, word_cat(fun_len, fun.crumbs, arg_len, arg.crumbs)));
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
    if (!fun.immediate) trace(fun.shared, "drop to merge in apply", "fun", 0);
    if (!arg.immediate) trace(arg.shared, "drop to merge in apply", "arg", 0);
    drop(fun); drop(arg);
    print64("ap_heap_cat", (word)result.shared);
    print64("ap_heap_cat[0]", result.shared->heap->crumbstring[0]);
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
  trace(sh, "synthetic ap (refc 1)",
    ap_node->fun.immediate
      ? (ap_node->arg.immediate ? "im im" : ap_node->arg.shared->on_heap ? "im hp" : "im sy")
    : ap_node->fun.shared->on_heap
      ? (ap_node->arg.immediate ? "hp im" : ap_node->arg.shared->on_heap ? "hp hp" : "hp sy")
      : (ap_node->arg.immediate ? "sy im" : ap_node->arg.shared->on_heap ? "sy hp" : "sy sy"),
    (word)ap_node
  );
  trace(sh, "synthetic ap->fun", "", ap_node->fun.immediate ? ap_node->fun.crumbs : (word)ap_node->fun.shared);
  trace(sh, "synthetic ap->arg", "", ap_node->arg.immediate ? ap_node->arg.crumbs : (word)ap_node->arg.shared);
  return shared(sh);
}


////////////////////////////////////////////////////////////////////////////////
// Machinery for computing reductions and managing input and output           //
////////////////////////////////////////////////////////////////////////////////


// Fuel limits the explicit work done, in the evaluation loop
u64 fuel asm("fuel") = 10000;
// Work is the number of reductions that it takes to get to what was produced,
// possibly out of order of the nominal lazy evaluation
u64 work asm("work") = 0;

ptrbit _dbg_ptrbit;

// Scan a crumbstring to find one operand
__attribute__((always_inline))
ptrbit _scan1(const word crumbstring[], ptrbit _input_at, const ptrbit _stop_at) {
  goodptrbit(_input_at);
  ptrbit failed = _input_at;
  // adjust the expected number of nodes by the leading nonzeros *crumbs*
  // we will add (could use leading zeros, but this seems more robust to
  // optimizations in `reachesZero()`)
  word exp = 1 + (_input_at.bit >> 1);
  dent(1);

  // #pragma unroll(2)
  while (_input_at.ptr < _stop_at.ptr || (_input_at.ptr == _stop_at.ptr && _input_at.bit <= _stop_at.bit)) {
    assert(ptrbit2crumbs(_input_at) <= ptrbit2crumbs(_stop_at));
    goodptrbit(_input_at);
    _dbg_ptrbit = _stop_at;
    // Fill the leading bits with ones (first loop iteration only)
    word crumbs = word_of(crumbstring, _input_at) | ~(word_max >> _input_at.bit);
    dbg(exp);
    dbg(crumbs);
    dbg(reachesZero(exp, crumbs));
    if ((_input_at.bit = reachesZero(exp, crumbs))) {
      if (_input_at.ptr < _stop_at.ptr || _input_at.bit < _stop_at.bit || (_input_at.ptr == _stop_at.ptr && _input_at.bit == _stop_at.bit)) {
        dent(-1);
        return _input_at;
      } else {
        print64("scan1 failed", ptrbit2crumbs(_input_at));
        print64("overran", ptrbit2crumbs(_stop_at));
        dent(-1);
        return failed;
      }
    }
    _input_at.ptr += 8;
    _input_at.bit = 0;
    s8 delta = deltaExpecting(crumbs);
    assert(delta <= 0 || exp > delta);
    exp -= delta;
    assert(exp);
  }
  dent(-1);
  return failed;
}

__attribute__((noinline))
ptrbit scan1(const word crumbstring[], ptrbit _input_at, const ptrbit _stop_at) {
  return _scan1(crumbstring, _input_at, _stop_at);
}

// Scan `input_words` to find an operand: increments `input_at` instead of
// returning anything (and respects `stop_at`).
__attribute__((noinline))
bool scan1input() {
  const ptrbit failed = input_at;
  input_at = _scan1(input_words, input_at, stop_at);
  return input_at.ptr != failed.ptr || input_at.bit != failed.bit;
}

// When the stack reaches zero, we are allowed to and need to refresh it from
// the input buffer `input_words`, continuing from where we left off. We do not
// just scan the full input buffer at once because then we would have to swap
// the stack from top to bottom.
__attribute__((noinline))
bool refill() {
  assert(stack_top == 0);

  // Scan one operand from `input_words`, so we know how to copy it
  goodptrbit(input_at);
  ptrbit start_at = input_at;
  if (!scan1input()) {
    print64("no scan", ptrbit2crumbs(input_at));
    return false;
  }
  goodptrbit(input_at);

  const word crumblen = ptrbit2crumbs(input_at) - ptrbit2crumbs(start_at);
  operand op;
  // Already fit into one word
  if (start_at.ptr == input_at.ptr) {
    word crumbs = leftbits_of(input_words, start_at);
    dbg(mask_hi(2*crumblen) & crumbs);
    assert(mask_hi(2*crumblen) & crumbs);
    op = direct(mask_hi(2*crumblen) & crumbs);
  // Stored over adjacent words but fits into one word
  } else if (2*crumblen <= word_size) {
    assert(input_at.ptr == start_at.ptr + sizeof(word));
    assert(input_at.bit <= start_at.bit);
    const word crumbs = word_cat(
      word_size - start_at.bit,
        leftbits_of(input_words, start_at),
      (2*crumblen) - (word_size - start_at.bit),
        nextword_of(input_words, start_at)
    );
    dbg(crumbs);
    op = direct(mask_hi(2*crumblen) & crumbs);
  } else {
    op = shared(alloc_shared_heap(ROUNDUPDIV(crumblen, word_crumbs)));
    copy_from_unaligned(
      op.shared->heap->crumbstring, from_ptr(input_words, start_at),
      start_at.bit, input_at.ptr/sizeof(word) - start_at.ptr/sizeof(word) + 1
    );
    for (int i=0; i<op.shared->heap->length; i++) trace(op.shared, "refill", "crumb", op.shared->heap->crumbstring[i]);
    dbg(bitlength(op.shared->heap->crumbstring, 0));
    assert(bitlength(op.shared->heap->crumbstring, 0) == 2*crumblen);
    // TODO: zero out last of bits
  }

  if (op.immediate && nontrivial_redexes(op.crumbs)) {
    shared_operand* tmp = alloc_shared_heap(1);
    tmp->heap->crumbstring[0] = op.crumbs;
    assert(bitlength(tmp->heap->crumbstring, 0) < word_size);
    op = shared(tmp);
    for (int i=0; i<op.shared->heap->length; i++) trace(op.shared, "refill", "immediate", op.shared->heap->crumbstring[i]);
  }

  goodop(op);
  trace((void*)stack_top, "onto stack", "refc:", op.immediate ? 12345678901234567890ULL : op.shared->ref_count);
  stack[stack_top++] = FRAME(op);
  return true;
}

// Write out an operand into the output buffer. This is an important operation
// because it gives the semantics of operands as crumbstrings! ^^
__attribute__((noinline))
void write_out(const operand op) {
  goodop(op);
  _dbg_op1 = op;
  goodptrbit(output_at);
  if (op.immediate) {
    print64("write_out", op.crumbs);
    const u8 len = reachesZero(1, op.crumbs);
    copy_to_unaligned(from_ptr(output_words, output_at), &op.crumbs, output_at.bit, 1, false);
    output_at = ptrbit_incr_bits(output_at, len);
  } else if (op.shared->on_heap) {
    _dbg_heap = op.shared->heap;
    dbg((word)op.shared->heap);
    print64("write_out...", op.shared->heap->crumbstring[0]);
    dbg(op.shared->heap->length);
    assert(op.shared->heap->length < 2000);
    const word len = bitlength(op.shared->heap->crumbstring, op.shared->heap->length);
    copy_to_unaligned(from_ptr(output_words, output_at), op.shared->heap->crumbstring, output_at.bit, op.shared->heap->length, false);
    assert(word_of(output_words, output_at) != 0);
    // assert(word_of(output_words, output_at) == op.shared->heap->crumbstring[0]);
    output_at = ptrbit_incr_bits(output_at, len);
    if (!op.immediate) trace(op.shared, "drop in write_out", "heap", 0);
    drop(op); // FIXME
  } else {
    // Write a zero crumb `0q0` (application)
    // (we may have written other bits to the word already, from hapless copying)
    word_of(output_words, output_at) &= mask_hi(output_at.bit);
    print64("write_out0", word_of(output_words, output_at));
    output_at = ptrbit_incr_bits(output_at, 2);
    // Write each operand
    write_out(op.shared->synth->fun);
    write_out(op.shared->synth->arg);
    if (!op.immediate) trace(op.shared, "drop in write_out", "synth", 0);
    drop(op); // FIXME
  }
}

// Evaluate one operand at the top of the stack. If it is a known combinator
// `S`, `K`, or `I` then we can apply it and compute a reduction (and if it
// ends up being underapplied and thus we are in WHNF). If not, it is an
// application node that we have to unpack onto the stack and recurse into the
// function side to continue evaluating on the next `step();`
bool step() {
  if (!stack_top && !refill()) return false; // all done

  operand op = stack[--stack_top].to_eval;
  assert(stack[stack_top].was_evaling == NULL);
  goodop(op);

  // Do some setup for shared nodes
  if (!op.immediate) {
    // print64("shared", (word) op.shared);
    // If it is `S`, `K`, or `I` that ended up on the heap for some reason
    // (e.g. because it was shared at one point), unpack it.
    if (
      op.shared->on_heap &&
        (op.shared->heap->crumbstring[0] >> (word_size - 2)) != 0
    ) {
      const word crumb = op.shared->heap->crumbstring[0];
      if (!op.immediate) trace(op.shared, "drop in step", "heap->immediate", 0);
      drop(op);
      // We set it back on the stack in the case that we are out of input crumbs
      // and are thus in WHNF, so we revert the stack to its original size before
      // this function was called
      stack[stack_top].to_eval = op = direct(crumb);
    }

    // Try to keep at least one thing on the stack to make sure we have
    // somewhere to pin `was_evaled`. But if we ran out of input, that is fine:
    // it is the final node to evaluate anyways.
    else if (op.shared->ref_count > 1 && (stack_top || refill())) {
      stack_was_evaling(stack_top, op.shared);
    }
  }

  if (op.immediate) {
    print64("immediate", op.crumbs);
    const u8 crumb = op.crumbs >> (word_size - 2);
    print64("redex?", crumb);

    // First take care of thunks that are still shared, which are now at least
    // in WHNF (weak head normal form: an underapplied S, K, or I).
    if (crumb) {
      assert(crumb <= 3);
      // Scan the stack to see how many we need to save, up to one less than
      // the arity of this node (since we are evaling it applied to the arity)
      u8 was_evaled = 0;
      for (u8 i=1; i <= stack_top && i < crumb; i++) {
        if (stack[stack_top - i].was_evaling != NULL) {
          // If ref_count was zero, it should have been freed and `was_evaling`
          // removed from the stack
          assert(stack[stack_top - i].was_evaling->ref_count > 0);
          was_evaled = i;
        }
      }
      if (was_evaled) {
        operand evaled = op;
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
            if (!evaled.immediate) trace(evaled.shared, "dup for was_evaling:", "", 0);
            have_evaled(stack[stack_top - i].was_evaling, dup(evaled));
            evaled = shared(stack[stack_top - i].was_evaling);
            stack[stack_top - i].was_evaling = NULL;
          }
          // Create a new apply node only if it will be transferred, otherwise
          // it is just a leak/immediately dropped.
          if (i < was_evaled) {
            evaled = apply(evaled, stack[stack_top].to_eval);
          } else {
            break;
          }
        }
      }
    }

    // This is the procedure to pop an item from the stack: if the stack is
    // empty, we try to refill it from the input buffer, and if we cannot, then
    // we have to return. If it succeeded, we save it to the given variable.
    #define POP_OR(lvalue, stmt) BLOCK( \
      if (!stack_top && !refill()) { \
        stmt; __builtin_trap(); \
      }; \
      lvalue = stack[--stack_top].to_eval; \
      assert(stack[stack_top].was_evaling == NULL); \
      goodop(stack[stack_top].to_eval); \
    )
    // Now we handle the particular opcodes: I=1, K=2, S=3
    if (crumb == 1) {
      // Check that there is an argument available; otherwise, we need to restore
      // the head operand back to the stack
      if (!stack_top && !refill()) { stack_top += 1; return false; }

      // Just drop this identity node, good riddance
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
      if (!y.immediate) trace(y.shared, "drop in step", "K", 0);
      drop(y);
      stack[stack_top++] = FRAME(x);
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
      if (!z.immediate) trace(z.shared, "dup for S:", "", 0);
      z = dup(z);
      // Put `x`, `z`, and the synthetic `(yz)` on the stack
      operand yz = apply(y, z);
      stack[stack_top++] = FRAME(yz);
      stack[stack_top++] = FRAME(z);
      stack[stack_top++] = FRAME(x);
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
    trace_op("spine", spine);
    goodop(op);
    // #pragma unfold(2)
    while (!spine.immediate && !spine.shared->on_heap) {
      shared_operand* this = spine.shared;
      print64("spine", (word)spine.shared);
      if (stack_top && this->ref_count > 1) {
        stack_was_evaling(stack_top, spine.shared);
      }
      // Now we add it onto the stack
      if (!this->synth->arg.immediate) trace(this->synth->arg.shared, "dup for stack:", "", 0);
      trace((void*)stack_top, "onto stack (arg)", "refc:", this->synth->arg.immediate ? 12345678901234567890ULL : this->synth->arg.shared->ref_count);
      stack[stack_top++] = FRAME(dup(this->synth->arg));
      // And proceed down the spine
      spine = this->synth->fun;
    }
    // Finally, whatever we ended up with is at the top of the stack.
    if (!spine.immediate) trace(spine.shared, "dup for stack head:", "", 0);
    trace((void*)stack_top, "onto stack (arg)", "refc:", spine.immediate ? 12345678901234567890ULL : spine.shared->ref_count);
    stack[stack_top++] = FRAME(dup(spine));
    // And free the spine (all the arguments were dup'ed, so are safe)
    if (!op.immediate) trace(op.shared, "drop in step", "spine", 0);
    drop(op); // FIXME
    return true;
  }

  // Otherwise we have some applications waiting to unpack from a crumbstring
  if (op.shared->on_heap) {
    _dbg_heap = op.shared->heap;
    // Cache the amount of bytes
    const int bytes = sizeof(word) * op.shared->heap->length;
    // Here is the crumbstring
    word* crumbstring = op.shared->heap->crumbstring;
    // Start at the first crumb
    ptrbit i = (ptrbit) {};
    dbg(&shared_tmp == op.shared);

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
    dbg(arity);

    // I guess it cannot be too large to fit on the stack
    assert(arity == (int)arity);
    assert(arity + stack_top < sizeof(stack) / sizeof(struct stack_frame));
    // Also must be nonzero
    assert(arity);

    // Increase stack_top already, and then we will walk back and fill in the
    // arguments on the stack.
    stack_top += arity + 1;
    dent(1);
    for (int stacked = 1; stacked <= arity + 1; stacked++) {
      goodptrbit(i);
      const ptrbit last = i;
      print64("crumbstring", crumbstring[last.ptr/sizeof(word)]);
      // Scan ahead one item
      i = scan1(crumbstring, last, (ptrbit) { .ptr = bytes, .bit = 0 });
      const int crumblen = ptrbit2crumbs(i) - ptrbit2crumbs(last);
      assert(crumblen);
      print64("crumblen", crumblen);
      if (crumblen < word_crumbs && i.ptr == last.ptr) {
        stack[stack_top - stacked] = FRAME(direct(word_of(crumbstring, last) << last.bit));
        print64("immediate", stack[stack_top - stacked].to_eval.crumbs);
        print64("at", stack_top - stacked);
      } else {
        // Copy it into its own heap
        shared_operand* sh = alloc_shared_heap(ROUNDUPDIV(crumblen, word_crumbs));
        copy_from_unaligned(sh->heap->crumbstring, from_ptr(crumbstring, last), last.bit, sh->heap->length);
        for (int i=0; i<op.shared->heap->length; i++) trace(op.shared, "step", "crumb", op.shared->heap->crumbstring[i]);
        // And plop it on the stack
        stack[stack_top - stacked] = FRAME(shared(sh));
        goodop(shared(sh));
      }
    }
    dent(-1);
    if (!op.immediate) trace(op.shared, "drop in step", "unpacked heap", 0);
    drop(op);
    return true;
  }
  return false;
}

// The full evaluation process: read `input_crumbs` out of `input_words`,
// evaluate it (using the stack for all intermediaries), and write out the
// normalized result to `output_words`, returning the `output_crumbs` that were
// written.
word eval(word input_crumbs) {
  if (!input_crumbs) return 0;
  input_at = (ptrbit) {};
  stop_at = crumbs2ptrbit(input_crumbs);
  output_at = (ptrbit){};

  print64("fuel0", fuel);
  refill();
  while (fuel-- && step()) {
    print64("fuel", fuel);
  };
  print64("-->", fuel);

  assert(stack_top);

  operand op = stack[--stack_top].to_eval;
  stack[stack_top].was_evaling = NULL;
  while (stack_top) {
    op = apply(op, stack[--stack_top].to_eval);
    stack[stack_top].was_evaling = NULL;
  }
  goodop(op);
  write_out(op);

  return ptrbit2crumbs(output_at);
}

#include <stdbool.h>
#include <stdint.h>

#define BLOCK(stmt) { do{ stmt } while(0); }
#define assert(c) BLOCK(if (!(c)) {__builtin_trap(); __builtin_unreachable();})

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint8_t u8;
typedef int64_t s64;
typedef int32_t s32;
typedef int8_t s8;

typedef u64 word;
typedef u32 ptr; // memory64?

// -mmultivalue -Xclang -target-abi -Xclang experimental-mv
typedef struct { u64 x; u64 y; } u64u64;
typedef struct { u32 x; u64 y; } u32u64;
typedef struct { u32 x; u32 y; } u32u32;
typedef struct { u8 x; u8 y; } u8u8;

typedef struct { ptr ptr; u8 bit; } ptrbit;

// Constants
static const u64 word_size = 8*sizeof(word);
static const u64 word_crumbs = word_size/2;
static const u64 word_max = ~((word)0);
static const u64 mask_even = word_max - 1;
static const u64 lower = word_max / 0b11;
static const u64 upper = 0b10 * lower;

#define popcnt __builtin_popcountll
#define clz __builtin_clzll
#define ctz __builtin_ctzll

__attribute__((import_name("dbg")))
extern void dbg(u32 l, u64 r);

__attribute__((import_name("print64")))
extern void print64(char* lbl, u64 value);

__attribute__((import_name("dent")))
extern void dent(s32 value);


__attribute__((export_name("nonzeros"), always_inline))
word nonzeros(word crumbs) {
  return lower & (crumbs | (crumbs >> 1));
}

__attribute__((always_inline))
static word redexes_impl(u8 leadingZeroBits, word crumbs) {
  word I, K, S;
  I = 0 | crumbs >> 2;
  K = I | crumbs >> 4;
  S = K | crumbs >> 6;
  switch (leadingZeroBits >> 1) {
    case 0:
      I |= ~(word_max >> 2);
      K |= ~(word_max >> 4);
      S |= ~(word_max >> 6);
      break;
    case 1:
      K |= ~(word_max >> 2);
      S |= ~(word_max >> 4);
      break;
    case 2:
      S |= ~(word_max >> 2);
      break;
    default:
      break;
  }
  S |= crumbs ^ (0b11 * lower);
  K |= crumbs ^ (0b10 * lower);
  I |= crumbs ^ (0b01 * lower);
  word ruled_out = (I | I >> 1) & (K | K >> 1) & (S | S >> 1);
  return crumbs & ~(0b11 * (lower & ruled_out));
}

__attribute__((export_name("redexes")))
word redexes(u8 leadingZeroBits, word crumbs) {
  // Highly encourage it to specialize the four cases
  switch (leadingZeroBits >> 1) {
    case 0:
      return redexes_impl(leadingZeroBits, crumbs);
    case 1:
      return redexes_impl(leadingZeroBits, crumbs);
    case 2:
      return redexes_impl(leadingZeroBits, crumbs);
    default:
      return redexes_impl(leadingZeroBits, crumbs);
  }
}

__attribute__((export_name("redex")))
u8u8 redex(word redexes) {
  word pos = mask_even & clz(redexes);
  return (u8u8) {
    .x = pos,
    .y = 0b11 & (redexes >> ((word_size - 2) - pos)),
  };
}

__attribute__((export_name("deltaExpecting"), always_inline))
s8 deltaExpecting(word crumbs) {
  return 2 * popcnt(nonzeros(crumbs)) - word_crumbs;
}

__attribute__((export_name("reachesZero")))
u8 reachesZero(word expecting, word crumbs) {
  word _nonzeros = nonzeros(crumbs);

  if (expecting + (clz(_nonzeros) >> 1) > popcnt(_nonzeros))
    return 0;

  u8 bit = word_size - 2;
  #pragma nounroll
  while (expecting += (0b1 & (_nonzeros >> bit) ? -1 : 1)) {
    if (!bit) return 0;
    bit -= 2;
  }
  return word_size - bit;
}

word toAtomic(word crumbs) {
  crumbs >>= ctz(crumbs);
  s8 exp = deltaExpecting(crumbs);
  return crumbs << (exp - (-1));
}

word mkcrumbs64(
  u8 q0, u8 q1, u8 q2, u8 q3, u8 q4, u8 q5, u8 q6, u8 q7, u8 q8, u8 q9, u8 qA, u8 qB, u8 qC, u8 qD, u8 qE, u8 qF,
  u8 p0, u8 p1, u8 p2, u8 p3, u8 p4, u8 p5, u8 p6, u8 p7, u8 p8, u8 p9, u8 pA, u8 pB, u8 pC, u8 pD, u8 pE, u8 pF
) {
  return (0
    | (((word) 3 & q0) << 62)
    | (((word) 3 & q1) << 60)
    | (((word) 3 & q2) << 58)
    | (((word) 3 & q3) << 56)
    | (((word) 3 & q4) << 54)
    | (((word) 3 & q5) << 52)
    | (((word) 3 & q6) << 50)
    | (((word) 3 & q7) << 48)
    | (((word) 3 & q8) << 46)
    | (((word) 3 & q9) << 44)
    | (((word) 3 & qA) << 42)
    | (((word) 3 & qB) << 40)
    | (((word) 3 & qC) << 38)
    | (((word) 3 & qD) << 36)
    | (((word) 3 & qE) << 34)
    | (((word) 3 & qF) << 32)
    | (((word) 3 & p0) << 30)
    | (((word) 3 & p1) << 28)
    | (((word) 3 & p2) << 26)
    | (((word) 3 & p3) << 24)
    | (((word) 3 & p4) << 22)
    | (((word) 3 & p5) << 20)
    | (((word) 3 & p6) << 18)
    | (((word) 3 & p7) << 16)
    | (((word) 3 & p8) << 14)
    | (((word) 3 & p9) << 12)
    | (((word) 3 & pA) << 10)
    | (((word) 3 & pB) <<  8)
    | (((word) 3 & pC) <<  6)
    | (((word) 3 & pD) <<  4)
    | (((word) 3 & pE) <<  2)
    | (((word) 3 & pF) <<  0)
  );
}

word mkcrumbs32(u8 q0, u8 q1, u8 q2, u8 q3, u8 q4, u8 q5, u8 q6, u8 q7, u8 q8, u8 q9, u8 qA, u8 qB, u8 qC, u8 qD, u8 qE, u8 qF) {
  return mkcrumbs64(q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, qA, qB, qC, qD, qE, qF, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
}

#define P 0
#define I 1
#define K 2
#define S 3

// word x = toAtomic(0b00001100100011000011101010); // T = PPSPKPSPPSKKK
// word y = mkcrumbs32(P,P,S,P,K,P,S,P,P,S,K,K,K,0,0,0);


// A ptrbit stores the raw memory pointer (a multiple of word_size/byte_size)
// and the bit within the word (which is even, since it is pointing to a crumb).
static word ptrbit2crumbs(ptrbit where) {
  // print64("ptr", where.ptr);
  // print64("bit", where.bit);
  return (where.bit >> 1) | (where.ptr << 2);
}
static ptrbit crumbs2ptrbit(word crumblength) {
  word bits = crumblength << 1;
  return (ptrbit) {
    .bit = bits % word_size,
    .ptr = sizeof(word) * (bits / word_size),
  };
}
ptrbit ptrbit_incr_bits(ptrbit where, word incr_bits) {
  word new_bit = where.bit + incr_bits;
  word incr_word = new_bit / word_size;
  return (ptrbit) {
    .bit = new_bit % word_size,
    .ptr = where.ptr + sizeof(word)*incr_word,
  };
}
#define word_of(crumbstring, where) ((crumbstring)[(where.ptr)/sizeof(word)])
#define leftbits_of(crumbstring, where) (word_of(crumbstring, where) << where.bit)
#define nextword_of(crumbstring, where) ((crumbstring)[1 + (where.ptr)/sizeof(word)])
#define from_ptr(crumbstring, where) ((crumbstring) + ((where.ptr)/sizeof(word)))

__attribute__((noinline))
static void goodptrbit(ptrbit where) {
  assert((where.ptr & 0x7) == 0);
  assert((where.bit & 0x1) == 0);
  assert(where.bit < word_size);
}

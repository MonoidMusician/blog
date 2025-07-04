#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define BLOCK(stmt) { do{ stmt } while(0); }
#undef assert
#define assert(c) BLOCK(if (!(c)) {__builtin_unreachable();})

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint8_t u8;
typedef int64_t s64;
typedef int32_t s32;
typedef int8_t s8;

typedef u64 word;
typedef size_t ptr; // memory64?

// -mmultivalue -Xclang -target-abi -Xclang experimental-mv
typedef struct { u64 x; u64 y; } u64u64;
typedef struct { u32 x; u64 y; } u32u64;
typedef struct { u32 x; u32 y; } u32u32;
typedef struct { u8 x; u8 y; } u8u8;

typedef struct { ptr ptr; u8 bit; } ptrbit;

// Constants
static const u32 word_size = 8*sizeof(word);
static const u32 word_crumbs = word_size/2;

static const word word_max = ~((word)0);
static const word mask_even = word_max - 1;
static const word lower = word_max / 0b11;
static const word upper = 0b10 * lower;
static const word word_bytes = word_max / 0b11111111;

#define popcnt __builtin_popcountll
#define clz __builtin_clzll
#define ctz __builtin_ctzll


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
__attribute__((always_inline))
word word_cat(u8 hi_bits, word hi, u8 lo_bits, word lo) {
  assert(hi_bits + lo_bits <= word_size);
  return (hi & mask_hi(hi_bits)) | ((lo & mask_hi(lo_bits)) >> hi_bits);
}


// Coalesce each nonzero crumb (pair of bits) into its lower bit.
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

// Calculate if there are any `S` redexes exclusively, since `I` and `K` are
// not really a problem for duplicated work. Assumes `leadingZeroBits = 0`.
__attribute__((export_name("nontrivial_redexes")))
word nontrivial_redexes(word crumbs) {
  word S;
  // Any nonzero crumb means that the
  // next 3 crumbs cannot be `S` redexes
  S = crumbs >> 2 | crumbs >> 4 | crumbs >> 6;
  // Top 3 crumbs cannot be redexes
  S |= ~(word_max >> 6);
  // Any any crumb that is not `0b11 = 0q3`
  // cannot be an `S` redex
  S |= crumbs ^ (0b11 * lower);
  // The redexes are then the crumbs where
  // this `S` is zero, so we coalesce them
  // and we give them the value `0b11 = 0q3`
  return ~(0b11 * nonzeros(S));
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

s8 deltaExpecting_part(word crumbs, u8 bits) {
  return 2 * popcnt(mask_hi(bits) & nonzeros(crumbs)) - bits/2;
}

// u8 reachesZero_aux(word expecting, word _nonzeros, u8 start, u8 end) {}

// pure attribute makes it worse??
__attribute__((export_name("reachesZero"), always_inline))
u8 reachesZero(word expecting, word crumbs) {
  // if (expecting == 1 && (crumbs >> (word_size - 2)) != 0)
  //   return 2;

  word _nonzeros = nonzeros(crumbs);

  // if (word_size == 64) {
  //   // Never hit in practice, apparently
  //   if (expecting + clz(_nonzeros)/2 > popcnt(_nonzeros))
  //     return 0;
  //   assert(expecting <= word_crumbs);

  //   word replicated = ((u8) expecting) * word_bytes;

  //   word acc2bits = _nonzeros; // u2x32
  //   word acc4bits = ((acc2bits >> 2) + acc2bits) & (word_max / 0b1111); // u4x16
  //   word acc8bits = ((acc4bits >> 4) + acc4bits) & (word_max / 0b11111111); // u8x8
  //   // ^ number of bits set, per byte
  //   // delta expecting, per byte:
  //   word deltas = (acc8bits);
  // }

  // u8 startat = 2 * (expecting + clz(_nonzeros)/2);
  // u8 bit = word_size - startat;
  // expecting -= deltaExpecting_part(crumbs, startat - 2);
  u8 bit = word_size - 2;
  while (expecting += (0b11 & (crumbs >> bit) ? -1 : 1)) {
    if (!bit) return 0;
    bit -= 2;
  }
  return word_size - bit;
}

__attribute__((always_inline))
u8 reachesZero1(word crumbs) {
  if ((crumbs >> (word_size - 2)) != 0)
    return 2;

  u8 expecting = 2;
  u8 bit = word_size - 4;
  while (expecting += (0b11 & (crumbs >> bit) ? -1 : 1)) {
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


// A ptrbit stores the raw memory pointer (a multiple of word_size/byte_size)
// and the bit within the word (which is even, since it is pointing to a crumb).
static word ptrbit2crumbs(ptrbit where) {
  // print64("ptr", where.ptr);
  // print64("bit", where.bit);
  return (where.bit >> 1) | (where.ptr * (word_crumbs / sizeof(word)));
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

static void goodptrbit(ptrbit where) {
  assert((where.ptr & 0x7) == 0);
  assert((where.bit & 0x1) == 0);
  assert(where.bit < word_size);
}

#include <stdbool.h>
#include <stdint.h>
#include "cuapteryx.h"

ptrbit input_at = {};
ptrbit stop_at = {};
ptrbit output_at = {};

#define SZ 1024 * 1024
word input_words[SZ];
word output_words[SZ];

#define INPUT input_words[input_at.ptr/8]
#define OUTPUT output_words[output_at.ptr/8]

__attribute__((import_name("print64")))
extern void print64(char* lbl, u64 value);
__attribute__((import_name("dent")))
extern void dent(s8);

ptr max_output = 0;

void copyBack() {
  if (output_at.ptr > max_output) max_output = output_at.ptr;
  __builtin_memcpy(input_words, output_words, output_at.ptr + (output_at.bit ? 8 : 0));
  stop_at = output_at;
}

__attribute__((noinline))
static void good() {
  goodptrbit(input_at);
  goodptrbit(stop_at);
  goodptrbit(output_at);
}

__attribute__((export_name("slowCopySubWord"), noinline))
void slowCopySubWord(u8 bits) {
  if (!bits) return;
  goodptrbit(input_at);
  goodptrbit(output_at);
  assert(bits <= word_size);
  assert(input_at.bit + bits <= word_size);

  word iwhence = ptrbit2crumbs(input_at);
  word owhence = ptrbit2crumbs(output_at);

  s8 offset = output_at.bit - input_at.bit;

  // mask off input_at (low bits)
  word tmp = INPUT & (word_max >> input_at.bit);
  if (offset > 0) {
    tmp >>= offset;
  } else {
    tmp <<= -offset;
  }

  // mask off output_at, join, and save
  OUTPUT = tmp | (OUTPUT & ~(word_max >> output_at.bit));

  // if we are across an output_at word boundary
  if (offset > 0) {
    // mask off input_at (high bits)
    output_words[1 + output_at.ptr/8] = INPUT << (word_size - offset);
  }

  // increment pointers
  if ((input_at.bit += bits) >= word_size) {
    input_at.ptr += 8;
    input_at.bit -= word_size;
  }
  if ((output_at.bit += bits) >= word_size) {
    output_at.ptr += 8;
    output_at.bit -= word_size;
  }
  assert((ptrbit2crumbs(input_at) - iwhence) == (ptrbit2crumbs(output_at) - owhence));
  good();
}
__attribute__((export_name("writeZero"), noinline))
void writeZero() {
  goodptrbit(output_at);
  // We may have written junk, so delete it
  OUTPUT &= ~(word_max >> output_at.bit);
  if ((output_at.bit += 2) == word_size) {
    output_at.ptr += 8;
    output_at.bit = 0;
  }
  goodptrbit(output_at);
}
__attribute__((export_name("slowCopy1"), noinline))
bool slowCopy1() {
  goodptrbit(input_at);
  goodptrbit(output_at);
  // adjust the expected number of nodes by the leading nonzeros *crumbs*
  // we will add (could use leading zeros, but this seems more robust to
  // optimizations in $reachesZero)
  word exp = 1 + (input_at.bit >> 1);

  dent(1);
  print64("stop_at.ptr", stop_at.ptr);
  print64("stop_at.bit", stop_at.bit);
  print64("stop_at", ptrbit2crumbs(stop_at));
  do {
    word crumbs = INPUT | ~(word_max >> input_at.bit);
    u8 _reachesZero = reachesZero(exp, crumbs);
    if (_reachesZero) {
      print64("reached zero at bit", _reachesZero);
      // Copy that many bits, then return
      slowCopySubWord(_reachesZero - input_at.bit);
      dent(-1);
      return true;
    } else {
      // Copy the rest of the word, advance
      slowCopySubWord(word_size - input_at.bit);
      assert(input_at.bit == 0);
      s8 delta = deltaExpecting(crumbs);
      print64("delta", delta | ((delta < 0)*0x10000));
      print64("exp", exp);
      print64("crumbs", crumbs);
      assert(delta <= 0 || exp > delta);
      exp -= delta;
      print64("still expecting", exp);
      // Check for overrun
      if (input_at.ptr >= stop_at.ptr && (input_at.ptr > stop_at.ptr || input_at.bit >= stop_at.bit)) {
        dent(-1);
        return false;
      }
    }
  } while(exp);
  good();
  dent(-1);

  return true;
}
__attribute__((export_name("slowScan1"), noinline))
bool slowScan1() {
  ptrbit tmp = output_at;
  bool r = slowCopy1();
  output_at = tmp;
  return r;
}

int fuel asm("fuel") = 10000;

__attribute__((export_name("slowest")))
word slowest(word crumbs_length) {
  // Carry over trailing zeros as leading zeros
  u8 leadingZeroBits;

  good();

  print64("slowest: ptr", stop_at.ptr);
  print64("slowest: bit", stop_at.bit);
  print64("fuel", fuel);

  dent(1);
  while(fuel--) {
    // Read from beginning
    input_at = (ptrbit) {};
    // Reset/reuse output_at tape
    output_at = (ptrbit) {};

    leadingZeroBits = 0; // awawawa

    word crumbs;
    word _redexes;

    print64("scan until", ptrbit2crumbs(stop_at));
    dent(1);
    // Scan each word for a redex
    // #pragma unroll(2)
    while (input_at.ptr <= stop_at.ptr) {
      print64("at position", ptrbit2crumbs(input_at));
      print64("out of", ptrbit2crumbs(stop_at));
      good();
      crumbs = INPUT | ~(word_max >> input_at.bit);
      print64("crumbs", crumbs);
      _redexes = redexes(leadingZeroBits, crumbs);
      print64("redexes", _redexes);
      if (_redexes) break; else

      if (input_at.ptr < stop_at.ptr) {
        // Copy this full word to the output_at
        slowCopySubWord(word_size);
        print64("continuing at", ptrbit2crumbs(input_at));
        // Record how many zeros there are at the end
        leadingZeroBits = ctz(crumbs);
        // Look for a redex in the next word
        continue;
      } else {
        // Copy the crumbs left
        slowCopySubWord(stop_at.bit);
        print64("stopping at", ptrbit2crumbs(input_at));
        // No redexes left
        dent(-1); goto already_normalized;
      }
    }
    dent(-1);
    good();

    // Inspect the redex: what position, what it is its value?
    u8u8 shiftarity = redex(_redexes);
    u8 shift = shiftarity.x;
    u8 arity = shiftarity.y;
    u8 arity2 = arity * 2; // number of bits of leading zeros
    print64("shift", shift);
    print64("arity", arity);

    // Edge case: we found a redex beyond the bounds. This is not "supposed"
    // to happen (if the rest of the word is zeroed out), but with all of the
    // full-word copies we use, we end up with junk there.
    if (input_at.ptr >= stop_at.ptr && shift + arity2 >= stop_at.bit) {
      print64("overran, copying", stop_at.bit - input_at.bit);
      slowCopySubWord(stop_at.bit - input_at.bit);
      goto already_normalized;
    }

    if (arity2 <= shift) {
      // Copy up to the start of the redex
      slowCopySubWord(shift - arity2);
      good();
    } else {
      // Roll back output_at.bit to overwrite the the zeros
      if (arity2 >= output_at.bit) {
        output_at.bit += word_size;
        output_at.ptr -= 8;
      }
      output_at.bit -= arity2;
    }
    // Advance past the head of the redex
    input_at.bit = shift+2;
    if (input_at.bit == word_size) {
      input_at.ptr += 8;
      input_at.bit = 0;
    }
    print64("arguments start at", ptrbit2crumbs(input_at));
    good();

    { // structured control flow for (revert:)
      // Temporaries for reverting a reduction without enough arguments
      ptrbit ri = input_at, ro = output_at;

      switch (arity) {
        case 1: // Ix = x
          if (!slowCopy1()) goto revert;
          break;
        case 2: // Kxy = x
          if (!slowCopy1()) goto revert;
          print64("Kx output at", ptrbit2crumbs(output_at));
          if (!slowScan1()) goto revert;
          print64("Kxy output at", ptrbit2crumbs(output_at));
          break;
        case 3: // Sxyz = 00xz0yz
          writeZero();
          writeZero();

          print64("00 output", OUTPUT);
          print64("00 output at", ptrbit2crumbs(output_at));
          if (!slowCopy1()) goto revert; // copy x
          print64("00x output", OUTPUT);
          print64("00x output at", ptrbit2crumbs(output_at));
          good();
          ptrbit y = input_at;
          print64("scan y from", ptrbit2crumbs(input_at));
          if (!slowScan1()) goto revert; // skip y
          print64("scanned y", ptrbit2crumbs(input_at));
          good();
          if (!slowCopy1()) goto revert; // copy z
          good();
          print64("00xz output at", ptrbit2crumbs(output_at));

          writeZero();
          input_at = y;
          if (!slowCopy1()) goto revert; // copy y, for real
          print64("00xz0y output at", ptrbit2crumbs(output_at));
          if (!slowCopy1()) goto revert; // copy z, again
          print64("00xz0yz output at", ptrbit2crumbs(output_at));
          break;
        default:
          print64("bad bad", arity);
          __builtin_unreachable();
      }

      // If it succeeded (if there were enough arguments)
      {
        print64("succeeded at", ptrbit2crumbs(input_at));
        print64("output at", ptrbit2crumbs(output_at));
        // Copy the rest
        while (input_at.ptr < stop_at.ptr) {
          slowCopySubWord(word_size - input_at.bit);
        } slowCopySubWord(stop_at.bit - input_at.bit);
        // Break from searching for a redex; continue looping for normal form
        goto not_yet_normalized;
      }

      // If it did not succeed (too few arguments)
      revert: {
        print64("reverting", ptrbit2crumbs(input_at));
        input_at = ri; output_at = ro;
        // Write the zeros back out
        while (arity) {
          writeZero();
          arity -= 1;
        }
        // Write the head back out
        input_at.bit = ri.bit - 2;
        good();
        slowCopySubWord(2);
        // This is where we will start looking for reductions from again
        input_at = ri;
        good();
      }
    } // end structured control flow for (revert:)

    not_yet_normalized:
    print64("first loopback", output_words[0]);
    print64("loopback length", ptrbit2crumbs(output_at));
    copyBack();
    continue;
    already_normalized: break;
  }
  dent(-1);

  print64("first output", output_words[0]);
  print64("output still at", ptrbit2crumbs(output_at));
  good();
  return ptrbit2crumbs(output_at);
}

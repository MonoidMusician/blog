#![feature(mapped_lock_guards)]
#![allow(nonstandard_style)]
#![feature(const_cmp)]
#![feature(const_trait_impl)]
#![feature(const_convert)]
#![feature(import_trait_associated_functions)]
use std::{ops::{Deref, DerefMut}, sync::{Arc, MappedRwLockReadGuard, RwLock, RwLockReadGuard, Weak}};
use Default::default;

macro_rules! const_assert {
    ($x:expr $(,)?) => {
        const _: () = assert!($x);
    };
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum SKI { I = 1, K = 2, S = 3 } use SKI::*;
impl const From<word> for SKI {
  fn from(value: word) -> Self {
    match value {
      1 => I,
      2 => K,
      3 => S,
      _ => panic!("Bad combinator") //panic!("Bad combinator {}", value)
    }
  }
}
impl const From<u32> for SKI {
  fn from(value: u32) -> Self {
    match value {
      1 => I,
      2 => K,
      3 => S,
      _ => panic!("Bad combinator") //panic!("Bad combinator {}", value)
    }
  }
}
impl const From<u8> for SKI {
  fn from(value: u8) -> Self {
    SKI::from(value as u32)
  }
}
impl const From<SKI> for Operand {
  fn from(value: SKI) -> Self {
    Immediate((value as u64) << (word_size - 2))
  }
}

type word = u64;

const word_size: u8 = 8*size_of::<word>() as u8;
const word_crumbs: u8 = word_size/2;
const word_max: word = !0;
const mask_even: word = word_max - 1;
const lower: word = word_max / 0b11;
const upper: word = 0b10 * lower;
const word_bytes: word = word_max / 0b11111111;

const fn mask_hi(hi_bits: u8) -> word {
  return !(word_max >> hi_bits);
}
const fn mask_lo(lo_bits: u8) -> word {
  return !(word_max << lo_bits);
}
#[inline]
const fn word_cat(hi_bits: u8, hi: word, lo_bits: u8, lo: word) -> word {
  debug_assert!(hi_bits + lo_bits <= word_size);
  return (hi & mask_hi(hi_bits)) | ((lo & mask_hi(lo_bits)) >> hi_bits);
}
#[inline]
// Coalesce each nonzero crumb (pair of bits) into its lower bit.
const fn nonzeros(crumbs: word) -> word {
  return lower & (crumbs | (crumbs >> 1));
}

const_assert!(nonzeros(0b11) == 0b01);
const_assert!(nonzeros(0b101) == 0b101);
const_assert!(nonzeros(0b1101) == 0b101);


#[inline]
const fn redexes_impl(leadingZeroBits: u8, crumbs: word) -> word {
  let mut cI = 0 | crumbs >> 2;
  let mut cK = cI | crumbs >> 4;
  let mut cS = cK | crumbs >> 6;

  match leadingZeroBits / 2 {
    0 => {
      cI |= !(word_max >> 2);
      cK |= !(word_max >> 4);
      cS |= !(word_max >> 6);
    }
    1 => {
      cK |= !(word_max >> 2);
      cS |= !(word_max >> 4);
    }
    2 => {
      cS |= !(word_max >> 2);
    }
    _ => {}
  }
  cS |= crumbs ^ (0b11 * lower);
  cK |= crumbs ^ (0b10 * lower);
  cI |= crumbs ^ (0b01 * lower);
  let ruled_out = (cI | cI >> 1) & (cK | cK >> 1) & (cS | cS >> 1);
  return crumbs & !(0b11 * (lower & ruled_out));
}

const fn redexes(leadingZeroBits: u8, crumbs: word) -> word {
  // Highly encourage it to specialize the four cases
  match leadingZeroBits >> 1 {
    0 => redexes_impl(leadingZeroBits, crumbs),
    1 => redexes_impl(leadingZeroBits, crumbs),
    2 => redexes_impl(leadingZeroBits, crumbs),
    _ => redexes_impl(leadingZeroBits, crumbs),
  }
}

// Calculate if there are any `S` redexes exclusively, since `I` and `K` are
// not really a problem for duplicated work. Assumes `leadingZeroBits = 0`.
const fn nontrivial_redexes(crumbs: word) -> word {
  let mut cS;
  // Any nonzero crumb means that the
  // next 3 crumbs cannot be `S` redexes
  cS = crumbs >> 2 | crumbs >> 4 | crumbs >> 6;
  // Top 3 crumbs cannot be redexes
  cS |= !(word_max >> 6);
  // Any any crumb that is not `0b11 = 0q3`
  // cannot be an `S` redex
  cS |= crumbs ^ (0b11 * lower);
  // The redexes are then the crumbs where
  // this `S` is zero, so we coalesce them
  // and we give them the value `0b11 = 0q3`
  return !(0b11 * nonzeros(cS));
}

const fn redex(redexes: word) -> (u8, u8) {
  let pos = redexes.leading_zeros() as u8 & !1;
  return (pos, (0b11 & (redexes >> ((word_size - 2) - pos))) as u8);
}

const fn deltaExpecting(crumbs: word) -> i8 {
  return 2 * nonzeros(crumbs).count_ones() as i8 - word_crumbs as i8;
}
const fn deltaExpecting_part(crumbs: word, bits: u8) -> i8 {
  return 2 * (mask_hi(bits) & nonzeros(crumbs)).count_ones() as i8 - (bits as i8)/2;
}

const fn reachesZero(mut expecting: word, crumbs: word) -> u8 {
  let mut bit = word_size - 2;
  loop {
    if 0 != 0b11 & (crumbs >> bit) {
      expecting -= 1;
    } else {
      expecting += 1;
    }
    if expecting == 0 { break }
    if bit == 0 { return 0 }
    bit -= 2;
  }
  return word_size - bit;
}

const fn reachesZero1(crumbs: word) -> u8 {
  if (crumbs >> (word_size - 2)) != 0
  { return 2 }

  let mut expecting = 2;
  let mut bit = word_size - 4;
  loop {
    if 0 != 0b11 & (crumbs >> bit) {
      expecting -= 1;
    } else {
      expecting += 1;
    }
    if expecting == 0 { break }
    if bit == 0 { return 0 }
    bit -= 2;
  }
  return word_size - bit;
}

const fn toAtomic(mut crumbs: word) -> word {
  crumbs >>= crumbs.trailing_zeros();
  let exp = deltaExpecting(crumbs);
  return crumbs << (exp - (-1));
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct idxbit(usize, u8);

const fn idxbitlength(crumbstring: &[word], _known_words: word) -> idxbit {
  let mut i: usize = 0;
  let mut exp: word = 1;
  let mut offset: u8 = 0;
  // Quickly scan through `known_words - 1`, using just
  // `deltaExpecting` and not `reachesZero`
  // if (known_words) while (i < known_words - 1) {
  //   exp -= deltaExpecting(crumbstring[i++]);
  //   assert(exp != 0);
  // }
  // Fall through, in case `known_words` was too short.
  // (This is used in `ap_heap_cat`.)
  while offset == 0 {
    // See if it reaches zero during the current word
    offset = reachesZero(exp, crumbstring[i]);
    if offset == 0 {
      // If not, we need to continue with the next word
      // and adjust the expected number of operands
      exp -= deltaExpecting(crumbstring[i]) as u64;
      i += 1;
      assert!(exp != 0);
      continue;
    } else { break }
  }
  // It needs to be even since it is a bit index of a crumb
  assert!((offset & 1) == 0);
  // `i` denotes the index of the word that reached zero
  return idxbit(i, offset);
}

impl std::ops::Add<usize> for idxbit {
  type Output = idxbit;
  fn add(self, incr_bits: usize) -> idxbit {
    let idxbit(idx, bit) = self;
    let new_bit = bit as usize + incr_bits;
    return idxbit(
      idx + new_bit/word_size as usize,
      (new_bit % word_size as usize) as u8
    );
  }
}
impl std::ops::Add<idxbit> for idxbit {
  type Output = idxbit;
  fn add(self, other: idxbit) -> idxbit {
    return idxbit(self.0 + other.0, self.1) + other.1 as usize;
  }
}
impl std::ops::AddAssign<usize> for idxbit {
  fn add_assign(&mut self, incr_bits: usize) {
    *self = *self + incr_bits
  }
}
impl std::ops::AddAssign<idxbit> for idxbit {
  fn add_assign(&mut self, other: idxbit) {
    *self = *self + other
  }
}
impl std::ops::Sub<idxbit> for idxbit {
  type Output = usize;
  fn sub(self, rhs: idxbit) -> Self::Output {
    return (rhs.0 - self.0)*size_of::<word>() + (rhs.1 - self.1) as usize;
  }
}
impl Default for idxbit {
  fn default() -> Self {
    idxbit(0,0)
  }
}

fn copy_to_unaligned(dst: &mut [word], src: &[word], offset: u8, clobber: bool) {
  if src.is_empty() { return }
  assert!(dst.len() + 1 >= src.len());
  debug_assert!(offset < word_size);
  if offset == 0 { return dst.copy_from_slice(src); }
  let mut hanging = if clobber { 0 } else { dst[0] };
  for (i, &word) in src.iter().enumerate() {
    dst[i] = word_cat(offset, hanging, word_size - offset, word);
    hanging = word << (word_size - offset);
  }
  if hanging != 0 { dst[src.len()] = hanging; }
}
fn copy_from_unaligned(dst: &mut [word], src: &[word], offset: u8) {
  if src.is_empty() { return }
  assert!(dst.len() >= src.len());
  debug_assert!(offset < word_size);
  if offset == 0 { return dst.copy_from_slice(src); }
  let mut iter = src.iter().enumerate();
  let mut last =
    if let Some((_, &w)) = iter.next() { w } else { return };
  for (i, &word) in iter {
    let this = last << offset;
    last = word;
    dst[i - 1] = this | (last >> (word_size - offset));
  }
  dst[src.len() - 1] = last;
}

#[derive(Clone, Default)]
struct BitString {
  words: Vec<word>,
  len: idxbit,
}

impl BitString {
  fn new(words: Vec<word>, ib: idxbit) -> BitString {
    BitString { words, len: ib }
  }
  fn _full_words(&self) -> &[word] {
    return &self.words[..self.len.0];
  }
  fn _last_word(&self) -> word {
    debug_assert!(self.words.len() >= self.len.0);
    if self.words.len() == self.len.0 { return 0 }
    return self.words[self.len.0] & mask_hi(self.len.1);
  }
}

impl PartialEq for BitString {
  fn eq(&self, other: &Self) -> bool {
    return
      self.len == other.len &&
      self._last_word() == other._last_word() &&
      self._full_words() == other._full_words();
  }
}

#[derive(Clone, Default)]
#[repr(transparent)]
struct CrumbString<const is_operand: bool>(BitString);
impl From<Vec<word>> for CrumbString<true> {
  fn from(value: Vec<word>) -> Self {
    let len = idxbitlength(&value[..], 0);
    assert!(len != idxbit(0,0));
    CrumbString(BitString::new(value, len))
  }
}
impl From<word> for CrumbString<true> {
  fn from(crumbs: word) -> Self {
    let len = reachesZero1(crumbs);
    assert!(len != 0);
    CrumbString(BitString::new(vec![crumbs], idxbit(0,len)))
  }
}

impl<const is_operand: bool> CrumbString<is_operand> {
}

#[derive(Clone)]
enum Operand {
  Shared(Arc<RwLock<SharedOperand>>),
  Immediate(word),
}
use Operand::*;

impl Operand {
  fn new(shared: SharedOperand) -> Operand {
    Shared(Arc::new(RwLock::new(shared)))
  }
}
impl Default for Operand {
  fn default() -> Self {
    Immediate(0)
  }
}

#[derive(Clone)]
enum SharedOperand {
  Synth { fun: Operand, arg: Operand, work: u64 },
  Heap  { crumbstring: Vec<word>,     work: u64 },
}
use SharedOperand::*;

const nullop: Operand = Immediate(0);
const fn isnullop(op: &Operand) -> bool {
  match op {
    &Immediate(word) => word == 0,
    Shared(_) => false,
  }
}
const fn isimmediate(op: &Operand) -> bool {
  match op {
    Immediate(word) => false,
    Shared(_) => true,
  }
}
const fn direct(crumbs: word) -> Operand {
  debug_assert!(crumbs != 0);
  debug_assert!(reachesZero1(crumbs) != 0);
  debug_assert!(reachesZero1(crumbs) < word_size);
  // Clear out the irrelevant crumbs (optional, slower)
  // (Seems to impact WASM more than x86_64, for whatever reason)
  return Immediate(crumbs & mask_hi(reachesZero1(crumbs)));
}

fn match_crumbs(match_low: word, crumblen: u8, op: &Operand) -> bool {
  let crumbs = match op {
    &Immediate(crumbs) => crumbs,
    Shared(ptr) => match ptr.read().unwrap().deref() {
      Heap { crumbstring, .. } => crumbstring[0],
      Synth { .. } => return false,
    },
  };
  let len = crumblen * 2;
  let mask = mask_hi(len);
  return (mask & crumbs) == (match_low << (word_size - len));
}

fn isI(op: &Operand) -> bool {match_crumbs(0x1, 1, op)}
fn isK(op: &Operand) -> bool {match_crumbs(0x2, 1, op)}
fn isS(op: &Operand) -> bool {match_crumbs(0x3, 1, op)}

#[derive(Default)]
struct StackFrame {
  to_eval: Operand,
  was_evaling: Weak<RwLock<SharedOperand>>, // weak ref! must be zeroed out when freed
}
impl const From<Operand> for StackFrame {
  fn from(to_eval: Operand) -> Self {
    StackFrame { to_eval, was_evaling: Weak::new() }
  }
}
impl const From<SKI> for StackFrame {
  fn from(to_eval: SKI) -> Self {
    Self::from(Operand::from(to_eval))
  }
}

struct System {
  input_at: idxbit,
  stop_at: idxbit,
  output_at: idxbit,
  input_words: Vec<word>,
  output_words: Vec<word>,
  work: u64,
  fuel: u64,
  stack_endstop: usize,
  // stack_top: usize,
  stack: Vec<StackFrame>,
}

fn goodop(op: &Operand) {
  debug_assert!(!isnullop(op));
  match op {
    &Immediate(crumbs) => debug_assert_ne!(reachesZero1(crumbs), 0),
    Shared(ptr) => match ptr.read().unwrap().deref() {
      Heap { crumbstring, .. } => debug_assert_ne!(crumbstring.len(), 0),
      Synth { .. } => {},
    },
  }
}

impl const From<Vec<word>> for SharedOperand {
  fn from(crumbstring: Vec<u64>) -> SharedOperand {
    Heap { crumbstring, work: 0 }
  }
}

impl SharedOperand {
  const fn get_work(&self) -> u64 {
    match self {
      Heap { work, .. } => *work,
      Synth { work, .. } => *work,
    }
  }
  // Morally this should take it by value, but it needs to be kept live/cloned
  // in the loop anyways when it is not the last operand to save
  fn have_evaled(&mut self, evaled: &Operand) {
    goodop(&evaled);
    goodop(&Operand::new(self.clone()));
    match evaled {
      &Immediate(crumbs) => {
        *self = Heap { crumbstring: vec![crumbs], work: self.get_work() };
      },
      Shared(ptr) => *self = ptr.read().unwrap().clone(),
    }
  }
}

impl Operand {
  fn dup(mut self) -> (Operand, Operand) {
    match self {
      Shared(ptr) => (Shared(ptr.clone()), Shared(ptr)),
      Immediate(crumbs) => {
        if nontrivial_redexes(crumbs) == 0 { return (Immediate(crumbs), Immediate(crumbs)) }
        self = Operand::new(Heap { crumbstring: vec![crumbs], work: 0 });
        (self.clone(), self)
      }
    }
  }
  fn fast_unconstant(&self) -> Option<Operand> {
    match self {
      Immediate(crumbs) => {
        if match_crumbs(0x2, 2, self) {
          return Some(Immediate((crumbs & !0b11) << 4));
        }
      },
      Shared(ptr) => match ptr.read().unwrap().deref() {
        Synth { fun, arg, .. } =>
          if isK(fun) { return Some(arg.clone()) },
        Heap { crumbstring, .. } =>
          if match_crumbs(0x2, 2, self) {
            if crumbstring.len() == 1 {
              return Some(Immediate((crumbstring[0] & !0b11) << 4));
            }
            if Arc::strong_count(ptr) == 1 {
              let mut guard = ptr.write().unwrap();
              let Heap { crumbstring, .. } = guard.deref_mut() else { return None };
              let mut last_bits = 0;
              for i in (0..crumbstring.len()).rev() {
                let bits_were = crumbstring[i] >> (word_size - 4);
                crumbstring[i] = (crumbstring[i] << 4) | last_bits;
                last_bits = bits_were;
              }
              assert!(last_bits == 0x2);
              return Some(self.clone());
            }
            // TODO: refcount 1?
          }
      }
    } None
  }
}

fn ap_heap_cat(fun: &[word], arg: &[word]) -> Operand {
  let mut dst = Vec::with_capacity(fun.len() + arg.len());
  copy_to_unaligned(&mut dst[..], fun, 2, true);
  let idxbit(idx, bit) = idxbitlength(fun, 0) + 2;
  copy_to_unaligned(&mut dst[idx..], arg, bit, false);
  return Operand::new(Heap { crumbstring: dst, work: 0 });
}
fn synthetic_apply(fun: Operand, arg: Operand) -> Operand {
  return Operand::new(Synth { fun, arg, work: 0 });
}

fn apply(fun: Operand, arg: Operand) -> Operand {
  goodop(&fun); goodop(&arg);
  if isI(&fun) { return arg }

  match (&fun, &arg) {
    (&Immediate(fun), &Immediate(arg)) => {
      if nontrivial_redexes(fun & !0b11) == 0
      && nontrivial_redexes(arg & !0b11) == 0
      {
        let fun_len = reachesZero1(fun);
        let arg_len = reachesZero1(arg);
        if 2 + fun_len + arg_len <= word_size {
          return Immediate(word_cat(fun_len, fun, arg_len, arg) >> 2);
        } // else fallthrough
      }
    },
    _ => {}
  }

  fn small_op_buf<'a>(op: &'a Operand) -> Option<Result<
    MappedRwLockReadGuard<'a, Vec<word>>,
    u64
  >> {
    match op {
      Immediate(crumbs) => {
        return Some(Err(*crumbs));
      },
      Shared(ptr) =>
        RwLockReadGuard::filter_map(ptr.read().unwrap(), |v| {
          match v {
            Heap { crumbstring, .. }
              if crumbstring.len() < 16
              && Arc::strong_count(ptr) == 1
              => Some(crumbstring),
            _ => None
          }
        }).ok().map(Ok)
    }
  }
  if let Some(fbuf) = small_op_buf(&fun)
  && let Some(abuf) = small_op_buf(&arg)
  {
    let mut tmp1 = vec![0];
    let mut tmp2 = vec![0];
    let fref = match &fbuf {
      Err(w) => { tmp1[0] = *w; &tmp1 },
      Ok(g) => g.deref()
    };
    let aref = match &abuf {
      Err(w) => { tmp2[0] = *w; &tmp2 },
      Ok(g) => g.deref()
    };
    return ap_heap_cat(fref, aref);
  }
  return synthetic_apply(fun, arg);
}

fn could_be_reducible(op: Operand) -> bool {
  match &op {
    Immediate(crumbs) =>
      return redexes(0, *crumbs) != 0,
    Shared(ptr) => match ptr.read().unwrap().deref() {
      Heap { crumbstring, .. } => {
        let mut exp: i64 = 1;
        let mut trailing = 0;
        for &crumbs in crumbstring {
          if redexes(trailing, crumbs) != 0
          { return true }
          if reachesZero(exp as u64, crumbs) != 0
          { return false }
          exp -= deltaExpecting(crumbs) as i64;
          if exp == 0 { return false; }
          trailing = crumbs.trailing_zeros() as u8;
        }
      },
      _ => {},
    }
  }
  return false;
}

fn _scan1(crumbstring: &[word], mut input_at: idxbit, stop_at: idxbit) -> idxbit {
  let failed = input_at;
  // adjust the expected number of nodes by the leading nonzeros *crumbs*
  // we will add (could use leading zeros, but this seems more robust to
  // optimizations in `reachesZero()`)
  let exp = 1 + (input_at.1 >> 1) as i64;
  while input_at <= stop_at {
    let crumbs = crumbstring[input_at.0] | mask_hi(input_at.1);
    input_at.1 = reachesZero(exp as u64, crumbs);
    if input_at.1 != 0 {
      input_at += 0;
      return if input_at < stop_at { input_at } else { failed }
    }
    input_at.0 += 1;
    input_at.1 = 0;
  }
  return failed;
}

fn scan1op(crumbstring: &[word], input_at: idxbit, stop_at: idxbit) -> Option<(idxbit, Operand)> {
  let next = _scan1(crumbstring, input_at, stop_at);
  let crumblen = (next - input_at)/2;
  if crumblen == 0 { return None }
  Some((next, if crumblen < word_crumbs as usize {
    if next.0 == input_at.0 || next.1 == 0 {
      assert!(next.0 <= input_at.0 + 1);
      let crumbs = crumbstring[input_at.0] << input_at.1;
      assert!(reachesZero1(crumbs) == 2*crumblen as u8);
      Immediate(crumbs & mask_hi(2*crumblen as u8))
    } else {
      let crumbs = word_cat(
        word_size - input_at.1,
          crumbstring[input_at.0] << input_at.1,
        (2*crumblen as u8) - (word_size - input_at.1),
          crumbstring[input_at.0 + 1]
      );
      assert!(reachesZero1(crumbs) == 2*crumblen as u8);
      Immediate(crumbs & mask_hi(2*crumblen as u8))
    }
  } else {
    let mut dst = Vec::with_capacity(2 + crumblen/word_crumbs as usize);
    copy_from_unaligned(&mut dst[..], &crumbstring[input_at.0..], input_at.1);
    Operand::new(Heap { crumbstring: dst, work: 0 })
  }))
}

impl System {
  fn at(&self) -> usize { return self.stack.len() }
  fn scan1input(&mut self) -> Option<Operand> {
    let (input_at, op) = scan1op(&self.input_words[..], self.input_at, self.stop_at)?;
    self.input_at = input_at;
    return Some(op);
  }
  fn refill(&mut self) -> Option<StackFrame> {
    if self.at() > self.stack_endstop {
      return self.stack.pop();
    }
    debug_assert!(self.at() == self.stack_endstop);
    if self.input_at == self.stop_at { return None }
    debug_assert!(self.stack_endstop == 0);
    match self.scan1input() {
      None => {
        // self.input_at += 2;
        panic!("Not well formed operand");
        return None;
      },
      Some(op) => {
        goodop(&op);
        return Some(StackFrame { to_eval: op, was_evaling: Weak::new() });
      }
    }
  }
  fn skip_leading_zeros(&mut self) {
    self.input_at = idxbit(0,0);
    // Whole words
    while self.input_at.0 < self.stop_at.0 && self.input_words[self.input_at.0] == 0 {
      self.input_at.0 += 1;
    }
    // Remaining crumbs
    self.input_at.1 = (self.input_words[self.input_at.0].leading_zeros() as u8) & !1;
  }
  fn unpack_synth_onto_stack(&mut self, mut op: Operand) -> Operand {
    goodop(&op);
    loop {
      op = if
        let Shared(ptr) = &op &&
        let Synth { fun, arg, work } = ptr.read().unwrap().deref()
      {
        if self.at() > self.stack_endstop && Arc::strong_count(ptr) == 1 {
          let at = self.at();
          self.stack[at - 1].was_evaling = Arc::downgrade(ptr);
        }
        self.stack.push(StackFrame {
          to_eval: arg.clone(),
          was_evaling: Weak::new(),
        });
        // assert!(self.stack_top < self.stack.len());
        fun.clone()
      } else { break }
    }
    return op;
  }
  fn unpack_crumbstring_onto_stack(&mut self, op: Operand) -> SKI {
    let mut tmp = vec![0];
    let obtain = match &op {
      &Immediate(crumbs) => {
        if crumbs >> (word_size - 2) != 0 { panic!("Not a combinator") }
        Err(crumbs)
      },
      Shared(ptr) =>
        Ok(RwLockReadGuard::map(ptr.read().unwrap(),|v| match v {
          Heap { crumbstring, .. } => crumbstring,
          Synth { .. } => panic!("Should not by synth"),
        })),
    };
    let crumbstring = match &obtain {
      Err(w) => { tmp[0] = *w; &tmp },
      Ok(g) => g.deref()
    };
    let len = crumbstring.len();
    let mut at = idxbit(0,0);
    while at.0 < len && crumbstring[at.0] == 0
      { at.0 += 1 }
    at.1 = crumbstring[at.0].leading_zeros() as u8 & !1;

    let arity = (at - idxbit(0,0))/2;

    // assert!(arity < self.stack.len());
    // assert!(self.stack_top < self.stack.len() - arity - 1);
    assert!(arity != 0);
    // self.stack_top += arity + 1;
    self.stack.extend((0..=arity+1).map(|_| default()));
    for stacked in 1..=arity+1 {
      let scanned = scan1op(crumbstring, at, idxbit(crumbstring.len(),0)).unwrap();
      at = scanned.0;
      goodop(&scanned.1);
      let at = self.at();
      self.stack[at - stacked] = scanned.1.into();
    }
    match self.stack.pop() {
      Some(StackFrame { to_eval: Immediate(crumb), .. }) =>
        SKI::from(crumb >> (word_size - 2)),
      _ =>   { panic!("") }
    }
  }
  fn save_whnf_on_stack(&mut self, crumb: SKI) {
    let mut was_evaled: usize = 0;
    for i in 1..=(self.at() - self.stack_endstop) {
      if i >= crumb as usize { break }
      if self.stack[self.at() - i].was_evaling.strong_count() > 0 {
        was_evaled = i;
      }
    }
    let mut evaled = Immediate(crumb as u64);
    for i in 1..=was_evaled {
      if let Some(ptr) = self.stack[self.at() - i].was_evaling.upgrade() {
        let mut here = ptr.write().unwrap();
        here.have_evaled(&evaled);
        let at = self.at();
        self.stack[at - i].was_evaling = Weak::new();
      }
      if i < was_evaled {
        evaled = apply(evaled, self.stack[self.at() - i].to_eval.clone());
      } else { break }
    }
  }
  fn reduction_rule(&mut self, crumb: SKI) -> bool {
    match crumb {
      I => {
        // Check that there is an argument available; otherwise, we need to restore
        // the head operand back to the stack
        if self.at() == self.stack_endstop {
          let Some(frame) = self.refill() else {
            self.stack.push(crumb.into());
            return false;
          };
          self.stack.push(frame);
        }

        self.work += 1;
        return true;
      },
      K => {
        // Get two arguments
        let Some(x) = self.refill().map(|x| x.to_eval) else {
          self.stack.push(crumb.into());
          return false;
        };
        // TODO: tell `refill()` that it does not need to actually copy this operand?
        let Some(_) = self.refill() else {
          self.stack.push(x.into());
          self.stack.push(crumb.into());
          return false;
        };
        // Drop `y` and put `x` back onto the stack: `Kxy = x`
        self.stack.push(x.into());
        self.work += 1;
        return true;
      },
      S => {
        // Get three arguments
        let Some(x) = self.refill().map(|x| x.to_eval) else {
          self.stack.push(crumb.into());
          return false;
        };
        let Some(y) = self.refill().map(|y| y.to_eval) else {
          self.stack.push(x.into());
          self.stack.push(crumb.into());
          return false;
        };
        let Some(z) = self.refill().map(|z| z.to_eval) else {
          self.stack.push(y.into());
          self.stack.push(x.into());
          self.stack.push(crumb.into());
          return false;
        };
        // Promote a reducible immediate to a shared operand
        let (z, z2) = z.dup();
        let yz = apply(y, z2);
        self.stack.push(yz.into());
        self.stack.push(z.into());
        self.stack.push(x.into());
        self.work += 1;
        return true;
      }
    }
  }
  fn step(&mut self) -> bool {
    debug_assert!(self.at() >= self.stack_endstop);
    let Some(StackFrame { to_eval: op, was_evaling }) = self.refill() else {
      return false
    };
    debug_assert!(was_evaling.strong_count() == 0);
    drop(was_evaling);
    goodop(&op);

    // Do some setup for shared nodes
    let (was_synth, mut op) = match &op {
      Immediate(_) => (false, op),
      Shared(ptr) => {
        if let Heap { crumbstring, .. } = ptr.read().unwrap().deref()
        && crumbstring[0] >> (word_size - 2) != 0
        {
          (false, Immediate(crumbstring[0]))
        } else
        if self.at() == self.stack_endstop
        && Arc::strong_count(ptr) > 1
        && let Some(frame) = self.refill()
        {
          self.stack.push(StackFrame {
            to_eval: frame.to_eval,
            was_evaling: Arc::downgrade(ptr),
          });
          (true, op)
        }
        else { (true, op) }
      }
    };
    op = self.unpack_synth_onto_stack(op);
    if was_synth
    && let Shared(ptr) = &op
    && self.at() == self.stack_endstop
    && Arc::strong_count(ptr) > 1
    && let Some(frame) = self.refill()
    {
      self.stack.push(StackFrame {
        to_eval: frame.to_eval,
        was_evaling: Arc::downgrade(ptr),
      });
    }
    let op = self.unpack_crumbstring_onto_stack(op);
    self.save_whnf_on_stack(op);

    return self.reduction_rule(op);
  }
  fn read_whnf_from_stack(&mut self) -> Operand {
    assert!(self.at() > self.stack_endstop);
    let Some(StackFrame { to_eval: mut op, .. }) = self.stack.pop() else {
      panic!("No operand on stack");
    };
    while let Some(StackFrame { to_eval: arg, .. }) = self.stack.pop() {
      goodop(&op);
      op = synthetic_apply(op, arg);
    }
    goodop(&op);
    return op;
  }
  fn whnf_op(&mut self, op: Operand) -> Operand {
    if self.fuel == 0 { return op }
    assert!(self.at() == self.stack_endstop);
    self.stack.push(op.into());
    while self.fuel != 0 && self.step() {
      self.fuel -= 1;
    }
    return self.read_whnf_from_stack();
  }
  fn whnf2nf_op(&mut self, mut op: Operand) {
    goodop(&op);
    loop {
      op = match op {
        _ if self.fuel == 0 => return,
        Immediate(_) => return,
        Shared(ptr) => match ptr.write().unwrap().deref_mut() {
          Heap { .. } => return,
          Synth { fun, arg, .. } => {
            self.whnf2nf_op(fun.clone());
            *arg = self.whnf_op(arg.clone());
            arg.clone()
          },
        }
      }
    }
  }
  fn write_out(&mut self, mut op: Operand) {
    goodop(&op);
    loop {
      op = match op {
        Immediate(crumbs) => {
          let len = reachesZero1(crumbs);
          copy_to_unaligned(&mut self.output_words[self.output_at.0..], &[crumbs], self.output_at.1, false);
          self.output_at += len as usize;
          return;
        },
        Shared(ptr) => match ptr.read().unwrap().deref() {
          Heap { crumbstring, .. } => {
            let len = idxbitlength(crumbstring, 0);
            copy_to_unaligned(&mut self.output_words[self.output_at.0..], &crumbstring, self.output_at.1, false);
            self.output_at += len;
            return;
          },
          Synth { fun, arg, .. } => {
            self.output_words[self.output_at.0] &= mask_hi(self.output_at.1);
            self.output_at += 2;
            self.write_out(fun.clone());
            arg.clone()
          }
        },
      }
    }
  }
  fn write_nf_from_whnf_stack(&mut self) { loop {
    let our_endstop = self.stack_endstop;
    assert!(self.at() > self.stack_endstop);

    self.output_words[self.output_at.0] &= mask_hi(self.output_at.1);
    let start = self.output_at.0 + 1;
    self.output_at += 2 * (self.at() - self.stack_endstop - 1);
    for i in start..self.output_at.0 {
      self.output_words[i] = 0;
    }
    let head = self.stack.pop().unwrap().to_eval;
    self.stack_endstop = self.at();
    self.write_out(head);

    if self.stack_endstop == our_endstop { return; }
    while self.stack_endstop > our_endstop {
      self.stack_endstop -= 1;

      // stack[stack_endstop].was_evaling = NULL; // TODO?

      // This check is a wash on x86_64 and slows down WASM a lot, even though it
      // results in `write_out` being called less?
      // if (could_be_reducible(self.stack[self.stack_endstop].to_eval))
      while self.fuel > 0 && self.step() {
        self.fuel -= 1;
        assert!(self.at() > self.stack_endstop);
      };

      if self.stack_endstop == our_endstop {
        break; // for tail call
      }

      self.write_nf_from_whnf_stack();
      assert!(self.at() == self.stack_endstop);
    }
  } }
  fn eval(&mut self, input_crumbs: usize) -> usize {
    self.input_at = idxbit(0,0);
    self.stop_at = idxbit(0,0) + 2*input_crumbs;
    self.output_at = idxbit(0,0);
    self.stack.truncate(0);

    self.skip_leading_zeros();

    if !self.step() { return 0 }
    self.fuel -= 1;
    while self.fuel > 0 && self.step() {
      self.fuel -= 1;
    };

    self.write_nf_from_whnf_stack();
    return (self.output_at - idxbit(0,0))/2;
  }
  // fn eval_vec(&mut self, input: Vec<u64>) -> Vec<u64> {
  //   let len = idxbitlength(&input[..], 0);
  //   let crumbs = (len - idxbit(0,0))/2;
  //   self.input_words = input;
  //   self.eval(crumbs);
  // }
}

fn main() {
}

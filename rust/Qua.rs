#![feature(mapped_lock_guards)]
#![allow(nonstandard_style)]
#![feature(const_cmp)]
#![feature(const_trait_impl)]
#![feature(const_convert)]
#![feature(import_trait_associated_functions)]
#![feature(derive_const)]
use std::{fmt::Debug, ops::{Deref, DerefMut}, sync::{Arc, MappedRwLockReadGuard, RwLock, RwLockReadGuard, Weak}};

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
// const mask_even: word = word_max - 1;
const lower: word = word_max / 0b11;
// const upper: word = 0b10 * lower;
// const word_bytes: word = word_max / 0b11111111;

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

fn toAtomic(mut crumbs: word) -> word {
  crumbs >>= crumbs.trailing_zeros() & !1;
  let exp = deltaExpecting(crumbs);
  return crumbs << 2*(1 - exp);
}

#[derive(Clone, Copy, Debug)]
#[derive_const(PartialEq, Eq, PartialOrd, Ord)]
struct idxbit(usize, u8);

const fn idxbitlength(crumbstring: &[word], _known_words: word) -> idxbit {
  let mut i: usize = 0;
  let mut exp: i64 = 1;
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
    offset = reachesZero(exp as u64, crumbstring[i]);
    if offset == 0 {
      // If not, we need to continue with the next word
      // and adjust the expected number of operands
      exp -= deltaExpecting(crumbstring[i]) as i64;
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
impl std::ops::Sub<usize> for idxbit {
  type Output = idxbit;
  fn sub(self, rhs: usize) -> Self::Output {
    let idx = self.0 - rhs / word_size as usize;
    let rhs = (rhs % word_size as usize) as u8;
    if rhs > self.1 {
      return idxbit(idx - 1, self.1 + word_size - rhs);
    } else {
      return idxbit(idx, self.1 - rhs);
    }
  }
}
impl std::ops::Sub<idxbit> for idxbit {
  type Output = usize;
  fn sub(self, rhs: idxbit) -> Self::Output {
    return (self.0 - rhs.0)*(word_size as usize) + self.1 as usize - rhs.1 as usize;
  }
}
impl std::ops::SubAssign<usize> for idxbit {
  fn sub_assign(&mut self, incr_bits: usize) {
    *self = *self - incr_bits
  }
}
impl Default for idxbit {
  fn default() -> Self {
    idxbit(0,0)
  }
}

fn copy_to_unaligned(dst: &mut [word], src: &[word], offset: u8, clobber: bool) {
  if src.is_empty() { return }
  assert!(dst.len() >= src.len());
  debug_assert!(offset < word_size);
  if offset == 0 { return dst[..src.len()].copy_from_slice(src); }
  let mut hanging = if clobber { 0 } else { dst[0] };
  for (i, &word) in src.iter().enumerate() {
    dst[i] = word_cat(offset, hanging, word_size - offset, word);
    hanging = word << (word_size - offset);
  }
  if hanging != 0 { dst[src.len()] = hanging; }
}
fn copy_from_unaligned(dst: &mut [word], src: &[word], offset: u8) {
  if src.is_empty() { return }
  assert!(dst.len() >= src.len(), "{} >= {} at {offset}", dst.len(), src.len());
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
  dst[src.len() - 1] = last << offset;
}

#[derive(Clone, Default)]
struct BitString {
  words: Vec<word>,
  at: idxbit,
}

impl BitString {
  fn new(words: Vec<word>, ib: idxbit) -> BitString {
    BitString { words, at: ib }
  }
  fn _full_words(&self) -> &[word] {
    return &self.words[..self.at.0];
  }
  fn _last_word(&self) -> word {
    debug_assert!(self.words.len() > self.at.0 || self.at == idxbit(self.words.len(), 0));
    if self.words.len() == self.at.0 { return 0 }
    return self.words[self.at.0] & mask_hi(self.at.1);
  }
}

impl PartialEq for BitString {
  fn eq(&self, other: &Self) -> bool {
    return
      self.at == other.at &&
      self._last_word() == other._last_word() &&
      self._full_words() == other._full_words();
  }
}
impl Eq for BitString {}

impl std::ops::Index<usize> for BitString {
  type Output = word;
  fn index(&self, index: usize) -> &Self::Output {
    &self.words[index]
  }
}
impl std::ops::IndexMut<usize> for BitString {
  fn index_mut(&mut self, index: usize) -> &mut Self::Output {
    &mut self.words[index]
  }
}

impl BitString {
  fn len(&self) -> usize {
    self.at.0 + (self.at.1 != 0) as usize
  }
  fn extend(&mut self, other: &BitString) {
    let was_at = self.at;
    self.at += other.at;
    let pad = if other.at.1 > 0 { 2 } else { 1 };
    let needed = self.at.0 + pad;
    if self.words.len() < needed {
      self.words.extend(&vec![0; needed - self.words.len()]);
    }
    copy_to_unaligned(&mut self.words[was_at.0..], &other.words[..], was_at.1, false);
  }
  fn crumb(val: u8) -> BitString {
    BitString { words: vec![(val as word) << (word_size as word - 2)], at: idxbit(0,2) }
  }
}

#[derive(Clone, Default, PartialEq, Eq)]
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
impl From<SKI> for CrumbString<true> {
  fn from(combinator: SKI) -> Self {
    CrumbString(BitString::new(vec![(combinator as word) << (word_size - 2)], idxbit(0,2)))
  }
}

impl<const is_operand: bool> std::ops::Index<usize> for CrumbString<is_operand> {
  type Output = word;
  fn index(&self, index: usize) -> &Self::Output {
    &self.0[index]
  }
}
impl<const is_operand: bool> std::ops::IndexMut<usize> for CrumbString<is_operand> {
  fn index_mut(&mut self, index: usize) -> &mut Self::Output {
    &mut self.0[index]
  }
}

impl<const is_operand: bool> CrumbString<is_operand> {
  fn len(&self) -> usize {
    self.0.len()
  }
  fn _last_word(&self) -> word {
    self.0._last_word()
  }
  fn _full_words(&self) -> &[u64] {
    self.0._full_words()
  }
  fn extend(&mut self, other: &CrumbString<is_operand>) {
    self.0.extend(&other.0);
  }
  fn write0crumb(&mut self) {
    self.0.extend(&BitString { words: vec![0], at: idxbit(0,2) });
  }
  fn write0crumbs(&mut self, amt: usize) {
    let at = idxbit(0,0) + 2*amt;
    let words = vec![0; (amt / word_crumbs as usize)+1];
    self.0.extend(&BitString { words, at });
  }
  fn ski(val: SKI) -> CrumbString<is_operand> {
    CrumbString(BitString::crumb(val as u8))
  }
  fn crumb_at(&self, i: idxbit) -> u8 {
    let v = (mask_lo(2) & (self[i.0] >> (word_size - 2 - (i.1 & !1)))) as u8;
    return v;
  }
  fn parse(val: &str) -> CrumbString<false> {
    let mut out = CrumbString(BitString{ words: vec![], at: idxbit(0,0) });
    for c in val.chars() {
      match c {
        '0' => out.0.extend(&BitString::crumb(0)),
        '1' => out.0.extend(&BitString::crumb(1)),
        '2' => out.0.extend(&BitString::crumb(2)),
        '3' => out.0.extend(&BitString::crumb(3)),
        _ => panic!("Bad char in crumbstring")
      }
    }
    return out;
  }
  fn unparse(&self) -> String {
    let mut out = String::with_capacity((self.0.at - idxbit(0,0))/2);
    for i in 0..out.capacity() {
      out.push(match self.crumb_at(idxbit(0,0) + 2*i) {
        0 => '0',
        1 => '1',
        2 => '2',
        3 => '3',
        c => panic!("Bad crumb {}", c)
      })
    }
    return out;
  }
  fn sugar(val: &str) -> CrumbString<false> {
    let mut out = CrumbString(BitString{ words: vec![], at: idxbit(0,0) });
    let mut chars = val.chars();
    while let Some(c) = chars.by_ref().next() {
      match c {
        '0' | 'P' | '`' => out.0.extend(&BitString::crumb(0)),
        '1' | 'I' => out.0.extend(&BitString::crumb(1)),
        '2' | 'K' => out.0.extend(&BitString::crumb(2)),
        '3' | 'S' => out.0.extend(&BitString::crumb(3)),
        'B' => out.extend(&CrumbString::from(toAtomic(0b00001100101110))),
        'C' => out.extend(&CrumbString::from(toAtomic(0b00001100001100100000110010111011001010))),
        'Y' => out.extend(&CrumbString::from(toAtomic(0b0000001111100000110010000011110011000011111010))),
        '*' => out.extend(&CrumbString::from(toAtomic(0b00001100101110))),
        '+' => out.extend(&CrumbString::from(toAtomic(0b0000110010110011001000001100101110))),
        '(' | ' ' | ')' => {},
        '#' => {
          let num: String = chars.by_ref().take_while(|c| '0' <= *c && *c <= '9').collect();
          let value = num.parse::<u64>().unwrap();
          for _ in 0..value {
            // 0
            out.0.extend(&BitString::crumb(0));
            // 0SB
            out.extend(&CrumbString::from(toAtomic(0b001100001100101110)));
          }
          // 0KI
          out.extend(&CrumbString::from(toAtomic(0b001001)));
        },
        _ => panic!("Bad char in crumbstring {}", c)
      }
    }
    return CrumbString(out.0);
  }
}

impl<const is_operand: bool> Debug for CrumbString<is_operand> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str("0q")?;
    f.write_str(self.unparse().as_str())
  }
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
impl Debug for Operand {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      &Immediate(0) => f.write_str("0."),
      &Immediate(w) =>
        f.write_str(CrumbString::<true>::from(w).unparse().as_str()),
      Shared(ptr) =>
        ptr.read().unwrap().deref().fmt(f)
    }
  }
}

#[derive(Clone)]
enum SharedOperand {
  Synth { fun: Operand, arg: Operand,     work: u64 },
  Heap  { crumbstring: CrumbString<true>, work: u64 },
}
use SharedOperand::*;

impl Debug for SharedOperand {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Synth { fun, arg, work } =>
        write!(f, "({:?} {:?})+{}", fun, arg, work),
      Heap { crumbstring, work } =>
        write!(f, "[{:?}]+{}", crumbstring, work),
    }
  }
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

#[derive(Default, Debug)]
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

#[derive(Debug)]
struct System {
  input_at: idxbit,
  input_words: CrumbString<false>,
  output_words: CrumbString<true>,
  work: u64,
  fuel: u64,
  stack_endstop: usize,
  // stack_top: usize,
  stack: Vec<StackFrame>,
}

#[cfg(debug_assertions)]
fn goodop(op: &Operand) {
  match op {
    &Immediate(crumbs) => debug_assert_ne!(reachesZero1(crumbs), 0),
    Shared(ptr) => match ptr.read().unwrap().deref() {
      Heap { crumbstring, .. } => {
        debug_assert_ne!(crumbstring.len(), 0);
        let calculated = idxbitlength(&crumbstring.0.words[..], 0);
        debug_assert_ne!(calculated, idxbit(0,0));
        debug_assert_eq!(calculated, crumbstring.0.at);
      },
      Synth { fun, arg, .. } => {
        //goodop(fun); goodop(arg)
      },
    },
  }
}
#[cfg(not(debug_assertions))]
fn goodop(op: &Operand) {}


impl From<CrumbString<true>> for SharedOperand {
  fn from(crumbstring: CrumbString<true>) -> SharedOperand {
    debug_assert_ne!(idxbitlength(&crumbstring.0.words[..], 0), idxbit(0,0));
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
        *self = Heap { crumbstring: crumbs.into(), work: self.get_work() };
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
        self = Operand::new(Heap { crumbstring: crumbs.into(), work: 0 });
        (self.clone(), self)
      }
    }
  }
  fn fast_unconstant(&self) -> Option<Operand> {
    let try_heap = match self {
      Immediate(crumbs) => {
        if match_crumbs(0x2, 2, self) {
          return Some(Immediate(crumbs << 4));
        } else { None }
      },
      Shared(ptr) => match ptr.read().unwrap().deref() {
        Synth { fun, arg, .. } =>
          if isK(fun) { return Some(arg.clone()) } else { None },
        Heap { crumbstring, .. } =>
          if match_crumbs(0x2, 2, self) {
            if crumbstring.len() == 1 {
              return Some(Immediate(crumbstring._last_word() << 4));
            }
            if Arc::strong_count(ptr) == 1 {
              Some(ptr)
            } else {
              None
            }
          } else { None }
      }
    }?;
    // There is no way to upgrade a read guard into a write guard
    // (At least not with this design of RwLock)
    let mut guard = try_heap.try_write().ok()?;
    let Heap { crumbstring, .. } = guard.deref_mut() else { return None };
    let mut last_bits = 0;
    for i in (0..crumbstring.len()).rev() {
      let bits_were = crumbstring[i] >> (word_size - 4);
      assert!(bits_were < 0x10);
      crumbstring[i] = (crumbstring[i] << 4) | last_bits;
      last_bits = bits_were;
    }
    crumbstring.0.at -= 4;
    assert!(last_bits == 0x2);
    drop(guard);
    goodop(&self);
    return Some(self.clone());
  }
  fn could_be_reducible(&self) -> bool {
    match self {
      Immediate(crumbs) =>
        return redexes(0, *crumbs) != 0,
      Shared(ptr) => match ptr.read().unwrap().deref() {
        Heap { crumbstring, .. } => {
          let mut exp: i64 = 1;
          let mut trailing = 0;
          for &crumbs in &crumbstring.0.words {
            if redexes(trailing, crumbs) != 0
            { return true }
            if reachesZero(exp as u64, crumbs) != 0
            { return false }
            exp -= deltaExpecting(crumbs) as i64;
            if exp == 0 { return false; }
            trailing = crumbs.trailing_zeros() as u8;
          }
        },
        _ => { return true },
      }
    }
    return false;
  }
  fn wrap0302(self) -> Operand {
    Operand::new(Synth {
      fun: S.into(),
      arg: Operand::new(Synth {
        fun: K.into(),
        arg: self,
        work: 0,
      }),
      work: 0,
    })
  }
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
    MappedRwLockReadGuard<'a, CrumbString<true>>,
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
    let mut tmp1 = CrumbString::from(I);
    let mut tmp2 = CrumbString::from(I);
    let fref = match &fbuf {
      Err(w) => { tmp1[0] = *w; tmp1.0.at.1 = reachesZero1(*w); &tmp1 },
      Ok(g) => g.deref()
    };
    let aref = match &abuf {
      Err(w) => { tmp2[0] = *w; tmp2.0.at.1 = reachesZero1(*w); &tmp2 },
      Ok(g) => g.deref()
    };
    let mut ret = CrumbString::default();
    ret.0.at.1 = 2;
    ret.extend(fref);
    ret.extend(aref);
    debug_assert!(
      ret.0.at == fref.0.at + aref.0.at + 2,
      "{:?} == {:?} + {:?} + 2",
      ret.0.at, fref.0.at, aref.0.at,
    );
    // println!("apped: {ret:?} {fref:?}={:0x} {aref:?}={:0x}", fref[0], aref[0]);
    return Operand::new(SharedOperand::from(ret));
  }
  return synthetic_apply(fun, arg);
}

fn _scan1(crumbstring: &[word], mut input_at: idxbit, stop_at: idxbit) -> idxbit {
  let failed = input_at;
  // adjust the expected number of nodes by the leading nonzeros *crumbs*
  // we will add (could use leading zeros, but this seems more robust to
  // optimizations in `reachesZero()`)
  let mut exp = 1 + (input_at.1 >> 1) as i64;
  while input_at < stop_at {
    let crumbs = crumbstring[input_at.0] | mask_hi(input_at.1);
    input_at.1 = reachesZero(exp as u64, crumbs);
    if input_at.1 != 0 {
      input_at += 0;
      return if input_at <= stop_at { input_at } else { failed }
    }
    input_at.0 += 1;
    input_at.1 = 0;
    exp -= deltaExpecting(crumbs) as i64;
    debug_assert!(exp > 0);
  }
  return failed;
}

fn scan1op(crumbstring: &[word], input_at: idxbit, stop_at: idxbit) -> Option<(idxbit, Operand)> {
  let next = _scan1(crumbstring, input_at, stop_at);
  let crumblen = (next - input_at)/2;
  if crumblen == 0 { 
    // println!("Underflow from {input_at:?}, {}", &CrumbString::<false>(BitString { words: Vec::from(crumbstring), at: stop_at }).unparse()[(input_at - idxbit(0,0))/2..]);
    return None
  }
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
    let pad = if input_at.1 > 0 { 2 } else { 1 };
    let mut dst = vec![0; pad + crumblen/word_crumbs as usize];
    copy_from_unaligned(&mut dst[..], &crumbstring[input_at.0..=next.0], input_at.1);
    Operand::new(Heap { crumbstring: dst.into(), work: 0 })
  }))
}

impl System {
  fn new() -> System {
    System {
      input_at: idxbit(0,0),
      input_words: CrumbString::default(),
      output_words: CrumbString::default(),
      work: 0,
      fuel: 0x1000000,
      stack_endstop: 0,
      // stack_top: usize,
      stack: vec![],
    }
  }
  fn at(&self) -> usize { return self.stack.len() }
  fn scan1input(&mut self) -> Option<Operand> {
    let (input_at, op) = scan1op(&self.input_words.0.words[..], self.input_at, self.input_words.0.at)?;
    // println!("{op:?} from {input_at:?}: {}", &self.input_words.unparse()[(input_at - idxbit(0,0))/2..]);
    self.input_at = input_at;
    return Some(op);
  }
  fn refill(&mut self) -> Option<StackFrame> {
    if self.at() > self.stack_endstop {
      return self.stack.pop();
    }
    debug_assert!(self.at() == self.stack_endstop);
    // EOF
    if self.input_at == self.input_words.0.at { return None }
    debug_assert!(self.stack_endstop == 0);
    match self.scan1input() {
      None => {
        self.input_at += 2;
        let frame = self.refill()?;
        return Some(StackFrame {
          to_eval: frame.to_eval.wrap0302(),
          was_evaling: frame.was_evaling
        });
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
    while self.input_at.0 < self.input_words.0.at.0 && self.input_words[self.input_at.0] == 0 {
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
    goodop(&op);
    let obtain = match &op {
      &Immediate(crumbs) => {
        if crumbs >> (word_size - 2) != 0 { return SKI::from(crumbs >> (word_size - 2)) }
        Err(crumbs)
      },
      Shared(ptr) =>
        Ok(RwLockReadGuard::map(ptr.read().unwrap(),|v| match v {
          Heap { crumbstring, .. } => crumbstring,
          Synth { .. } => panic!("Should not by synth"),
        })),
    };
    let mut tmp = CrumbString::from(I);
    let crumbstring = match &obtain {
      Err(w) => { tmp[0] = *w; tmp.0.at.1 = reachesZero1(*w); &tmp },
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
    // println!("arity: {arity}/{at:?}, stack size: {}, {:?}", self.stack.len(), crumbstring);
    self.stack.extend((1..=arity+1).map(|_| StackFrame::default()));
    for stacked in 1..=arity+1 {
      let scanned = scan1op(&crumbstring.0.words[..], at, idxbit(crumbstring.len(),0)).unwrap();
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
        if isK(&x) {
          drop(x); drop(y);
          self.stack.push(z.into());
          self.work += 2;
          return true;
        }
        let case: Result<(Operand, Operand), (Operand, Operand)> = match y.fast_unconstant() {
          Some(yz) => {
            self.work += 1;
            Ok((z, yz))
          },
          _ => {
            match x.fast_unconstant() {
              Some(xz) => {
                let yz = apply(y, z);
                Err((xz, yz))
              },
              _ => {
                let (z, z2) = z.dup();
                let yz = apply(y, z2);
                Ok((z, yz))
              }
            }
          },
        };
        match case {
          Ok((z, yz)) => {
            // Promote a reducible immediate to a shared operand
            self.stack.push(yz.into());
            self.stack.push(z.into());
            if isI(&x) {
              self.work += 1;
            } else
            {
              self.stack.push(x.into());
            }
            self.work += 1;
          },
          Err((xz, yz)) => {
            self.stack.push(yz.into());
            if isI(&xz) {
              self.work += 1;
            } else
            {
              self.stack.push(xz.into());
            }
            self.work += 1;
          },
        }
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
    if !op.could_be_reducible() { return op }
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
          self.output_words.extend(&CrumbString::from(crumbs));
          return;
        },
        Shared(ptr) => match ptr.read().unwrap().deref() {
          Heap { crumbstring, .. } => {
            self.output_words.extend(&crumbstring);
            return;
          },
          Synth { fun, arg, .. } => {
            self.output_words.write0crumb();
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

    self.output_words.write0crumbs(self.at() - self.stack_endstop - 1);
    let head = self.stack.pop().unwrap().to_eval;
    self.stack_endstop = self.at();
    self.write_out(head);

    if self.stack_endstop == our_endstop { return; }
    while self.stack_endstop > our_endstop {
      self.stack_endstop -= 1;

      // stack[stack_endstop].was_evaling = NULL; // TODO?

      // This check is a wash on x86_64 and slows down WASM a lot, even though it
      // results in `write_out` being called less?
      if self.stack.last().unwrap().to_eval.could_be_reducible() {
        while self.fuel > 0 && self.step() {
          self.fuel -= 1;
          assert!(self.at() > self.stack_endstop);
        };
      }

      if self.stack_endstop == our_endstop {
        break; // for tail call
      }

      self.write_nf_from_whnf_stack();
      assert!(self.at() == self.stack_endstop);
    }
  } }
  fn do_eval(&mut self, fuel: u64) -> u64 {
    self.input_at = idxbit(0,0);
    self.stack_endstop = 0;
    self.stack.truncate(0);
    self.output_words = CrumbString::default();
    self.fuel = fuel;

    self.skip_leading_zeros();

    // println!("0: {:#?}", self);

    if self.fuel == 0 {
      let mut temp: Vec<StackFrame> = Vec::new();
      while let Some(v) = self.refill() {
        temp.push(v.into());
      }
      temp.reverse();
      self.stack.extend(temp);
    }
    while self.fuel > 0 && self.step() {
      self.fuel -= 1;
    };

    // println!("f: {:#?}", self);
    for StackFrame { to_eval, .. } in &self.stack {
      goodop(to_eval);
    }
    self.write_nf_from_whnf_stack();
    return fuel - self.fuel;
  }
  fn eval_crumbstring(&mut self, input: CrumbString<false>, fuel: u64) -> (u64, CrumbString<true>) {
    self.input_words = input;
    let used = self.do_eval(fuel);
    return (used, std::mem::replace(&mut self.output_words, CrumbString::default()));
  }
  fn eval_string(&mut self, input: &str, fuel: u64) -> (u64, String) {
    // println!("eval: {input}");
    let (used, ret) = self.eval_crumbstring(CrumbString::<false>::sugar(input), fuel);
    return (used, ret.unparse());
  }
}

fn main() {
  let mut sys = System::new();
  let mut tests: u64 = 0;
  let mut test = |input, fuel| {
    let (used, output) = sys.eval_string(input, fuel);
    // println!("{} -> {} in {} <= {}", input, output, used, fuel);
  };
  // test("2", 10);
  // test("3", 10);
  // test("021", 10);
  // test("03021", 10);
  // test("03011", 10);
  // test("03031", 10);
  // test("0001301", 10);

  let repeat = |amt, s: &str| -> String {
    vec![s.chars(); amt].iter().flat_map(|x| x.clone()).collect()
  };

  // let sus: Option<&str> = None;
  let mut testcase = |input, output, fuel| {
    // if sus.is_some() && Some(input) != sus { return }
    let (used, actual) = sys.eval_string(input, fuel);
    // println!("{} -> {} = {} in {} <= {}", input, actual, output, used, fuel);
    assert_eq!(output, actual);
    tests += 1;
  };
  testcase("00231", "3", 1);
  testcase("0100231", "3", 2);
  testcase("3", "3", 1);
  testcase("033", "033", 1);
  testcase("0100231", "00231", 1);
  testcase("00231", "3", 2);
  testcase("3", "3", 2);
  testcase("0100231", "3", 4);
  testcase("0100231", "3", 30);
  testcase("000022330030232", "3", 25);
  testcase("03000022330030232", "033", 25);
  testcase("0030030232000022330030232", "00300302323", 25);
  testcase("010000030030200302323022330030232", "0000030030200302323022330030232", 1);
  testcase("00210000030030200302323022330030232", "1", 1);
  testcase("010000030030200302323022330030232", "00300302323", 25);


  for m in ["3"] {
  for n in ["3", "2", "032"] {
  for o in ["3", "2", "1", "032", "021", "0030232"] {
    let mut t = |s: &str, fuel| {
      let z = s.replace("m", m).replace("n", n).replace("o", o);
      let (input, output) = z.split_once(" = ").unwrap();
      let (used, actual) = sys.eval_string(input, fuel);
      assert_eq!(output, actual);
      tests += 1;
    };
    t("000Bmno = 0m0no", 4);
    t("000302mno = 0m0no", 2);
    t("000Cmno = 00mon", 10);
    t("000030203m2no = 00mon", 4);
    t("0003m02no = 00mon", 2);
    t("000302m02no = 0mn", 3);
    // these bump it up over word size for some cases
    t("010000030030200302323022mno = 00mon", 11);
    t("01010000030030200302323022mno = 00mon", 12);
    t("002(0030232)(00000300302(B)3022mno) = 0030232", 1);
    t("002(o)(0YY) = o", 100);
    t("00(00SB(00SB(0SK)))mn = 0m0mn", 13);
    t("00(#1)mn = 0mn", 13);
    t("00(#2)mn = 0m0mn", 13);
    t("00(00SBI)mn = 0m0mn", 13);
    t("0000*(00SBI)(#1)mn = 0m0mn", 25);
    // mul 2 2 = 4
    t("0000*(00SBI)(00SBI)mn = 0m0m0m0mn", 25);
    t("0000+(00SBI)(00SBI)mn = 0m0m0m0mn", 23);
    t("0000*(#2)(00SBI)mn = 0m0m0m0mn", 30);
    t("0000+(#2)(00SBI)mn = 0m0m0m0mn", 28);
  } } }

  for m in ["3"] {
  for n in ["3", "2", "032"] {
  for x in [0, 1, 2, 3, 4, 6] {
  for y in [0, 1, 2, 3, 4, 6] {
    let mut t = |s: String, fuel| {
      let z = s.replace("m", m).replace("n", n).replace("x", &format!("(#{})", x)).replace("y", &format!("(#{})", y));
      let (input, output) = z.split_once(" = ").unwrap();
      let (used, actual) = sys.eval_string(input, fuel);
      assert_eq!(output, actual);
      tests += 1;
    };
    if x * y <= 12 {
      t(format!("0000*xymn = {}n", repeat(x*y, "0m")), 150);
    }
    t(format!("0000+xymn = {}n", repeat(x+y, "0m")), 100)

    // if (!x && y <= 2) t(`00${sugar(factorial(y))}mn = ${repeat(factorial_calc(y), '0m')}n`, 5000)
  } } } }

  let factorial = "0003020031100302003302003112003020030203003003003102020210220210300302320030200330200302003020302003302021030200302003302220030200330200302003020302003020312031222";
  let asNat = "00300310203003023202021";

  let fact = |mut x| {
    let mut r = 1;
    while x > 1 {
      r *= x;
      x -= 1;
    }
    return r;
  };

  assert!(fact(0) == 1);
  assert!(fact(1) == 1);
  assert!(fact(2) == 2);
  assert!(fact(5) == 120);

  // println!("{:?}", sys.eval_string(&format!("{asNat} 0{factorial} 021"), 100));
  // for i in 0..10 {
  //   println!("{:?}", sys.eval_string(&format!("{asNat} 0{factorial} 0030030232021"), i));
  // }
  // S =>
  // println!("{:?}", sys.eval_string(&format!("{asNat} 0{factorial} 0030030232021"), 0x1B));
  // println!("{:?}", sys.eval_string(&format!("{asNat} 0{factorial} 0030030232021"), 0x1C));
  // println!("{:?}", sys.eval_string("000302000003020033020030200302030200330202103020030200330222003020033020030200302030200302031203122200031100030200330200311200302003020300300300310202021022021030030232003020033020030200302030200330202103020030200330222003020033020030200302030200302031203122200300302320210300302321021", 0x1));
  // println!("{:?}", sys.eval_string(&format!("{asNat} 0{factorial} 00300302320030030232021"), 10000000));

  for x in [0, 1, 2, 3, 4, 5] {
  println!("{tests} passed, {x}");
  for n in ["3", "2", "032"] {
  for o in ["2", "3", "1", "032", "021", "0030232"] {
    let mut t = |s: String, fuel| {
      let z = s.replace("n", n).replace("o", o);
      let v = x;
      let (input, output) = z.split_once(" = ").unwrap();
      let (used, actual) = sys.eval_string(input, fuel);
      // println!("{v}: {} -> {} = {} in {} <= {}", input, actual, output, used, fuel);
      assert_eq!(output, actual);
      tests += 1;
    };
    t(format!("000{factorial}(#{x})no = {}o", repeat(fact(x), "0n")), [1000,1000,10000,10000,100000,1000000,10000000,100000000,1000000000][x]);
    if n == "3" && o == "2" {
      t(format!("{asNat} P{factorial}(#{x}) = {}021", repeat(fact(x), "0030030232")), 123248203829023423);
    }
  } } }

  println!("{tests} passed");
}

#![feature(uint_bit_width)]
#![feature(random)]
use num_bigint::BigUint;
use num_traits::{One, Zero};
use std::array;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::BTreeSet;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::num::NonZero;
use std::ops::Deref;
use std::random;
use std::sync::{Arc, Weak};

#[derive(Clone)]
enum HFS {
  NumLike { value: BigUint, base: u8 },
  SetLike { value: BTreeSet<AHFS>, depth: u64 },
}
use HFS::{NumLike, SetLike};
#[derive(Clone)]
#[repr(transparent)]
struct HHFS(HFS);
#[derive(Clone)]
struct AHFS {
  ptr: Arc<HFS>,
  uid: Option<NonZero<u64>>,
}
// #[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
struct WHFS {
  ptr: Weak<HFS>,
  uid: Option<NonZero<u64>>,
}
struct Pool {
  hashconsed: HashMap<HHFS, WHFS>,
  uid: u64,
  hex: [AHFS; 258],
  dec: [AHFS; 111],
  bin: [AHFS; 130],
}

impl Deref for AHFS {
  type Target = HFS;
  fn deref(&self) -> &HFS {
    self.ptr.deref()
  }
}

impl Pool {
  fn new() -> Pool {
    let mut uid = 0;
    let make = |base: u8| move |value: usize| -> AHFS {
      uid += 1;
      return AHFS { ptr: Arc::new(NumLike { value: value.into(), base }), uid: NonZero::new(uid) }
    };
    return Pool {
      hashconsed: HashMap::new(),
      uid: uid,
      hex: array::from_fn(make(16)),
      dec: array::from_fn(make(10)),
      bin: array::from_fn(make(2)),
    }
  }
  fn mk_num(&mut self, value: BigUint, base: u8) -> AHFS {
    match base {
      16 => if let Some(r) = self._lit(&value, self.hex.as_slice()) { return r },
      10 => if let Some(r) = self._lit(&value, self.dec.as_slice()) { return r },
      2  => if let Some(r) = self._lit(&value, self.bin.as_slice()) { return r },
      _  => {}
    }
    self.mk_hfs(NumLike { value, base })
  }
  fn _lit(&self, value: &BigUint, table: &[AHFS]) -> Option<AHFS> {
    if *value < table.len().into() {
      let i: usize = value.try_into().ok()?;
      table.get(i).cloned()
    } else { None }
  }
  fn mk_hex(&mut self, value: BigUint) -> AHFS {
    self.mk_num(value, 16)
  }
  fn mk_dec(&mut self, value: BigUint) -> AHFS {
    self.mk_num(value, 10)
  }
  fn mk_bin(&mut self, value: BigUint) -> AHFS {
    self.mk_num(value, 2)
  }
  fn mk_hfs(&mut self, value: HFS) -> AHFS {
    match &value {
      NumLike { value, base } => match base {
        16 => if let Some(r) = self._lit(&value, self.hex.as_slice()) { return r },
        10 => if let Some(r) = self._lit(&value, self.dec.as_slice()) { return r },
        2  => if let Some(r) = self._lit(&value, self.bin.as_slice()) { return r },
        _ => {},
      },
      _ => {}
    }
    if let Some(r) = self.hashconsed.get(&HHFS(value.clone()))
    && let Some(ptr) = r.ptr.upgrade() {
      return AHFS { ptr: ptr, uid: r.uid };
    }

    self.uid += 1;
    let uid = NonZero::new(self.uid);
    let cloned = AHFS { ptr: Arc::new(value.clone()), uid: uid };
    let whfs = WHFS { ptr: Arc::downgrade(&cloned.ptr), uid: cloned.uid };
    self.hashconsed.insert(HHFS(value), whfs);
    if self.uid % 512 == 0 { self.remove_expired(); }
    return cloned;
    return AHFS { ptr: Arc::new(value.clone()), uid: None };
  }
  fn remove_expired(&mut self) {
    self.hashconsed.retain(|_, v| v.ptr.upgrade().is_some());
  }
  fn fully_remove_expired(&mut self) {
    loop {
      let start = self.hashconsed.len();
      self.remove_expired();
      let end = self.hashconsed.len();
      if start == end { return }
    }
  }
  fn num_to_set(&mut self, value: BigUint) -> AHFS {
    let mut creating = BTreeSet::new();
    for place in num::bits(&value) {
      creating.insert(self.mk_hex(place.into()));
    }
    return self.mk_hfs(SetLike { value: creating, depth: num::depth(&value) });
  }
  fn num_to_hfs(&mut self, value: BigUint) -> AHFS {
    let mut creating = BTreeSet::new();
    for place in num::bits(&value) {
      creating.insert(self.num_to_hfs(place.into()));
    }
    return self.mk_hfs(SetLike { value: creating, depth: num::depth(&value) });
  }
  fn unsingle(&mut self, wrapped: &AHFS) -> Option<AHFS> {
    if !wrapped.is_singleton() { return None }
    match wrapped.deref() {
      NumLike { value, .. } => Some(self.mk_hex(value.trailing_zeros()?.into())),
      SetLike { value, .. } => value.first().map(|v| v.clone())
    }
  }
}

mod num {
  use std::iter;
  use num_bigint::BigUint;
  use num_traits::{One, Zero};

  pub fn depth(v: &BigUint) -> u64 {
    match v.bits() {
      n if n <= 3 => n,
      n if n <= 4 => 3,
      n if n <= 16 => 4,
      n if n <= 65536 => 5,
      _ => 6,
    }
  }

  pub fn bits<'a>(v: &'a BigUint) -> Biter<'a> {
    Biter { digits: v.iter_u64_digits().enumerate(), count: 0, word: 0 }
  }
  pub fn back_bits<'a>(v: &'a BigUint) -> BackBiter<'a> {
    BackBiter { digits: v.iter_u64_digits().enumerate(), count: 0, word: 0 }
  }

  pub struct Biter<'a> {
    digits: iter::Enumerate<num_bigint::U64Digits<'a>>,
    count: u64,
    word: u64,
  }
  pub struct BackBiter<'a> {
    digits: iter::Enumerate<num_bigint::U64Digits<'a>>,
    count: u64,
    word: u64,
  }

  impl<'a> Iterator for Biter<'a> {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
      while self.word == 0 {
        let thingy = self.digits.next_back()?;
        self.count = thingy.0 as u64;
        self.word = thingy.1;
      }
      let bit = 63 - self.word.leading_zeros() as u64;
      self.word &= !(1 << bit);

      Some(64*(self.count as u64) + (bit as u64))
    }
  }
  impl<'a> Iterator for BackBiter<'a> {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
      while self.word == 0 {
        let thingy = self.digits.next()?;
        self.count = thingy.0 as u64;
        self.word = thingy.1;
      }
      let bit = self.word.trailing_zeros() as u64;
      self.word &= !(1 << bit);

      Some(64*(self.count as u64) + (bit as u64))
    }
  }

}

impl HFS {
  fn depth(&self) -> u64 {
    match self {
      NumLike { value, .. } => num::depth(&value),
      SetLike { depth, .. } => (*depth).into(),
    }
  }
  fn count(&self) -> u64 {
    match self {
      NumLike { value, .. } => value.count_ones(),
      SetLike { value, .. } => value.len() as u64,
    }
  }
  fn width(&self) -> Option<u64> {
    match self {
      NumLike { value, .. } => Some(value.bits()),
      SetLike { value, depth } => {
        if *depth > 7 { return None }
        if let Some(first) = value.last() {
          let nat = first.ptr.as_nat()?;
          return nat.try_into().ok().map(|v: u64| v+1);
        } else {
          return Some(0);
        }
      },
    }
  }
  fn as_nat(&self) -> Option<BigUint> {
    match self {
      NumLike { value, .. } => Some(value.clone()),
      SetLike { value, depth } => {
        if *depth > 6 { return None }
        let w = self.width()?;
        if w > isize::MAX as u64 { return None }
        let mut v = BigUint::zero();
        for child in value {
          v.set_bit(child.ptr.as_u64()?, true);
        }
        return Some(v);
      },
    }
  }
  fn as_u64(&self) -> Option<u64> {
    if self.depth() > 6 { return None }
    if self.width()? > 64 { return None }
    return self.as_nat()?.try_into().ok();
  }
  fn is_empty(&self) -> bool {
    match self {
      NumLike { value, .. } => value.is_zero(),
      SetLike { depth, .. } => *depth == 0,
    }
  }
  fn is_zero(&self) -> bool { self.is_empty() }
  fn is_one(&self) -> bool {
    match self {
      NumLike { value, .. } => value.is_one(),
      SetLike { depth, .. } => *depth == 1,
    }
  }
  fn is_two(&self) -> bool {
    match self {
      NumLike { value, .. } => *value == 2u32.into(),
      SetLike { depth, value } => *depth == 2 && value.len() == 1,
    }
  }
  fn is_triv(&self) -> bool {
    self.is_zero() || self.is_one()
  }
  fn is_singleton(&self) -> bool {
    self.count() == 1
  }
  fn is_disjoint(&self, other: &HFS) -> bool {
    if self.is_empty() || other.is_empty() { return true }
    fn num_set_disjoint(v: &BigUint, vs: &BTreeSet<AHFS>) -> bool {
      for bit in num::bits(v) {
        let hfs = NumLike { value: bit.into(), base: 16 };
        let ahfs = AHFS { ptr: hfs.into(), uid: None };
        if vs.contains(&ahfs) { return false; }
      }
      return true;
    }
    match (self, other) {
      (NumLike { value: v1, .. }, NumLike { value: v2, .. }) => (v1 & v2).is_zero(),
      (SetLike { value: v1, .. }, SetLike { value: v2, .. }) => v1.is_disjoint(v2),
      (NumLike { value: v, .. }, SetLike { value: vs, .. }) => num_set_disjoint(&v, &vs),
      (SetLike { value: vs, .. }, NumLike { value: v, .. }) => num_set_disjoint(&v, &vs),
    }
  }
  fn is_subset(&self, other: &HFS) -> bool {
    if self.is_empty() { return true }
    if self.depth() > other.depth() { return false }
    if self.count() > other.count() { return false }
    if let Some(w1) = self.width()
    && let Some(w2) = other.width()
    && w1 > w2 { return false }
    fn num_subset_set(v: &BigUint, vs: &BTreeSet<AHFS>) -> bool {
      for bit in num::bits(v) {
        let hfs = NumLike { value: bit.into(), base: 16 };
        let ahfs = AHFS { ptr: hfs.into(), uid: None };
        if !vs.contains(&ahfs) { return false; }
      }
      return true;
    }
    fn set_subset_num(vs: &BTreeSet<AHFS>, v: &BigUint) -> bool {
      for item in vs {
        match item.ptr.as_u64() {
          None => return false,
          Some(bit) => if !v.bit(bit) { return false }
        }
      }
      return true;
    }
    match (self, other) {
      (NumLike { value: v1, .. }, NumLike { value: v2, .. }) => (v1 | v2) == *v2,
      (SetLike { value: v1, .. }, SetLike { value: v2, .. }) => v1.is_subset(v2),
      (NumLike { value: v, .. }, SetLike { value: vs, .. }) => num_subset_set(&v, &vs),
      (SetLike { value: vs, .. }, NumLike { value: v, .. }) => set_subset_num(&vs, &v),
    }
  }
}

impl Eq for HFS {}
impl PartialEq for HFS {
  fn eq(&self, other: &HFS) -> bool {
    if self.depth() != other.depth() { return false; }

    fn eq_num_set(v: &BigUint, vs: &BTreeSet<AHFS>) -> bool {
      if (v.count_ones() as u64) != (vs.len() as u64) { return false; }
      for place in num::bits(v) {
        let hfs = NumLike { value: place.into(), base: 16 };
        let ahfs = AHFS { ptr: hfs.into(), uid: None };
        if !vs.contains(&ahfs) { return false }
      }
      return true;
    }

    match (self, other) {
      (NumLike { value: v1, .. }, NumLike { value: v2, .. }) => v1 == v2,
      (NumLike { value: v, .. }, SetLike { value: vs, .. }) => eq_num_set(&v, &vs),
      (SetLike { value: vs, .. }, NumLike { value: v, .. }) => eq_num_set(&v, &vs),
      (SetLike { value: v1, .. }, SetLike { value: v2, .. }) => v1 == v2,
    }
  }
}

impl PartialOrd for HFS {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    return Some(self.cmp(other));
  }
}
impl Ord for HFS {
  fn cmp(&self, other: &HFS) -> Ordering {
    match self.depth().cmp(&other.depth()) {
      Ordering::Greater => return Ordering::Greater,
      Ordering::Less => return Ordering::Less,
      Ordering::Equal => {},
    }
    fn cmp_num_set(v: &BigUint, vs: &BTreeSet<AHFS>) -> Ordering {
      let mut biter = num::bits(v);
      for member in vs.iter().rev() {
        let left_member: BigUint =
          if let Some(n) = biter.next()
          { n.into() } else { return Ordering::Less };
        let comparison = match member.ptr.as_ref() {
          NumLike { value, .. } => left_member.cmp(&value),
          SetLike { value, .. } => cmp_num_set(&left_member, &value),
        };
        if comparison != Ordering::Equal { return comparison; }
      }
      if biter.next().is_none() {
        return Ordering::Equal;
      } else {
        return Ordering::Greater;
      }
    }
    match (self, other) {
      (NumLike { value: v1, .. }, NumLike { value: v2, .. }) =>
        v1.cmp(&v2),
      (NumLike { value: v, .. }, SetLike { value: vs, .. }) =>
        cmp_num_set(&v, &vs),
      (SetLike { value: vs, .. }, NumLike { value: v, .. }) =>
        cmp_num_set(&v, &vs).reverse(),
      (SetLike { value: v1, .. }, SetLike { value: v2, .. }) =>
        // Compare large values first
        v1.iter().rev().cmp(v2.iter().rev()),
    }
  }
}

impl PartialEq for AHFS {
  fn eq(&self, other: &AHFS) -> bool {
    if self.uid == other.uid && self.uid != None { return true; }
    return self.ptr.as_ref().eq(other.ptr.as_ref());
  }
}
impl Eq for AHFS {}
impl PartialOrd for AHFS {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    return Some(self.cmp(other));
  }
}
impl Ord for AHFS {
  fn cmp(&self, other: &Self) -> Ordering {
    if self.uid == other.uid && self.uid != None { return Ordering::Equal; }
    return self.ptr.as_ref().cmp(other.ptr.as_ref());
  }
}
// impl Hash for AHFS {
//   fn hash<H: Hasher>(&self, hasher: &mut H) {
//     if self.uid != 0 {
//       self.ptr.as_ref().hash(hasher);
//     } else {
//       hasher.write_u64(self.uid);
//     }
//   }
// }

impl Display for HFS {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      NumLike { value, base } =>
        write!(f, "{}", value),
      SetLike { value, depth } =>
        if value.is_empty() {
          write!(f, "{{}}")
        } else if value.len() == 1 && let Some(member) = value.first() {
          write!(f, "{{{}}}", member)
        } else {
          write!(f, "{{ ")?;
          let mut first = true;
          for member in value.iter().rev() {
            if !first { write!(f, ", ")?; }
            first = false;
            write!(f, "{}", member)?;
          }
          write!(f, " }}")
        }
    }
  }
}
impl Display for AHFS {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.ptr.as_ref().fmt(f)
  }
}
impl Debug for AHFS {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.ptr.as_ref().fmt(f)
  }
}

impl HFS {
  fn eq_exact(&self, other: &HFS) -> bool {
    fn eq_member_exact((a, b): (&AHFS, &AHFS)) -> bool {
      if a.uid != None && b.uid != None { return a.uid == b.uid }
      return (a.ptr.as_ref()).eq_exact(b.ptr.as_ref());
    }
    match (&self, &other) {
      (NumLike { value: v1, base: b1 }, NumLike { value: v2, base: b2 }) => b1 == b2 && v1 == v2,
      (SetLike { value: v1, depth: d1 }, SetLike { value: v2, depth: d2 }) =>
        d1 == d2 && v1.len() == v2.len() && v1.iter().zip(v2).all(eq_member_exact),
      (_, _) => false,
    }
  }
}
impl PartialEq for HHFS {
  fn eq(&self, other: &HHFS) -> bool {
    self.0.eq_exact(&other.0)
  }
}
impl Eq for HHFS {}
impl HFS {
  fn hash_exact<H: Hasher>(&self, state: &mut H) {
    match &self {
      NumLike { value, base: _ } => {
        state.write_u8(1);
        value.hash(state);
      }
      SetLike { value, depth: _ } => {
        state.write_u8(2);
        state.write_usize(value.len());
        for elt in value {
          // TODO
          elt.ptr.as_ref().hash_exact(state);
        }
      }
    }
  }
}
impl Hash for HHFS {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.0.hash_exact(state);
  }
}


// Calculate large fibonacci numbers.
fn fib(n: usize) -> BigUint {
    let mut f0 = BigUint::ZERO;
    let mut f1 = BigUint::one();
    for _ in 0..n {
        let f2 = f0 + &f1;
        f0 = f1;
        f1 = f2;
    }
    f0
}

fn main() {
  // This is a very large number.
  println!("fib(100) = {}", fib(100));
  let mut pool = Pool::new();
  let hfs = pool.num_to_set(157842u64.into());
  // println!("hfs: {}", hfs);
  let hfs2 = pool.num_to_set(157842u64.into());
  println!("ptr== {}, sem== {}", std::ptr::eq(hfs.ptr.as_ref(), hfs2.ptr.as_ref()), (hfs) == hfs2);
  let hfs3 = pool.mk_hfs(NumLike { value: 157842u64.into(), base: 10 });
  println!("hfs3: {}", hfs3);
  println!("ptr== {}, sem== {}, exact? {}", std::ptr::eq(hfs.ptr.as_ref(), hfs3.ptr.as_ref()), (hfs) == hfs3, HHFS(hfs.ptr.as_ref().clone()) == HHFS(hfs3.ptr.as_ref().clone()));

  println!("set: {}, hfs: {}", hfs, pool.num_to_hfs(157842u64.into()));

  let bigint = 157842u64.into();
  let bits_dsc: Vec<u64> = num::bits(&bigint).collect();
  let bits_asc: Vec<u64> = num::back_bits(&bigint).collect();
  println!("bits: {bits_dsc:?}, back_bits: {bits_asc:?}");
  println!("size: {}", size_of::<Pool>());
}

#[test]
fn test() {
  let mut pool = Pool::new();
  {
    let one = pool.mk_hex(1u64.into());
    let two = pool.mk_hex(2u64.into());
    let ssz = pool.num_to_hfs(2u64.into());
    assert_eq!(two, ssz);

    let offset: u8 = random::random(std::ops::RangeFull);
    let range =
      (0..=2049)
        .chain((5..=16).map(|v| 1024 * v + offset as u64))
        .chain(65530..=65538);
    for exn in range {
      // As a number
      let num = pool.mk_hfs(NumLike { value: exn.into(), base: 16 });
      let nwidth = num.width().unwrap();
      let ndepth = num.depth();
      let ncount = num.count();
      // As a set of numbers
      let set = pool.num_to_set(exn.into());
      // As a hereditarily finite set
      let hfs = pool.num_to_hfs(exn.into());

      // Test singletons
      let singleton = num.is_singleton();
      let unsingle = pool.unsingle(&num);
      assert_eq!(singleton, unsingle.is_some());
      if let Some(unsingled) = &unsingle {
        assert_eq!(ndepth, unsingled.depth() + 1, "{} = {} = {{{}}}", num, set, unsingled);
      }

      // Assert bit width
      assert_eq!(nwidth, exn.bit_width().into(), "{} = {}", num, set);

      // Check bit iterators
      let bigint = exn.into();
      let bits_dsc: Vec<u64> = num::bits(&bigint).collect();
      let bits_asc: Vec<u64> = num::back_bits(&bigint).collect();
      let bits_asc_dsc = { let mut v = bits_asc.clone(); v.reverse(); v };
      assert_eq!(bits_dsc, bits_asc_dsc);

      for rep in [&num, &set, &hfs] {
        for other in [&num, &set, &hfs] {
          assert_eq!(rep, other);
          assert_eq!(other, rep);
          assert_eq!(rep.cmp(other), Ordering::Equal);
          assert_eq!(other.cmp(rep), Ordering::Equal);
        }
        if exn < 50 {
          for m in 0..=50u64 {
            let mnum = pool.mk_hfs(NumLike { value: m.into(), base: 16 });
            let mset = pool.num_to_set(m.into());
            let mhfs = pool.num_to_hfs(m.into());

            for other in [&mnum, &mset, &mhfs] {
              assert_eq!(rep.eq(other), rep.cmp(other) == Ordering::Equal);
              assert_eq!(rep.eq(other), other.cmp(rep) == Ordering::Equal);
              assert_eq!(rep.cmp(other), other.cmp(rep).reverse());
              assert_eq!(rep.cmp(other), rep.as_u64().cmp(&other.as_u64()), "{} cmp {} != {} cmp {}", rep, other, rep.as_u64().unwrap(), other.as_u64().unwrap());
              assert_eq!(rep.is_subset(other), (exn | m) == m, "{} cmp {} != {} cmp {}", rep, other, rep.as_u64().unwrap(), other.as_u64().unwrap());
              assert_eq!(rep.is_disjoint(other), (exn & m) == 0, "{} cmp {} != {} cmp {}", rep, other, rep.as_u64().unwrap(), other.as_u64().unwrap());
            }
          }
        }
        let width = rep.width().unwrap();
        let depth = rep.depth();
        let count = rep.count();
        assert_eq!(nwidth, width);
        assert_eq!(ndepth, depth);
        assert_eq!(ncount, count);
        assert!(width >= count, "{} >= {} for {}", width, count, set);
        assert!(width >= depth, "{} <= {} for {}", width, depth, set);

        assert_eq!(singleton, rep.is_singleton());
        assert_eq!(unsingle, pool.unsingle(rep));
      }
    }
  }
  pool.fully_remove_expired();
  println!("used: {}, left: {}", pool.uid, pool.hashconsed.len());
}

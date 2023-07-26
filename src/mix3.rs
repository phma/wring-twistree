// Module mix3
// This module splits a buffer (a slice of bytes) into three equal parts, with
// 0, 1, or 2 bytes left over, and mixes them as follows:
//
// The mix function takes three bytes and flips each bit which is not the same
// in all three bytes. This is a self-inverse, nonlinear operation.
//
// The 0th third is traversed forward, the 1st third is traversed backward,
// and the 2nd third is traversed by steps close to 1/φ the length of a third.
// Taking a 16-byte buffer as an example:
// 00 0d 1a 27 34|41 4e 5b 68 75|82 8f 9c a9 b6|c3
// <>            |            <>|<>            |
//    <>         |         <>   |         <>   |
//       <>      |      <>      |   <>         |
//          <>   |   <>         |            <>|
//             <>|<>            |      <>      |
// f7 e8 cf de c9|bc b7 8e 8d 82|75 5a 61 4c 4f|c3

use std::collections::BTreeMap; // factorize returns a Btree
use num_bigint::BigUint;
use num_traits::{Zero, One};
use num_integer::{div_rem,gcd,lcm};
//use num_prime::nt_funcs::factorize64;
use std::mem::replace;

pub fn mix(a: &mut u8, b: &mut u8, c: &mut u8) {
  let mask:u8=(*a|*b|*c)-(*a&*b&*c);
  *a=*a^mask;
  *b=*b^mask;
  *c=*c^mask;
}

pub fn fibo_pair(n: BigUint) -> (BigUint,BigUint) {
  let mut f0: BigUint = Zero::zero();
  let mut f1: BigUint = One::one();
  while f0<=n {
    let f2 = f0 + &f1;
    f0 = replace(&mut f1, f2);
  }
  (f0,f1)
}

pub fn search_dir(n: BigUint) -> (BigUint,i8) {
  let (num,den)=fibo_pair(BigUint::from(2u8)*&n);
  let (q,r)=div_rem(n*num,den.clone());
  if r*BigUint::from(2u8)<den {
    (q,1)
  } else {
    (q+BigUint::from(1u8),-1)
  }
}

pub fn mix3parts(buf: &mut[u8], len: usize, rprime: usize) {
  let mut a=0;
  let mut b=2*len-1;
  let mut c=2*len;
  while a<b {
    // This amounts to mix(buf[a],buf[b],buf[c]).
    // It has to be written in this convoluted way
    // to keep the borrow checker happy.
    let (d,rest)=buf[a..].split_first_mut().unwrap();
    let (e,rest)=rest[b-a-1..].split_first_mut().unwrap();
    let (f,_)=rest[c-b-1..].split_first_mut().unwrap();
    mix(d,e,f);
    a=a+1;
    b=b-1;
    c=c+rprime;
    if c>=3*len {
      c=c-len;
    }
  }
}

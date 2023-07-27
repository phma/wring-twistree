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
use num_prime::nt_funcs::factorize64;
use mod_exp::*;
use std::mem::replace;

pub fn mix(a: u8, b: u8, c: u8) -> (u8,u8,u8) {
  let mask:u8=(a|b|c)-(a&b&c);
  (a^mask,b^mask,c^mask)
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

pub fn carmichael(n: usize) -> usize {
  let facs = factorize64(n as u64);
  let mut ret:usize=1;
  for (p, ex) in facs.iter() {
    let carfac=if *p==2u64 && *ex>=3usize {
      p.pow((*ex as u32)-2)
    } else {
      p.pow((*ex as u32)-1)*(p-1)
    };
    ret=lcm(ret,carfac as usize);
  }
  ret
}

pub fn is_max_order(modl: u64, car: u64, fac: &BTreeMap<u64,usize>, n: u64) -> bool {
// modl is the modulus, car is its Carmichael function,
// fac is the set of prime factors of car,
// and n is the number being tested.
// Returns true if n has maximum order, which implies it's a primitive root
// if modulus has any primitive roots.
  let mut ret:bool=mod_exp(n,car,modl)==1;
  for (p,_) in fac.iter() {
    ret=ret && mod_exp(n,car/p,modl)!=1;
  }
  ret
}

pub fn mix3parts(buf: &mut[u8], len: usize, rprime: usize) {
  let mut a=0;
  let mut b=2*len-1;
  let mut c=2*len;
  while a<b {
    let (d,e,f)=mix(buf[a],buf[b],buf[c]);
    buf[a]=d;
    buf[b]=e;
    buf[c]=f;
    a=a+1;
    b=b-1;
    c=c+rprime;
    if c>=3*len {
      c=c-len;
    }
  }
}

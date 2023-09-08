extern crate lazy_static;
use lazy_static::lazy_static;
use crate::mix3::*;

pub const BLOCKSIZE: u16=32;
pub const TWISTPRIME: u16=37;
const SZ: usize=(BLOCKSIZE/2+1) as usize;
// BLOCKSIZE must be a multiple of 4. Blocks in the process of compression
// can be any size from BLOCKSIZE to 3*BLOCKSIZE in steps of 4. TWISTPRIME is
// the smallest prime greater than BLOCKSIZE, which is relatively prime to all
// block sizes during compression.

lazy_static! {
  // RELPRIMES[0]=find_max_order(32), RELPRIMES[1]=find_max_order[36], etc.
  static ref RELPRIMES: [u16; SZ as usize] = {
    let mut m: [u16; SZ as usize]=[0;SZ as usize];
    for i in 0..SZ {
      m[i]=find_max_order(((BLOCKSIZE as usize)+4*i) as u64) as u16;
    }
    m
  };
}

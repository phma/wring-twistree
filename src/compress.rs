// Module compress
// This module is used in Twistree.
// It compresses a vector of 64 or 96 bytes into 32, using three s-boxes in
// an order specified by the sboxalt argument.

extern crate lazy_static;
use lazy_static::lazy_static;
use crate::mix3::*;
use crate::rotbitcount::*;

pub const BLOCKSIZE: usize=32;
pub const TWISTPRIME: isize=37;
const SZ: usize=BLOCKSIZE/2+1;
// BLOCKSIZE must be a multiple of 4. Blocks in the process of compression
// can be any size from BLOCKSIZE to 3*BLOCKSIZE in steps of 4. TWISTPRIME is
// the smallest prime greater than BLOCKSIZE, which is relatively prime to all
// block sizes during compression.

fn lfsr1(n:u32) -> u32 {
  (n>>1) ^ ((n&1)*0x84802140)
}

lazy_static! {
  // RELPRIMES[0]=find_max_order(32), RELPRIMES[1]=find_max_order[36], etc.
  static ref RELPRIMES: [u16; SZ] = {
    let mut m: [u16; SZ as usize]=[0;SZ];
    for i in 0..SZ {
      m[i]=find_max_order(((BLOCKSIZE+4*i)/3) as u64) as u16;
    }
    m
  };

  static ref LFSR: [u32; 256] = {
    let mut m: [u32; 256]=[0;256];
    for i in 0..256 {
      let mut n=i as u32;
      for _ in 0..8 {
	n=lfsr1(n);
      }
      m[i]=n;
    }
    m
  };
}

fn round_compress(sbox:&[[u8; 256]; 3], buf:&mut Vec<u8>, sboxalt:u32) {
  // The number of bytes in buf is decreased by 4 per round.
  assert!(buf.len()&3==0);
  let mut tmp:Vec<u8> = Vec::new();
  tmp.extend_from_slice(buf);
  let rprime=RELPRIMES[(buf.len()-BLOCKSIZE)/4] as usize;
  let len=buf.len()/3;
  mix3parts(buf,len,rprime);
  for i in 0..buf.len() {
    buf[i]=sbox[((sboxalt as usize)+i)%3][buf[i] as usize];
  }
  rot_bitcount(buf, &mut tmp, TWISTPRIME);
  let mut acc:u32=0xdeadc0de;
  for i in (0..buf.len()).rev() {
    acc=(acc>>8)^LFSR[(acc&255) as usize]^tmp[i] as u32;
    buf[i]=acc as u8;
  }
  buf.truncate(buf.len()-4);
}

pub fn compress(sbox:&[[u8; 256]; 3], buf:&mut Vec<u8>, sboxalt:u32) {
  while buf.len()>BLOCKSIZE {
    round_compress(sbox,buf,sboxalt);
  }
}

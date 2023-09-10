#![allow(arithmetic_overflow)]

// Used by wring
pub mod mix3;

// Used by both wring and twistree
pub mod rotbitcount;
pub mod permute;
pub mod keyschedule;
pub mod sboxes;

// Used by twistree
pub mod blockize;
pub mod compress;

pub mod wring {

use crate::mix3::*;
use crate::rotbitcount::*;
use crate::sboxes::*;

fn n_rounds(n: usize) -> u32 {
  let mut ret=3;
  let mut m=n;
  while m>=3 {
    m/=3;
    ret+=1;
  }
  ret
}

pub fn xorn(n: u64) -> u8 {
  let mut ret=0u8;
  let mut i=n;
  while i>0 {
    ret ^= i as u8;
    i = i>>8;
  }
  ret
}

#[derive(Clone)]
pub struct Wring {
  pub sbox: [[u8; 256]; 3],
  pub inv_sbox: [[u8; 256]; 3],
}

impl Wring {
  pub fn new() -> Wring {
    Wring { sbox: [[0u8; 256]; 3], inv_sbox: [[0u8; 256]; 3] }
  }

  fn set_inv_sbox(&mut self) {
    for i in 0..3 {
      for j in 0..256 {
	self.inv_sbox[i][self.sbox[i][j] as usize]=j as u8;
      }
    }
  }

  pub fn set_key_linear(&mut self) {
    for i in 0..3 {
      for j in 0..256 {
	self.sbox[i][j]=(j as u8).rotate_left((3*i+1) as u32);
      }
    }
    self.set_inv_sbox();
  }

  pub fn set_key(&mut self,str:&[u8]) {
    sboxes(str,&mut self.sbox);
    self.set_inv_sbox();
  }

  fn round_encrypt(&self, src: &mut[u8], dst: &mut[u8], rprime: usize, rond: u32) {
    assert_eq!(src.len(),dst.len());
    let len=src.len()/3;
    mix3parts(src,len,rprime); // this clobbers src
    for i in 0..src.len() {
      src[i]=self.sbox[((rond as usize)+i)%3][src[i] as usize];
    }
    rot_bitcount(src, dst, 1);
    for i in 0..dst.len() {
      dst[i]+=xorn((i^(rond as usize)) as u64);
    }
  }

  fn round_decrypt(&self, src: &mut[u8], dst: &mut[u8], rprime: usize, rond: u32) {
    assert_eq!(src.len(),dst.len());
    let len=src.len()/3;
    for i in 0..src.len() { // this clobbers src
      src[i]-=xorn((i^(rond as usize)) as u64);
    }
    rot_bitcount(src, dst, -1);
    for i in 0..dst.len() {
      dst[i]=self.inv_sbox[((rond as usize)+i)%3][dst[i] as usize];
    }
    mix3parts(dst,len,rprime);
  }

  pub fn encrypt(&self, buf: &mut[u8]) {
    let mut tmp:Vec<u8> = Vec::new();
    tmp.extend_from_slice(buf);
    let nrond=n_rounds(buf.len());
    let rprime=if buf.len()<3 {
      1 // in Haskell, laziness takes care of this
    } else {
      find_max_order((buf.len()/3) as u64)
    } as usize;
    for i in 0..nrond {
      if (i&1)==0 {
	self.round_encrypt(buf,&mut tmp,rprime,i);
      } else {
	self.round_encrypt(&mut tmp,buf,rprime,i);
      }
    }
    if (nrond&1)>0 {
      for i in 0..buf.len() {
	buf[i]=tmp[i];
      }
    }
  }

  pub fn decrypt(&self, buf: &mut[u8]) {
    let mut tmp:Vec<u8> = Vec::new();
    tmp.extend_from_slice(buf);
    let nrond=n_rounds(buf.len());
    let rprime=if buf.len()<3 {
      1 // in Haskell, laziness takes care of this
    } else {
      find_max_order((buf.len()/3) as u64)
    } as usize;
    for i in (0..nrond).rev() {
      if ((nrond-i)&1)==1 {
	self.round_decrypt(buf,&mut tmp,rprime,i);
      } else {
	self.round_decrypt(&mut tmp,buf,rprime,i);
      }
    }
    if (nrond&1)>0 {
      for i in 0..buf.len() {
	buf[i]=tmp[i];
      }
    }
  }
}

}

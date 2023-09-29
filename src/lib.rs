#![allow(arithmetic_overflow)]

// Used by both wring and twistree
pub mod mix3;
pub mod rotbitcount;
pub mod permute;
pub mod keyschedule;
pub mod sboxes;

// Used by twistree
pub mod blockize;
pub mod compress; // uses mix3 and rotbitcount

pub mod wring {

use crate::mix3::*;
use crate::rotbitcount::*;
use crate::sboxes::*; // uses keyschedule and permute

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

#[cfg(test)]
mod tests {
  use super::*;

  fn test_vector(wring: &Wring, plain: &[u8], cipher: &[u8]) {
    let mut buf:Vec<u8> = Vec::new();
    buf.extend_from_slice(&plain);
    wring.encrypt(&mut buf);
    assert_eq!(&buf,&cipher);
  }

  #[test]
  fn test_vectors() {
    let zero8=[0u8; 8];
    let ff8=[255u8; 8];
    let twistree8="Twistree".as_bytes();
    let zero9=[0u8; 9];
    let ff9=[255u8; 9];
    let allornone9="AllOrNone".as_bytes();
    let mut linear_wring=Wring::new();
    linear_wring.set_key_linear();
    let mut empty_wring=Wring::new();
    empty_wring.set_key(&[]);
    let mut aerate_wring=Wring::new();
    aerate_wring.set_key("aerate".as_bytes());
    let mut χαίρετε_wring=Wring::new();
    χαίρετε_wring.set_key("Πάντοτε χαίρετε!".as_bytes());
    let mut двор_wring=Wring::new();
    двор_wring.set_key("Водворетраванатраведрова.Нерубидрованатраведвора!".as_bytes());
    test_vector(&linear_wring,&zero8,&[0x04,0xd7,0x16,0x6a,0xca,0x70,0x57,0xbc]);
    test_vector(&linear_wring,&ff8,&[0x84,0x91,0x95,0xa0,0x9f,0x48,0x4b,0x59]);
    test_vector(&linear_wring,&twistree8,&[0xed,0x4b,0x2e,0xc6,0xc4,0x39,0x65,0x2b]);
    test_vector(&linear_wring,&zero9,&[0xad,0x93,0x5e,0x85,0xe1,0x49,0x45,0xca,0xe2]);
    test_vector(&linear_wring,&ff9,&[0x36,0xdf,0x60,0xae,0xf5,0xbd,0x1a,0xaf,0x6e]);
    test_vector(&linear_wring,&allornone9,&[0x53,0x28,0xe7,0xc7,0xe0,0x71,0xa5,0x2e,0x8c]);
    test_vector(&empty_wring,&zero8,&[0x77,0x3e,0x34,0x8f,0x48,0xa1,0x24,0x1a]);
    test_vector(&empty_wring,&ff8,&[0xc7,0xa7,0x58,0xed,0x5c,0x2b,0xb6,0xec]);
    test_vector(&empty_wring,&twistree8,&[0xa3,0xcf,0xd4,0xa1,0x0d,0x7e,0xb7,0xb3]);
    test_vector(&empty_wring,&zero9,&[0x10,0x10,0x95,0x96,0x90,0xb5,0x97,0xeb,0x38]);
    test_vector(&empty_wring,&ff9,&[0x09,0x0f,0xf3,0x66,0x36,0xa4,0xac,0x8d,0x5c]);
    test_vector(&empty_wring,&allornone9,&[0xee,0x15,0x02,0x05,0xdd,0xa9,0x77,0xe4,0x23]);
    test_vector(&aerate_wring,&zero8,&[0x23,0x44,0x2e,0x6e,0xf3,0xd7,0xa0,0x7e]);
    test_vector(&aerate_wring,&ff8,&[0x7e,0x05,0xae,0x5c,0x64,0xdd,0xf4,0xeb]);
    test_vector(&aerate_wring,&twistree8,&[0x36,0x39,0x14,0x22,0x40,0x7f,0xc3,0x79]);
    test_vector(&aerate_wring,&zero9,&[0x41,0xc1,0x44,0x0f,0x07,0x2d,0x92,0xbf,0x43]);
    test_vector(&aerate_wring,&ff9,&[0x46,0xb0,0x57,0x43,0xfb,0xdb,0x9d,0x32,0x88]);
    test_vector(&aerate_wring,&allornone9,&[0x5e,0x3b,0x49,0xd4,0xb8,0x70,0xdd,0x07,0xac]);
    test_vector(&χαίρετε_wring,&zero8,&[0x90,0x4d,0x00,0x3e,0x39,0x75,0x9e,0xe4]);
    test_vector(&χαίρετε_wring,&ff8,&[0x85,0x56,0x3d,0x4e,0x84,0x5a,0x14,0xe3]);
    test_vector(&χαίρετε_wring,&twistree8,&[0x96,0x89,0x48,0x50,0x98,0x26,0xeb,0x03]);
    test_vector(&χαίρετε_wring,&zero9,&[0x04,0xb0,0x1e,0xdf,0xd3,0xf0,0x39,0xa3,0x3c]);
    test_vector(&χαίρετε_wring,&ff9,&[0x7c,0x67,0xeb,0xc8,0x40,0x97,0xc2,0x5f,0x82]);
    test_vector(&χαίρετε_wring,&allornone9,&[0x4b,0xb2,0x68,0xe9,0xf1,0x64,0x0a,0x44,0xc4]);
    test_vector(&двор_wring,&zero8,&[0x9c,0xc1,0x3c,0xe7,0x8a,0xc5,0x6f,0x18]);
    test_vector(&двор_wring,&ff8,&[0x37,0x42,0x00,0x47,0xd5,0x2f,0x9d,0x7f]);
    test_vector(&двор_wring,&twistree8,&[0x15,0x06,0xc6,0xa6,0x3d,0xef,0x19,0xf1]);
    test_vector(&двор_wring,&zero9,&[0xd6,0x7b,0x2c,0xcc,0x71,0x24,0x5d,0x06,0x07]);
    test_vector(&двор_wring,&ff9,&[0xd3,0xa8,0x3f,0xb6,0x9a,0x5e,0x0f,0x07,0x11]);
    test_vector(&двор_wring,&allornone9,&[0x53,0x6c,0x7e,0x2d,0xcd,0xda,0xdc,0xf2,0x70]);
  }

}

}

pub mod twistree {

use crate::sboxes::*; // uses keyschedule and permute
use crate::blockize::*;
use crate::compress::*; // uses mix3 and rot_bitcount


#[derive(Clone)]
pub struct Twistree {
  pub sbox: [[u8; 256]; 3],
  tree2: Vec<Vec<u8>>,
  tree3: Vec<Vec<u8>>,
  partial_block: Vec<u8>,
}

impl Twistree {
  pub fn new() -> Twistree {
    Twistree { sbox: [[0u8; 256]; 3],
	       tree2: Vec::new(),
	       tree3: Vec::new(),
	       partial_block: Vec::new() }
  }

  pub fn set_key_linear(&mut self) {
    for i in 0..3 {
      for j in 0..256 {
	self.sbox[i][j]=(j as u8).rotate_left((3*i+1) as u32);
      }
    }
  }

  pub fn set_key(&mut self,str:&[u8]) {
    sboxes(str,&mut self.sbox);
  }

  pub fn initialize(&mut self) {
    // Check that the Twistree has been keyed
    let mut sum:u32=0;
    for i in 0..3 {
      for j in 0..256 {
	sum+=self.sbox[i][j] as u32;
      }
    }
    if sum!=3*255*128 {
      panic!("call set_key before initialize");
    }
    // Check that the Twistree is empty
    if self.tree2.len()>0 || self.tree3.len()>0 || self.partial_block.len()>0 {
      panic!("call finalize before calling initialize again");
    }
    self.tree2.push(Vec::new());
    self.tree3.push(Vec::new());
    self.tree2[0].extend_from_slice(&EXP4_2ADIC[..]);
    self.tree3[0].extend_from_slice(&EXP4_BASE2[..]);
  }

  fn compress_pairs_triples(&mut self) {
    let mut i:usize=0;
    while self.tree2[i].len()>BLOCKSIZE {
      if i+1==self.tree2.len() {
	self.tree2.push(Vec::new());
      }
      compress(&self.sbox,&mut self.tree2[i],0);
      let (extend_from, to_extend) = self.tree2[i..(i+2)].split_at_mut(1);
      to_extend[0].extend_from_slice(&*extend_from[0]);
      self.tree2[i].clear();
      i=i+1;
    }
    i=0;
    while self.tree3[i].len()>2*BLOCKSIZE {
      if i+1==self.tree3.len() {
	self.tree3.push(Vec::new());
      }
      compress(&self.sbox,&mut self.tree3[i],1);
      let (extend_from, to_extend) = self.tree3[i..(i+2)].split_at_mut(1);
      to_extend[0].extend_from_slice(&*extend_from[0]);
      self.tree3[i].clear();
      i=i+1;
    }
  }

  fn finalize_pairs_triples(&mut self) {
    for i in 0..self.tree2.len() {
      compress(&self.sbox,&mut self.tree2[i],0);
      if i+1<self.tree2.len() {
	let (extend_from, to_extend) = self.tree2[i..(i+2)].split_at_mut(1);
	to_extend[0].extend_from_slice(&*extend_from[0]);
	self.tree2[i].clear();
      }
    }
    for i in 0..self.tree3.len() {
      compress(&self.sbox,&mut self.tree3[i],1);
      if i+1<self.tree3.len() {
	let (extend_from, to_extend) = self.tree3[i..(i+2)].split_at_mut(1);
	to_extend[0].extend_from_slice(&*extend_from[0]);
	self.tree3[i].clear();
      }
    }
  }

  pub fn update(&mut self,data:&[u8]) {
    // Check that the Twistree has been initialized
    if self.tree2.len()==0 || self.tree3.len()==0 {
      panic!("call initialize before update");
    }
    let blocks=blockize(data,&mut self.partial_block);
    for i in 0..blocks.len() {
      self.tree2[0].extend(&blocks[i]);
      self.tree3[0].extend(&blocks[i]);
      self.compress_pairs_triples();
    }
  }

  pub fn finalize(&mut self) -> Vec<u8> {
    // Check that the Twistree has been initialized
    if self.tree2.len()==0 || self.tree3.len()==0 {
      panic!("call initialize before update");
    }
    let last_block=pad(&mut self.partial_block);
    self.tree2[0].extend(&last_block);
    self.tree3[0].extend(&last_block);
    self.compress_pairs_triples();
    self.finalize_pairs_triples();
    let mut fruit=self.tree2.swap_remove(self.tree2.len()-1);
    fruit.extend(self.tree3.swap_remove(self.tree3.len()-1));
    self.tree2.clear();
    self.tree3.clear();
    compress(&self.sbox,&mut fruit,2);
    fruit
  }
}

}

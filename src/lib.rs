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
    test_vector(&linear_wring,&zero8,&[0x04,0xd7,0x16,0x6a,0xca,0x70,0x57,0xbc]);
    test_vector(&linear_wring,&ff8,&[0x84,0x91,0x95,0xa0,0x9f,0x48,0x4b,0x59]);
    test_vector(&linear_wring,&twistree8,&[0xed,0x4b,0x2e,0xc6,0xc4,0x39,0x65,0x2b]);
    test_vector(&linear_wring,&zero9,&[0xad,0x93,0x5e,0x85,0xe1,0x49,0x45,0xca,0xe2]);
    test_vector(&linear_wring,&ff9,&[0x36,0xdf,0x60,0xae,0xf5,0xbd,0x1a,0xaf,0x6e]);
    test_vector(&linear_wring,&allornone9,&[0x53,0x28,0xe7,0xc7,0xe0,0x71,0xa5,0x2e,0x8c]);
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

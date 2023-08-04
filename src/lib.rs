// Used by wring
pub mod mix3;

// Used by both wring and twistree
pub mod rotbitcount;

pub mod wring {

use crate::mix3::*;
use crate::rotbitcount::*;

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
  sbox: [[u8; 256]; 3],
  inv_sbox: [[u8; 256]; 3],
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
}

}

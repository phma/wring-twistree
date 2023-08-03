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

}

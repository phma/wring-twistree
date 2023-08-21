// Module sboxes
// This module is used in both Wring and Twistree.
// It converts a slice of bytes of arbitrary length, which should not exceed 96,
// into three s-boxes.

use crate::keyschedule::*;
use crate::permute::*;

pub fn sboxes(str:&[u8],sbox: &mut [[u8; 256]; 3]) {
  let mut subkey:[u16; 96]=[0u16; 96];
  key_schedule(str,&mut subkey);
  permute256(&mut sbox[0],&mut subkey);
  reschedule(&mut subkey);
  permute256(&mut sbox[1],&mut subkey);
  reschedule(&mut subkey);
  permute256(&mut sbox[2],&mut subkey);
}

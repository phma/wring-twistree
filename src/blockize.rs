// Module blockize
// This module is used in Twistree.
// It splits a slice of bytes into 32-byte blocks, using part to hold the
// partial block, and pads it with 0,1,2,... at the end.

use crate::compress::*;

// e⁴, in two binary representations, is prepended to the
// blocks being hashed, so that if the message is only one block,
// two different compressed blocks are combined at the end.

pub const EXP4_2ADIC: [u8;32]=
  [ 0x4d, 0x41, 0x8e, 0x38, 0x72, 0x1a, 0x3a, 0xeb
  , 0x18, 0xe0, 0x08, 0x7f, 0xa3, 0x7f, 0x9c, 0xe0
  , 0x17, 0xb6, 0x45, 0xee, 0xa5, 0x3c, 0x95, 0x34
  , 0xca, 0x6d, 0x5c, 0xfe, 0x7f, 0x94, 0x14, 0x09
  ];

pub const EXP4_BASE2: [u8;32]=
  [ 0xe8, 0xa7, 0x66, 0xce, 0x5b, 0x2e, 0x8a, 0x39
  , 0x4b, 0xb7, 0x89, 0x2e, 0x0c, 0xd5, 0x94, 0x05
  , 0xda, 0x72, 0x7b, 0x72, 0xfb, 0x77, 0xda, 0x1a
  , 0xcf, 0xb0, 0x74, 0x4e, 0x5c, 0x20, 0x99, 0x36
  ];

//                  01 =    1/1   = 01
//                  04 =    4/1   = 04
//                  08 =   16/2   = 08
// ...5555555555555560 =   64/6   = 0a.aaaaaaaaaaaaaa...
// ...5555555555555560 =  256/24  = 0a.aaaaaaaaaaaaaa...
// ...7777777777777780 = 1024/120 = 08.88888888888888...
// ...a4fa4fa4fa4fa500 = 4096/720 = 05.b05b05b05b05b0...
//    ----------------      ...     -----------------
// ...eb3a1a72388e414d =  exp(4)  = 36.99205c4e74b0cf...

pub fn blockize(bs:&[u8], part:&mut Vec<u8>) -> Vec<Vec<u8>> {
  let mut ret:Vec<Vec<u8>> = Vec::new();
  let mut i:usize=0;
  if part.len()+bs.len()>=BLOCKSIZE {
    i=BLOCKSIZE-part.len();
    part.extend_from_slice(&bs[0..i]);
    ret.push(part.clone());
    part.clear();
  }
  while bs.len()-i>=BLOCKSIZE {
    part.extend_from_slice(&bs[i..i+BLOCKSIZE]);
    ret.push(part.clone());
    part.clear();
    i+=BLOCKSIZE;
  }
  part.extend_from_slice(&bs[i..bs.len()]);
  ret
}

pub fn pad(part:&mut Vec<u8>) -> Vec<u8> {
  let mut ret:Vec<u8> = Vec::new();
  for i in 0..BLOCKSIZE-part.len() {
    part.push(i as u8);
  }
  ret=part.clone();
  part.clear();
  ret
}

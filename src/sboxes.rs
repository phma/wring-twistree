// Module sboxes
// This module is used in both Wring and Twistree.
// It converts a slice of bytes of arbitrary length, which should not exceed 96,
// into three s-boxes.

use crate::keyschedule::*;
use crate::permute::*;

pub const NULLKEY: [[u8; 256]; 3]=
  [ // These are the s-boxes resulting from keying with the null string.
    [
      0x59,0xdb,0x8a,0x81,0x4e,0x94,0xca,0xe8,0x43,0x3a,0x11,0x5f,0x26,0xc1,0xdc,0xbc,
      0x1f,0xd6,0xf8,0x23,0x42,0xb0,0x7f,0x55,0x48,0x45,0x5b,0x60,0x31,0x56,0xc3,0xf5,
      0xba,0x53,0x1b,0x1a,0x7a,0x07,0x76,0x5d,0x24,0x2f,0x68,0x4a,0xa3,0x32,0x78,0x87,
      0x9b,0xa5,0x21,0x52,0x1e,0x0c,0x6b,0x4b,0x69,0x67,0x0d,0x57,0x9c,0x8f,0x22,0x46,
      0x15,0x12,0xb8,0x47,0xe3,0xd8,0x0e,0xae,0x66,0x01,0x79,0x8e,0x75,0xd3,0x35,0xb5,
      0x44,0xd0,0x7e,0xbd,0xb1,0xa9,0xfb,0x51,0xd5,0x84,0xcd,0xa2,0x96,0x83,0xc0,0xc4,
      0x0f,0xcc,0xe4,0x71,0xad,0x2b,0x00,0xe2,0xa7,0x54,0x28,0x2c,0x9d,0x02,0x06,0xaa,
      0x50,0x90,0x72,0x1d,0x8c,0x3c,0xd2,0x74,0x1c,0x0a,0xe9,0xf4,0x2e,0xde,0xb7,0x39,
      0x92,0x61,0xdf,0x34,0x0b,0x85,0xea,0xa4,0x8b,0x14,0xf1,0x82,0x05,0x7b,0x97,0x16,
      0x58,0x4d,0x18,0xe5,0x19,0x20,0x62,0x2d,0xfd,0x3b,0x89,0xab,0xeb,0x3f,0x6d,0x03,
      0x6e,0xb3,0xa1,0xee,0xbe,0x77,0x93,0x95,0x4f,0xc9,0xfa,0x9e,0x6f,0xcf,0x10,0x04,
      0x33,0xc6,0x4c,0xd4,0x5c,0x29,0xa0,0xd1,0xed,0x36,0xcb,0x3e,0xbf,0xff,0x13,0x9f,
      0xef,0xf6,0x3d,0xb4,0xac,0xe1,0x5a,0x80,0xaf,0x6a,0x6c,0x9a,0xda,0x7c,0xf2,0x30,
      0x17,0x86,0x64,0xbb,0xc5,0xf0,0xdd,0x41,0xf3,0xec,0x99,0x40,0x70,0xb9,0xa8,0x91,
      0xe0,0xf9,0xd9,0xe6,0x5e,0x98,0x09,0x08,0xb2,0xfe,0x65,0x38,0xe7,0xce,0x2a,0xb6,
      0x73,0xf7,0xc2,0xc8,0x8d,0xc7,0x88,0x37,0xa6,0x25,0xd7,0x49,0x7d,0x63,0xfc,0x27
    ],[
      0xe9,0x13,0x67,0x94,0x64,0x14,0xb5,0x25,0x66,0x6d,0xb7,0xd1,0x1f,0x54,0x8e,0x95,
      0x2f,0xcc,0x4b,0xb9,0x81,0xc3,0x9d,0xa8,0xf3,0x3d,0x4c,0x06,0xa1,0x85,0xd2,0x0d,
      0xa7,0xd5,0x9c,0xaa,0x3e,0x24,0x82,0x91,0x73,0x75,0x26,0xe3,0x88,0x22,0x6a,0x56,
      0xd9,0x4f,0x1b,0x48,0xdf,0xd3,0xe8,0x87,0xf6,0x89,0xfe,0x17,0x33,0xb3,0x35,0xb8,
      0x8b,0xd7,0xf1,0x2b,0xbd,0xff,0x29,0x1a,0x4a,0xfa,0xc2,0xaf,0x01,0x18,0x36,0x70,
      0xd4,0xb0,0x8d,0x7c,0xe4,0x41,0xeb,0x40,0x3c,0x09,0xed,0x23,0x00,0x7e,0xf5,0x28,
      0xee,0x0a,0xf8,0x84,0xf0,0x2d,0x6e,0x2e,0xfc,0xef,0x7b,0x99,0x69,0x04,0xda,0x7d,
      0xe6,0xd8,0x98,0xf2,0xf4,0x12,0x19,0xec,0x74,0xea,0xa2,0x11,0x52,0xa4,0xc9,0xdd,
      0x50,0x93,0x10,0x1c,0x44,0x9f,0x53,0xf7,0x61,0xab,0x39,0xf9,0xc4,0x9a,0x0f,0x8c,
      0x42,0x30,0x58,0xc1,0x51,0x4e,0x8a,0x47,0x6f,0x62,0xc0,0x60,0x27,0x8f,0x3a,0x83,
      0x31,0x4d,0x37,0x21,0xfb,0x5c,0xa5,0xd0,0xe2,0x78,0xcd,0xa0,0xc5,0x3f,0x77,0x0b,
      0x1e,0x2a,0x6c,0x5e,0x5a,0xce,0x46,0xdb,0x79,0x9e,0xa9,0xbf,0x92,0x68,0x7a,0xbc,
      0x05,0x7f,0xe1,0xfd,0x2c,0xc7,0x90,0x34,0x32,0x72,0x07,0x49,0x5b,0xad,0xc6,0x02,
      0x0e,0xe5,0xe0,0x57,0x16,0x80,0xdc,0xa6,0x97,0xae,0x71,0xcb,0x43,0xba,0x45,0x6b,
      0xb1,0xa3,0x15,0xe7,0x86,0x96,0x08,0x65,0xde,0x1d,0xcf,0x38,0x9b,0x5f,0xb4,0x3b,
      0xca,0xbb,0xd6,0x03,0xc8,0xac,0x63,0x5d,0x0c,0x20,0xb6,0x59,0xb2,0xbe,0x76,0x55
    ],[
      0xe7,0x00,0x28,0xeb,0x80,0x69,0x61,0x22,0xff,0x5d,0x68,0xb0,0x67,0x8d,0x7b,0x07,
      0x05,0x32,0x9c,0x8a,0xfc,0x75,0xd3,0xc2,0x3b,0x02,0xbe,0x56,0x8e,0x4f,0x9f,0x5a,
      0x30,0x7d,0x3f,0x31,0xe4,0xb8,0x42,0x15,0x87,0x9b,0x91,0xa5,0x70,0xad,0xa1,0xf0,
      0x1a,0xa7,0xca,0x95,0x39,0xf1,0x0d,0x17,0xc8,0xa4,0x8c,0x81,0x18,0x1d,0xbd,0x4b,
      0x8b,0xb3,0x71,0xe5,0x50,0xfa,0x04,0x77,0x34,0xbc,0xe8,0xec,0x9a,0x85,0xc5,0xba,
      0xc6,0xfb,0xd9,0x48,0x20,0x79,0x11,0x1b,0x55,0x4e,0x0a,0x1f,0xc1,0xde,0x03,0x66,
      0xea,0xaf,0x3e,0x72,0xce,0x62,0x5b,0xd0,0xf2,0xf6,0xb7,0x1c,0x1e,0x08,0x12,0x82,
      0x94,0xaa,0xe1,0xfe,0x2f,0x64,0x0e,0xc3,0x0f,0x0c,0xae,0xab,0x92,0x3a,0xb4,0xcc,
      0xc0,0x0b,0xb2,0xcd,0xf5,0x6e,0x51,0x4d,0xb6,0x57,0xe3,0x90,0x14,0x9d,0x29,0x60,
      0xd5,0x74,0x8f,0x83,0x6f,0x5f,0xf4,0x5e,0x23,0xee,0x33,0x2b,0xe9,0x98,0x3c,0x21,
      0xef,0xd2,0x37,0xf8,0x43,0xc9,0x65,0x7f,0x10,0x4c,0x93,0xdf,0xd8,0xb5,0xb9,0xa3,
      0xe0,0x47,0x84,0xcf,0x6b,0x41,0xe2,0x9e,0x16,0x97,0x01,0xf7,0x7c,0x2d,0x88,0xf9,
      0x96,0x63,0x45,0x5c,0x27,0xa2,0xc4,0x78,0x73,0xc7,0xdc,0x40,0xcb,0x26,0x24,0x19,
      0x06,0x3d,0x46,0x7a,0xe6,0xdb,0x35,0xa0,0x58,0x59,0xd6,0xda,0xbf,0x52,0xd1,0xac,
      0x44,0x13,0x09,0x4a,0xb1,0x49,0xa9,0x2a,0xa6,0x2e,0x53,0x6a,0x36,0xa8,0x76,0xed,
      0x2c,0x89,0xf3,0x25,0xd7,0xd4,0x6c,0x54,0xbb,0xdd,0x6d,0x99,0x86,0x7e,0x38,0xfd
    ]
  ];

pub fn sboxes(str:&[u8],sbox: &mut [[u8; 256]; 3]) {
  let mut subkey:[u16; 96]=[0u16; 96];
  key_schedule(str,&mut subkey);
  permute256(&mut sbox[0],&mut subkey);
  reschedule(&mut subkey);
  permute256(&mut sbox[1],&mut subkey);
  reschedule(&mut subkey);
  permute256(&mut sbox[2],&mut subkey);
}

// Module rotbitcount
// This module is used in both Wring and Twistree.
// It rotates an array of bytes by a multiple of its bitcount,
// producing another array of the same size. As long as the multiplier
// is relatively prime to the number of bits in the array, this
// operation satisfies the strict avalanche criterion. Changing *two*
// bits, however, has half a chance of changing only two bits in
// the output.
//
// Bit 0 of byte 0 is bit 0 of the array. Bit 0 of byte 1 is bit 8 of the array.
// e1 00 00 00 00 00 00 00, rotated by its bitcount (4), becomes
// 10 0e 00 00 00 00 00 00.

pub fn rot_bitcount(src:&[u8], dst:&mut[u8], mult:isize) {
  assert_eq!(src.len(),dst.len());
  let bitcount = src.iter().fold(0, |acc,x| acc+x.count_ones()) as usize;
  let rotcount:usize;
  if (src.len()>0) {
    let multmod = mult.rem_euclid((src.len() as isize)*8) as usize;
    rotcount = (bitcount*multmod)%(src.len()*8);
  } else {
    let multmod = mult as usize;
    rotcount = bitcount*multmod;
  }
  let byte = rotcount>>3;
  let bit = rotcount&7;
  // The number of bit patterns where bit==0 tends to 1/8 of them:
  // 1/128, 1609/8192, 45967/524288, ...
  // This if-statement is vulnerable to a timing attack that can leak 0.192645
  // bit for each round where bit>0 and 3 bits for each round where bit==0,
  // for an average of 0.543564 bit per round. The leaked info is probably
  // not of much use to Eve, and for large messages is tiny compared to the
  // size of the message.
  //
  // This leak may be present in the Haskell code, depending on the processor
  // and the LLVM or C-- compiler.
  if bit>0 {
    for i in 0..dst.len() {
      dst[i]=(src[(i+src.len()-byte)  %src.len()]<<bit) |
	     (src[(i+src.len()-byte-1)%src.len()]>>(8-bit));
    }
  } else {
    for i in 0..dst.len() {
      dst[i]=(src[(i+src.len()-byte)  %src.len()]);
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn cycle_rot_bitcount(buf:&[u8])->u32 {
    let mut history:Vec<Vec<u8>> = Vec::new();
    let mut cycle:u32=0;
    history.push(buf.to_vec());
    while cycle==0 {
      for i in 0..history.len()-1 {
	if history[i]==history[history.len()-1] {
	  cycle=(history.len()-1+(i<<16)) as u32;
	}
      }
      history.push(buf.to_vec());
      let sz=history.len();
      let (last,rest)=history.split_last_mut().unwrap();
      rot_bitcount(&rest[sz-2],last,1);
    }
  cycle
  }

  #[test]
  // These bitcounts, being in the tails of the binomial distribution,
  // are unlikely to come up at random, but must be tested to ensure
  // proper functioning of the rot_bitcount function.
  fn test_tails() {
    assert_eq!(cycle_rot_bitcount(&[0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0]),1);
    assert_eq!(cycle_rot_bitcount(&[0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x1]),64);
    assert_eq!(cycle_rot_bitcount(&[0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80]),8);
    assert_eq!(cycle_rot_bitcount(&[0xfe,0xfd,0xfb,0xf7,0xef,0xdf,0xbf,0x7f]),8);
    assert_eq!(cycle_rot_bitcount(&[0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xfe]),64);
    assert_eq!(cycle_rot_bitcount(&[0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff]),1);
  }
}

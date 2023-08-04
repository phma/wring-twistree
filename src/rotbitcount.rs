// Module rotbitcount
// This module is used in both Wring and Twistree.
// It rotates an array of bytes by a multiple of its bitcount,
// producing another array of the same size. As long as the multiplier
// is relatively prime to the number of bits in the array, this
// operation satisfies the strict avalanche criterion. Changing *two*
// bits, however, has half a chance of changing only two bits in
// the output.

pub fn rot_bitcount(src:&[u8], dst:&mut[u8], mult:isize) {
  assert_eq!(src.len(),dst.len());
  let multmod = mult.rem_euclid((src.len() as isize)*8) as usize;
  let bitcount = src.iter().fold(0, |acc,x| acc+x.count_ones()) as usize;
  let rotcount = (bitcount*multmod)%(src.len()*8);
  let byte = rotcount>>3;
  let bit = rotcount&7;
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

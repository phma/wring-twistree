// Module permute
// This module is used in both Wring and Twistree.
// It permutes an array of 256 bytes by an array of 96 15-bit numbers,
// resulting in an s-box.

fn permut8(ys: &mut [u8], n: u16) {
  let mut swap_order:[u16; 7]=[0;7];
  swap_order[0]=n&1;
  swap_order[2]=(n>>1)&3;
  swap_order[6]=(n>>3)&7;
  let mut temp=((n>>6)&15)+1;
  swap_order[1]=temp%3;
  swap_order[4]=temp/3;
  temp=((n>>10)&31)+1;
  if temp>16 {temp+=1;}
  swap_order[3]=temp%5;
  swap_order[5]=temp/5;
  for i in 0..7 {
    ys.swap(i+1,swap_order[i] as usize);
  }
}

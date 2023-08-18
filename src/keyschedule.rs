// Module keyschedule
// This module is used in both Wring and Twistree.
// It converts a slice of bytes of arbitrary length, which should not exceed 96,
// into an array of 96 u16. Then it reschedules the array for the next s-box.

pub fn extend_key(str:&[u8]) -> Vec<u16> {
  let mut ret:Vec<u16> = Vec::new();
  let n:u16 = if str.len()>0 {(-(-384/str.len() as isize)) as u16} else {0};
  for i in 0..n {
    for j in 0..str.len() {
      ret.push(256*i+(str[j] as u16));
    }
  }
  ret
}

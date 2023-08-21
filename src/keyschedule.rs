// Module keyschedule
// This module is used in both Wring and Twistree.
// It converts a slice of bytes of arbitrary length, which should not exceed 96,
// into an array of 96 u16. Then it reschedules the array for the next s-box.

pub fn extend_key(str:&[u8]) -> Vec<u16> {
  let mut ret:Vec<u16> = Vec::new();
  let mut n:u16 = if str.len()>0 {(384/str.len() as isize) as u16} else {0};
  if (n as usize)*str.len()<384 {
    n+=1;
  }
  for i in 0..n {
    for j in 0..str.len() {
      ret.push(256*i+(str[j] as u16));
    }
  }
  ret
}

fn mul65537(a:u16,b:u16) -> u16 {
  ((((a as u32)+1)*((b as u32)+1))%65537-1) as u16
}

fn alter(subkey:&mut [u16],key_word: u16,inx: usize) {
  assert_eq!(subkey.len(),96);
  subkey[inx]=mul65537(subkey[inx],key_word);
  subkey[inx]+=subkey[(inx+59)%96]^subkey[(inx+36)%96]^subkey[(inx+62)%96];
  subkey[inx]=subkey[inx].rotate_left(8);
}

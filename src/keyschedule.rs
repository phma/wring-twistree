// Module keyschedule
// This module is used in both Wring and Twistree.
// It converts a slice of bytes of arbitrary length, which should not exceed 96,
// into an array of 96 u16. Then it reschedules the array for the next s-box.

fn extend_key(str:&[u8]) -> Vec<u16> {
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
  ((((a as u64)+1)*((b as u64)+1))%65537-1) as u16
}

fn alter(subkey:&mut [u16],key_word: u16,inx: usize) {
  assert_eq!(subkey.len(),96);
  subkey[inx]=mul65537(subkey[inx],key_word);
  subkey[inx]+=subkey[(inx+59)%96]^subkey[(inx+36)%96]^subkey[(inx+62)%96];
  subkey[inx]=subkey[inx].rotate_left(8);
}

pub fn key_schedule(str:&[u8],subkey:&mut [u16]) {
  subkey[0]=1;
  for i in 1..96 {
    // This sequence was used as the PRNG in an Apple implementation of Forth.
    // Its cycle length is 64697.
    subkey[i]=(subkey[i-1]*13).rotate_left(8);
  }
  let xkey=extend_key(str);
  for i in 0..xkey.len() {
    alter(subkey,xkey[i],i%96);
  }
}

pub fn reschedule(subkey:&mut [u16]) {
  for i in 0..96 {
    alter(subkey,40504,i%96); // 40505 is a primitive root near 65537/φ
  }
}

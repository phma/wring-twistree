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

#[test]
fn test_permut8() {
  let mut word:[u8; 8]=*b"calipers";
  permut8(&mut word[..],0x3c45);
  assert_eq!(&word,b"spiracle");
  word=*b"recounts";
  permut8(&mut word[..],0x595b);
  assert_eq!(&word,b"construe");
  word=*b"thousand";
  permut8(&mut word[..],0x30da);
  assert_eq!(&word,b"handouts");
  for i in 0..0x8000 {
    word=*b"repaints";
    permut8(&mut word[..],i);
    assert_ne!(&word,b"pantries");
  }
}

fn permut8x32(sbox: &mut [u8], key: &[u16]) {
  assert_eq!(sbox.len(),8*key.len());
  for i in 0..key.len() {
    permut8(&mut sbox[8*i..8*i+8],key[i]);
  }
}

const SHIFT3: [u16;8]=[0x00,0x1d,0x3a,0x27,0x74,0x69,0x4e,0x53];

fn deal_inx(n:usize) -> usize {
  ((n<<3)&0xf8)^(SHIFT3[(n>>5)&7]) as usize
}

fn deal_bytes(sbox0: &[u8],sbox1: &mut [u8]) {
  assert_eq!(sbox0.len(),256);
  assert_eq!(sbox1.len(),256);
  for i in 0..256 {
    sbox1[deal_inx(i)]=sbox0[i];
  }
}

pub fn permute256(sbox: &mut [u8],key: &[u16]) {
  assert_eq!(sbox.len(),256);
  assert_eq!(key.len(),96);
  for i in 0..256 { sbox[i]=i as u8; }
  let mut sbox2:[u8;256]=[0;256];
  let mut sbox3:[u8;256]=[0;256];
  permut8x32(sbox,&key[0..32]);
  deal_bytes(&sbox,&mut sbox2);
  permut8x32(&mut sbox2,&key[32..64]);
  deal_bytes(&sbox2,&mut sbox3);
  permut8x32(&mut sbox3,&key[64..96]);
  deal_bytes(&sbox3,sbox);
}

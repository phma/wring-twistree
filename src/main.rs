use wring_twistree::mix3::*;
use wring_twistree::rotbitcount::*;
use wring_twistree::wring::*;
use num_bigint::BigUint;

fn printvec(k:&[u8]) {
  for i in 0..k.len() {
    print!("{:02x} ",k[i]);
    if i%16==15 || i+1==k.len() {
      println!();
    }
  }
}

fn main() {
  let mut buf:Vec<u8> = Vec::new();
  let mut src=vec!(0u8; 16);
  let mut dst=vec!(0u8; 16);
  let mut wring=Wring::new();
  wring.set_key_linear();
  for i in 0..=15 {
    buf.push(i*13);
  }
  printvec(&buf);
  mix3parts(&mut buf,5,3);
  printvec(&buf);
  buf.clear();
  buf.extend([0; 256]);
  wring.encrypt(&mut buf);
  printvec(&buf);
  let fibos=fibo_pair(BigUint::from(144u32));
  println!("{},{}",fibos.0,fibos.1);
  println!("{}",carmichael(1016255020032000000));
  src[0]=255;
  rot_bitcount(&src,&mut dst,1);
  printvec(&dst);
}

use wring_twistree::mix3::*;
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
  for i in 0..=15 {
    buf.push(i*13);
  }
  printvec(&buf);
  mix3parts(&mut buf,5,3);
  printvec(&buf);
  let fibos=fibo_pair(BigUint::from(144u32));
  println!("{},{}",fibos.0,fibos.1);
  println!("{}",carmichael(1016255020032000000));
}

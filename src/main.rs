use wring_twistree::mix3::*;

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
  let a=0;
  let b=9;
  let c=10;
  let (d,rest)=buf[a..].split_first_mut().unwrap();
  let (e,rest)=rest[b-a-1..].split_first_mut().unwrap();
  let (f,_)=rest[c-b-1..].split_first_mut().unwrap();
  mix(d,e,f);
  printvec(&buf);
}

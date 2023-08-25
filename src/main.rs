use wring_twistree::mix3::*;
use wring_twistree::rotbitcount::*;
use wring_twistree::keyschedule::*;
use wring_twistree::wring::*;
use num_bigint::BigUint;
use clap::Parser;

#[derive(Parser)]
struct Cli {
  input: String,
  #[clap(short,long,group="action")]
  encrypt: bool,
  #[clap(short,long,group="action")]
  decrypt: bool,
  #[clap(short,long)]
  output: String,
}

fn printvec(k:&[u8]) {
  for i in 0..k.len() {
    print!("{:02x} ",k[i]);
    if i%16==15 || i+1==k.len() {
      println!();
    }
  }
}

fn printvec16(k:&[u16]) {
  for i in 0..k.len() {
    print!("{:04x} ",k[i]);
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
  //printvec(&buf);
  wring.decrypt(&mut buf);
  //printvec(&buf);
  src[0]=225;
  rot_bitcount(&src,&mut dst,1);
  printvec(&dst);
  let sched=extend_key("roygbiv".as_bytes());
  printvec16(&sched);
  let sched=extend_key("aerate".as_bytes());
  printvec16(&sched);
}

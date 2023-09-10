use wring_twistree::mix3::*;
use wring_twistree::rotbitcount::*;
use wring_twistree::keyschedule::*;
use wring_twistree::wring::*;
use wring_twistree::blockize::*;
use wring_twistree::compress::*;
use num_bigint::BigUint;
use clap::Parser;
use std::io;
use std::io::*;
use std::fs::File;

fn encrypt_file(key:&str, plainname:&str, ciphername:&str) -> io::Result<()> {
  let mut plainfile=File::open(plainname)?;
  let mut buffer:Vec<u8> = Vec::new();
  plainfile.read_to_end(&mut buffer)?;
  plainfile.sync_all()?;
  let mut wring=Wring::new();
  wring.set_key(key.as_bytes());
  wring.encrypt(&mut buffer);
  let mut cipherfile=File::create(ciphername)?;
  cipherfile.write_all(&buffer)?;
  Ok(())
}

fn decrypt_file(key:&str, ciphername:&str, plainname:&str) -> io::Result<()> {
  let mut cipherfile=File::open(ciphername)?;
  let mut buffer:Vec<u8> = Vec::new();
  cipherfile.read_to_end(&mut buffer)?;
  cipherfile.sync_all()?;
  let mut wring=Wring::new();
  wring.set_key(key.as_bytes());
  wring.decrypt(&mut buffer);
  let mut plainfile=File::create(plainname)?;
  plainfile.write_all(&buffer)?;
  Ok(())
}

#[derive(Parser)]
struct Cli {
  input: Option<String>,
  #[clap(short,long,group="action")]
  encrypt: bool,
  #[clap(short,long,group="action")]
  decrypt: bool,
  #[clap(short,long,group="action")]
  test: bool,
  #[clap(short,long)]
  key: Option<String>,
  #[clap(short,long)]
  output: Option<String>,
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

fn test_compress() {
  let mut buf:Vec<u8> = Vec::new();
  let mut wring=Wring::new();
  wring.set_key_linear();
  buf.extend_from_slice(&EXP4_2ADIC);
  buf.extend_from_slice(&EXP4_BASE2);
  compress(&wring.sbox,&mut buf,0);
  printvec(&buf);
}

fn test_new_code() {
  let mut buf:Vec<u8> = Vec::new();
  let mut src=vec!(0u8; 16);
  let mut dst=vec!(0u8; 16);
  let mut sched=vec!(0u16; 96);
  let mut wring=Wring::new();
  wring.set_key_linear();
  for i in 0..=15 {
    buf.push(i*13);
  }
  //printvec(&buf);
  mix3parts(&mut buf,5,3);
  //printvec(&buf);
  buf.clear();
  buf.extend([0; 256]);
  wring.encrypt(&mut buf);
  //printvec(&buf);
  wring.decrypt(&mut buf);
  //printvec(&buf);
  src[0]=225;
  rot_bitcount(&src,&mut dst,1);
  //printvec(&dst);
  key_schedule("roygbiv".as_bytes(),&mut sched);
  //printvec16(&sched);
  key_schedule("aerate".as_bytes(),&mut sched);
  //printvec16(&sched);
  //encrypt_file("aerate","/tmp/1meg","/tmp/1meg.crypt");
  test_compress();
}

fn main() {
  let args=Cli::parse();
  let input=match args.input {
    Some(s) => s,
    None => "".to_string(),
  };
  let output=match args.output {
    Some(s) => s,
    None => "".to_string(),
  };
  let key=match args.key {
    Some(s) => s,
    None => "".to_string(),
  };
  if args.encrypt {
    if input.len()>0 {
      if output.len()>0 {
	encrypt_file(&key,&input,&output).expect("Can't encrypt file");
      } else {
	encrypt_file(&key,&input,&input).expect("Can't encrypt file");
      }
    } else {
      println!("Please specify file to encrypt");
    }
  }
  if args.decrypt {
    if input.len()>0 {
      if output.len()>0 {
	decrypt_file(&key,&input,&output).expect("Can't decrypt file");
      } else {
	decrypt_file(&key,&input,&input).expect("Can't decrypt file");
      }
    } else {
      println!("Please specify file to decrypt");
    }
  }
  if args.test {
    test_new_code();
  }
}

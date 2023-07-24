pub fn mix(a: &mut u8, b: &mut u8, c: &mut u8) {
  let mask:u8=(*a|*b|*c)-(*a&*b&*c);
  *a=*a^mask;
  *b=*b^mask;
  *c=*c^mask;
}

pub fn mix3parts(buf: &mut[u8], len: usize, rprime: usize) {
  let mut a=0;
  let mut b=2*len-1;
  let mut c=2*len;
  while a<b {
    // This amounts to mix(buf[a],buf[b],buf[c]).
    // It has to be written in this convoluted way
    // to keep the borrow checker happy.
    let (d,rest)=buf[a..].split_first_mut().unwrap();
    let (e,rest)=rest[b-a-1..].split_first_mut().unwrap();
    let (f,_)=rest[c-b-1..].split_first_mut().unwrap();
    mix(d,e,f);
    a=a+1;
    b=b-1;
    c=c+rprime;
    if c>=3*len {
      c=c-len;
    }
  }
}

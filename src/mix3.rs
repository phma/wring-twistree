pub fn mix(a: &mut u8, b: &mut u8, c: &mut u8) {
  let mask:u8=(*a|*b|*c)-(*a&*b&*c);
  *a=*a^mask;
  *b=*b^mask;
  *c=*c^mask;
}


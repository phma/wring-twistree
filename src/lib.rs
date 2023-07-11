pub fn mix(a: &mut u8, b: &mut u8, c: &mut u8) {
  let mask:u8=(*a|*b|*c)-(*a&*b&*c);
  *a=*a^mask;
  *b=*b^mask;
  *c=*c^mask;
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}

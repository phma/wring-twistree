pub const BLOCKSIZE: u16=32;
pub const TWISTPRIME: u16=37;
// BLOCKSIZE must be a multiple of 4. Blocks in the process of compression
// can be any size from BLOCKSIZE to 3*BLOCKSIZE in steps of 4. TWISTPRIME is
// the smallest prime greater than BLOCKSIZE, which is relatively prime to all
// block sizes during compression.

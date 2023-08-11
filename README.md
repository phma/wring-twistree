# WringTwistree
Wring is a whole-message cipher. This is like a block cipher, but the block can be arbitrarily large, up to about 3 GiB (the Rust version will start getting multiplication overflows about there).

Twistree is a hash function. It constructs two trees out of the blocks of data, runs the compression function at each node of the trees, and exclusive-ors the results.

Both algorithms are keyed. The key is turned into 96 16-bit words, which are then used to make three S-boxes. The key can be any byte string, including the empty string.

# Rust
To run the program, type `cargo run`. The first time you run it, Cargo may download a quarter-gigabyte of data, which is the index to all crates.

# Haskell
You can run Daphne in the REPL with `stack ghci` or use other Stack commands.

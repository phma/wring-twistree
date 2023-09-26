# WringTwistree
Wring is a whole-message cipher. This is like a block cipher, but the block can be arbitrarily large, up to about 3 GiB (the Rust version will start getting multiplication overflows about there). Wring can also be used as a length-preserving all-or-nothing transform.

Twistree is a hash function. It constructs two trees out of the blocks of data, runs the compression function at each node of the trees, and combines the results with the compression function.

Both algorithms are keyed. The key is turned into 96 16-bit words, which are then used to make three S-boxes. The key can be any byte string, including the empty string.

# Rust
To run the program, type `cargo run`. The first time you run it, Cargo may download a quarter-gigabyte of data, which is the index to all crates.

# Haskell
You can run WringTwistree in the REPL with `stack ghci`, compile and run it with `stack run`, or use other Stack commands.

# Command-line options
`stack run -- -k key -e plaintext -o ciphertext` enciphers a file. Omit `-o ciphertext` to encipher in place.

`stack run -- -k key -d ciphertext -o plaintext` deciphers a file. Omit `-o plaintext` to decipher in place.

`stack run -- -k key -H plaintext -o hash` hashes a file. Omit `-o hash` to output the hash to stdout.

`cargo` instead of `stack` runs the Rust implementation; the options are the same, except that `-H` isn't implemented yet.

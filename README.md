# WringTwistree
Wring is a whole-message cipher. This is like a block cipher, but the block can be arbitrarily large, up to about 3 GiB (the Rust version will start getting multiplication overflows about there). Wring can also be used as a length-preserving all-or-nothing transform.

Twistree is a hash function. It constructs two trees out of the blocks of data, runs the compression function at each node of the trees, and combines the results with the compression function.

Both algorithms are keyed. The key is turned into 96 16-bit words, which are then used to make three S-boxes. The key can be any byte string, including the empty string.

# Features
A round consists of four operations:

1. The `mix3Parts` operation splits the message or block in three equal parts and mixes them nonlinearly. This provides both diffusion and nonlinearity. The number of rounds in Wring increases logarithmically with message size so that diffusion spreads to the entire message.
2. The three key-dependent 8×8 S-boxes provide confusion and 
resist linear cryptanalysis.
3. Rotating by the population count thwarts integral and differential cryptanalysis by moving the difference around to a different part of the message.
4. Wring's round constant, which is dependent on the byte position as well as the round number, is added to every byte to prevent slide attacks and ensure that an all-zero message doesn't stay that way.
4. Twistree runs a CRC backwards to make the four bytes about to be dropped affect all the other bytes.

# Rust
To run the program, type `cargo run`. The first time you run it, Cargo may download a quarter-gigabyte of data, which is the index to all crates. You can also test the program with `cargo test`.

# Haskell
You can run WringTwistree in the REPL with `stack ghci`, compile and run it with `stack run`, or use other Stack commands. You can also test the program with `stack test`.

If you develop this using Cabal, please edit `package.yaml` to match any changes you make to `WringTwistree.cabal`.

# Command-line options
`stack run -- -k key -e plaintext -o ciphertext` enciphers a file. Omit `-o ciphertext` to encipher in place.

`stack run -- -k key -d ciphertext -o plaintext` deciphers a file. Omit `-o plaintext` to decipher in place.

`stack run -- -k key -H plaintext -o hash` hashes a file. Omit `-o hash` to output the hash to stdout.

`stack run -- -c foo` runs cryptanalysis of type `foo`, which is currently `relkey` for related-key cryptanalysis. This option is not available in the Rust implementation.

`cargo` instead of `stack` runs the Rust implementation; the options are the same, except that hash is `-h`.

The Rust binary is `wring-twistree` and can be installed with `cargo install --path .`; the Haskell binary is `WringTwistree` and can be installed with `stack install`. They take the options after `--`.

# Test vectors
Test vectors are in `test/Spec.hs` and `src/lib.rs`.

# Changelog for `WringTwistree`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/) for Haskell
and [Semantic Versioning](https://semver.org/) for Rust.

## Unreleased

### Added

- -O2 option
- Progress bar for cryptanalysis
- In Rust, initialize sbox of new Wring and Twistree

## 0.0.1.1 - 0.1.0 - 2024-01-11

### Removed

- -O2 and -fllvm options

## 0.0.1.0 - 0.1.0 - 2024-01-03

### Added

- Keying, common to Wring and Twistree
- Whole-message cipher Wring
- Keyed hash algorithm Twistree
- Test vectors for both algorithms
- Shell script to test that both implementations give identical results
- Related-key cryptanalysis of Wring
- Integral cryptanalysis of Wring
- Restricted-byte differential cryptanalysis of Twistree

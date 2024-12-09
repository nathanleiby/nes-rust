# nes-rust ("Muenster")

An emulator for the NES, written in Rust.

Priorities:

- WASM support from the start
- Try Bevy game engine for Frontend (audio, input, screen)
- Unit tested
- CI on commit: linting, unit tests, releases
- Try some Rust patterns for midsized projects...
  - break up the repo into modules?
  - run benchmarks?


## References

### Howto

- Book with Walkthrough: https://bugzmanov.github.io/nes_ebook
- Spec:
  - https://www.nesdev.org/obelisk-6502-guide/reference.html
  - http://www.6502.org/tutorials/6502opcodes.html
- NES Test Roms: https://github.com/christopherpow/nes-test-roms
- 6502 details
  - overflow and underflow: https://www.righto.com/2012/12/the-6502-overflow-flag-explained.html

### Other interesting Tools

- https://github.com/tedsteen/nes-bundler

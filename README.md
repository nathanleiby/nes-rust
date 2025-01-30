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
- Other Roms:
  - https://www.nesworld.com/article.php?system=nes&data=neshomebrew
  - https://www.nesfiles.com/Games
- 6502 details
  - overflow and underflow: https://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
- Making Games
  - and more.. https://github.com/denisenepraunig/nes-development
- How games should look:
  - Donkey Kong 1: https://www.youtube.com/watch?v=p54nqmtK50Y&ab_channel=IGN
  - Pacman: https://www.youtube.com/watch?v=DxFjFS-idYk&ab_channel=WorldofLongplays
- Nice blogs
  - overview: https://leeteng.com/blog/content/writing-nes-emulator
  - rendering: https://austinmorlan.com/posts/nes_rendering_overview/
  - mappers: https://nesdoug.com/2019/10/02/22-advanced-mapper-mmc1/
- other NES emus in rust:
  - https://github.com/DaveTCode/nes-emulator-rust
  - https://github.com/rib/nes-emulator
  - https://github.com/lukexor/tetanes
  - https://github.com/Comba92/nen-emulator - lots of good linked docs, too

### Other interesting Tools

- https://github.com/tedsteen/nes-bundler

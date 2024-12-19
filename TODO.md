- [ ] Bug: gamepad input not detected in Mario
- [ ] Add support for 2 gamepads
- [..] Add scrolling support
  - [x] horizontal scrolling (vertical mirroring)
  - [ ] vertical scrolling (horizontal mirroring) -- working on it via Galaga
- [ ] Implement mappers
  - [ ] iNES Mapper 1 = MMC1
    - SNROM - e.g. [Metroid](https://nescartdb.com/profile/view/224/metroid), [Final Fantasy](https://nescartdb.com/profile/view/154/final-fantasy)
  - [ ] iNES Mapper 2
    - UNROM - [Mega Man](https://nescartdb.com/profile/view/608/mega-man)
- [..] Add tests cases for PPU registers
- [ ] Separate the core from the specific hardware (screen, input)
  - [ ] Try replacing screen and input with macroquad
- [ ] Integrate code coverage into CI. (maybe add a repo tag in README)
  https://github.com/xd009642/tarpaulin
  https://medium.com/@gnanaganesh/robust-rust-how-code-coverage-powers-rust-software-quality-417ef3ac2360
  https://blog.balthazar-rouberol.com/measuring-the-coverage-of-a-rust-program-in-github-actions
- [ ] Migrate to bitfields (from bitflags)
  - https://docs.rs/bilge/latest/bilge/
  - https://gitlab.com/SmartAcoustics/sparrow/-/blob/master/sparrow-bitpacker/src/bitfields.rs
  - https://github.com/wrenger/bitfield-struct-rs
  - https://github.com/gregorygaines/bitfields-rs
- [ ] Debugger
  - allow requesting a to break at a given pc position (run until pc=X) or at a given OP (run until op=STA)
    - this would let me quickly inspect processor state to repair issues in nestest
    - this would help me not run too far when isolating an issue. in particular in my current PPU debugging
  - Try an external debugger and setting breakpts
  - Debugger view
    - show both pattern tables beside the UI
    - allow play/pause of CPU
    - show current instruction
    - when we get to sprite drawing,
      - log the details
      - highlight which pattern is being used
    - show state of CPU (same idea as "trace")
    - inspiration:
      - https://www.reddit.com/r/EmuDev/comments/1hh4gr6/my_nes_emulator_debugger/
- [ ] More NES Test roms
  - Try running more NES Test roms, maybe they can help now that i have some graphics?
  - Lots of the PPU test rom links here are broken.. https://www.nesdev.org/wiki/Emulator_tests
- [ ] Bug: crash that is trying to write to wrong memory
  - Repro-able by running Pacman a few times
  ```
    thread 'main' panicked at src/ppu.rs:362:26:
    attempt to write to CHR ROM: 0000 (read-only)
  ```
- [ ] refactor to split up functionality more
  - why? one of my goals is to learn better Rust patterns for project layout
  - why? I would find it easier to think about testable units
- [ ] Allow lookup of opcode from op name + addressing mode, esp for use in tests to give clarity and reduce typos of op codes
- [ ] Modify the test "Snake" game to work without changing the program start address
  - This should require updating any jmp instructions
- [ ] Extract "tileset viewer" subproject
  - CLI that you run and it pops up a view of the game's tilesets
- [ ] Extract subproject: 6502 emulator .. as REPL?
  - review emails/etc where I suggested other nice subprojects, too.

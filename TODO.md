- [ ] Figure out why sprites aren't drawing

--

- [ ] refactor to split up functionality more
  - why? one of my goals is to learn better Rust patterns for project layout
  - why? I would find it easier to think about testable units
- [ ] Debugger
  - [ ] allow requesting a to break at a given pc position (run until pc=X) or at a given OP (run until op=STA)
    - this would let me quickly inspect processor state to repair issues in nestest
    - this would help me not run too far when isolating an issue. in particular in my current PPU debugging
- [ ] allow lookup of opcode from op name + addressing mode, esp for use in tests to give clarity and reduce typos of op codes
- [ ] add tests cases for PPU registers
- [ ] Modify the test "Snake" game to work without changing the program start address
  - This should require updating any jmp instructions
- [ ] Extract "tileset viewer" subproject
  - CLI that you run and it pops up a view of the game's tilesets
- [ ] Extract subproject: 6502 emulator .. as REPL?
  - review emails/etc where I suggested other nice subprojects, too.
- [ ] Is there a way to get a nice trace that doesn't manipulate registers which cause side effects of READ operations?
  Right now, running a trace causes a program to fail :( :(
    I wrote a `tracelite` operation to explore this.
    I also modified `trace` to skip the address lookups, and this was usable enough for debugging (but broke some hard-coding in tests).

- Get the snake game running
```
pc=0x06cc (program idx=204) op=0x10
pc=0x07c7 (program idx=455) op=0x00
SHOULD BE: 06c7

- 00110001 (bits 45 are on in snake.. BRK ones)

Learn
- arithmetic vs logical shift

-------------

PRs upstream:

- [ ] Explain in the book that trace() needs to be deactivated (if it does) due to side-effects of READ ops

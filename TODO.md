- [ ] allow running in step-wise debugger mode.. BREAK at a given pc position
  - this would let me quickly inspect processor state to repair issues in nestest
- [x] add a function to set status flags more easily (bitflags or similar)
- [ ] allow lookup of opcode from op name + addressing mode, esp for use in tests to give clarity and reduce typos of op codes
- [ ] Modify the test "Snake" game to work without changing the program start address
  - This should require updating any jmp instructions
- Get the snake game running
```
pc=0x06cc (program idx=204) op=0x10
pc=0x07c7 (program idx=455) op=0x00
SHOULD BE: 06c7

- 00110001 (bits 45 are on in snake.. BRK ones)

Learn
- arithmetic vs logical shift


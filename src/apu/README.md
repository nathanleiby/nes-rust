# APU - Audio Processing Unit

Another great APU ref:
https://www.slack.net/~ant/nes-emu/apu_ref.txt

Read about relevant synthesis:
https://www.slack.net/~ant/bl-synth/

Deep divin
https://forums.nesdev.org/viewtopic.php?t=8602

Other people asking about APU:
- https://github.com/amhndu/SimpleNES/issues/24
- https://www.reddit.com/r/rust/comments/16kx8i2/how_to_implement_apu_for_my_nes_emulator/
- https://www.reddit.com/r/EmuDev/comments/16kx80n/how_to_implement_apu/
- https://github.com/OneLoneCoder/olcNES/blob/master/Part%20%237%20-%20Mappers%20%26%20Basic%20Sounds/olcNes_Sounds1.cpp

Rust libs:

- synthesis, has osc for common wavs.. will it work well to generate one for each register?
https://docs.rs/twang/latest/twang/
- SDL2 can play sounds
  - ex of squarewave: https://github.com/Rust-SDL2/rust-sdl2/blob/master/examples/audio-queue-squarewave.rs
  - can play multiple sounds with mixer:
    - https://rust-sdl2.github.io/rust-sdl2/sdl2/mixer/index.html
    - usage ex: https://github.com/Rust-SDL2/rust-sdl2/blob/master/examples/mixer-demo.rs
  - https://nicktasios.nl/posts/making-sounds-using-sdl-and-visualizing-them-on-a-simulated-oscilloscope.html

Combining wave forms:
https://0xc45.com/blog/digital-audio-synthesizer-in-rust/

Other synthesizer examples:
- https://www.reddit.com/r/rust/comments/mcbx48/fullyfeatured_fm_synthesizer_running_in_the/

Very basic synthesis:
- https://thewolfsound.com/sound-synthesis/wavetable-synth-in-rust/

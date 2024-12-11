use core::Cpu;
use core::Mem;
use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::thread::sleep;
use std::time::Duration;

mod bus;
mod core;
mod gamepad;
mod macros;
mod ops;
mod ppu;
mod rom;
mod utility;

use gamepad::GamepadButtons;
use ppu::Frame;
use rand::random;
use rom::Rom;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;
use sdl2::EventPump;

struct KeyboardInput {
    key_map: HashMap<Keycode, GamepadButtons>,
}

impl KeyboardInput {
    pub fn new() -> Self {
        let mut key_map = HashMap::new();
        key_map.insert(Keycode::Down, gamepad::GamepadButtons::Down);
        key_map.insert(Keycode::Up, gamepad::GamepadButtons::Up);
        key_map.insert(Keycode::Right, gamepad::GamepadButtons::Right);
        key_map.insert(Keycode::Left, gamepad::GamepadButtons::Left);
        key_map.insert(Keycode::Space, gamepad::GamepadButtons::Select);
        key_map.insert(Keycode::Return, gamepad::GamepadButtons::Start);
        key_map.insert(Keycode::A, gamepad::GamepadButtons::ButtonA);
        key_map.insert(Keycode::S, gamepad::GamepadButtons::ButtonB);

        Self { key_map }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;
    let window = video_subsystem
        .window("Tile Viewer", (256.0 * 3.0) as u32, (240.0 * 3.0) as u32)
        .position_centered()
        .build()?;

    let mut canvas = window.into_canvas().present_vsync().build()?;
    let mut event_pump = sdl_context.event_pump()?;
    canvas.set_scale(3., 3.)?;

    let creator = canvas.texture_creator();
    let mut texture = creator.create_texture_target(PixelFormatEnum::RGB24, 256, 240)?;

    // let mut screen_state = [0_u8; 32 * 3 * 32];

    // TODO: get program name from args at CLI
    let program = fs::read("roms/Alter_Ego.nes").unwrap();
    // let program = fs::read("roms/pacman.nes").unwrap();
    // let program = fs::read("roms/Ms_pacman.nes").unwrap();

    let mut cpu = Cpu::new();
    let rom = Rom::new(&program);

    let mut frame = Frame::new();

    let tiles_per_row = 24;
    let tile_size = 8 + 1;
    for bank in 0..=1 {
        for tile_n in 0..256 {
            let x = tile_n % tiles_per_row;
            let y = tile_n / tiles_per_row + bank * 12;

            frame.draw_tile(&rom.chr_rom, bank, tile_n, (x * tile_size, y * tile_size));
        }
    }

    // if read_screen_state(cpu, &mut screen_state.data) {
    //     // redraw the screen
    //     texture.update(None, &screen_state.d, 32 * 3).unwrap();
    //     canvas.copy(&texture, None, None).unwrap();
    //     canvas.present();
    // }
    texture.update(None, &frame.data, 256 * 3).unwrap();
    canvas.copy(&texture, None, None).unwrap();
    canvas.present();

    loop {
        handle_user_input(&mut cpu, &mut event_pump);
    }
    // cpu.load_rom(rom);
    // cpu.reset();
    // cpu.run_with_callback(move |cpu| {
    //     println!("{}", cpu.trace());

    //     // read user input and write it to mem[0xFF]
    //     handle_user_input(cpu, &mut event_pump);

    //     // update mem[0xFE] with a new random number
    //     cpu.mem_write(0xFE, random::<u8>());

    //     // read mem mapped screen state
    //     if read_screen_state(cpu, &mut screen_state) {
    //         // redraw the screen
    //         texture.update(None, &screen_state, 32 * 3).unwrap();
    //         canvas.copy(&texture, None, None).unwrap();
    //         canvas.present();
    //     }

    //     sleep(Duration::new(0, 70_000));
    // });

    Ok(())
}

fn handle_user_input(cpu: &mut Cpu, event_pump: &mut EventPump) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => std::process::exit(0),
            Event::KeyDown {
                keycode: Some(kc), ..
            } => match kc {
                Keycode::W => cpu.mem_write(0xFF, 0x77),
                Keycode::S => cpu.mem_write(0xFF, 0x73),
                Keycode::A => cpu.mem_write(0xFF, 0x61),
                Keycode::D => cpu.mem_write(0xFF, 0x64),
                _ => { /* ignore other keys for now */ }
            },
            _ => { /* do nothing */ }
        }
    }
}

fn color(byte: u8) -> Color {
    match byte {
        0 => sdl2::pixels::Color::BLACK,
        1 => sdl2::pixels::Color::WHITE,
        2 | 9 => sdl2::pixels::Color::GREY,
        3 | 10 => sdl2::pixels::Color::RED,
        4 | 11 => sdl2::pixels::Color::GREEN,
        5 | 12 => sdl2::pixels::Color::BLUE,
        6 | 13 => sdl2::pixels::Color::MAGENTA,
        7 | 14 => sdl2::pixels::Color::YELLOW,
        _ => sdl2::pixels::Color::CYAN,
    }
}

// fn read_screen_state(cpu: &mut Cpu, screen: &mut [u8; 32 * 3 * 32]) -> bool {
//     let mut screen_idx = 0;
//     let mut updated = false;
//     for i in 0x0200..0x0600 {
//         let color_idx = cpu.mem_read(i as u16);
//         let (b1, b2, b3) = color(color_idx).rgb();
//         if screen[screen_idx] != b1 || screen[screen_idx + 1] != b2 || screen[screen_idx + 2] != b3
//         {
//             screen[screen_idx] = b1;
//             screen[screen_idx + 1] = b2;
//             screen[screen_idx + 2] = b3;
//             updated = true
//         }
//         screen_idx += 3;
//     }

//     updated
// }

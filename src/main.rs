use cpu::Cpu;
use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::fs;
use std::process::exit;

mod addr_register;
mod apu;
mod bus;
mod cpu;
mod gamepad;
mod macros;
mod ops;
mod palette;
mod ppu;
mod render;
mod rom;
mod utility;

use bus::Bus;
use gamepad::GamepadButtons;
use gamepad::GamepadRegister;
use ppu::Ppu;
use render::Frame;
use rom::Rom;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;

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

    // Check usage
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("usage: nes-rust <rom.nes>");
        exit(1);
    }
    let program = fs::read(&args[1])?;

    // Bootstrap SDL (Graphics)
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;
    let window = video_subsystem
        .window("nes-rust", (256.0 * 3.0) as u32, (240.0 * 3.0) as u32)
        .position_centered()
        .build()?;

    let mut canvas = window.into_canvas().present_vsync().build()?;
    let mut event_pump = sdl_context.event_pump()?;
    canvas.set_scale(3., 3.)?;

    let creator = canvas.texture_creator();
    let mut texture = creator.create_texture_target(PixelFormatEnum::RGB24, 256, 240)?;

    // Setup the CPU to run the program
    let rom = Rom::new(&program);
    let mut cpu = Cpu::new();
    let keys = KeyboardInput::new();
    let bus = Bus::new_with_cb(
        rom,
        move |ppu: &Ppu, gamepad1: &mut GamepadRegister, _gamepad2: &mut GamepadRegister| {
            // compute the screen's content from the PPU
            let mut frame = Frame::new();
            ppu.draw_scrollable_background(&mut frame);
            ppu.draw_sprites(&mut frame);

            // redraw the screen
            texture.update(None, &frame.data, 256 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();

            // handle keyboard input
            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => std::process::exit(0),
                    Event::KeyDown {
                        keycode: Some(kc), ..
                    } => {
                        if let Some(&button) = keys.key_map.get(&kc) {
                            gamepad1.set_button_status(button, true);
                        }
                    }
                    Event::KeyUp {
                        keycode: Some(kc), ..
                    } => {
                        if let Some(&button) = keys.key_map.get(&kc) {
                            gamepad1.set_button_status(button, false);
                        }
                    }
                    _ => { /* do nothing */ }
                }
            }
        },
    );

    cpu.set_bus(bus);
    cpu.reset();

    // Run!
    cpu.run_with_callback(|cpu| {
        if env::var("CPU_TRACE").is_ok() {
            println!("{}", cpu.trace());
        } else if env::var("CPU_TRACELITE").is_ok() {
            println!("{}", cpu.tracelite());
        }
    });

    Ok(())
}

use crate::{palette::SYSTEM_PALETTE, ppu::Sprite};

pub struct Frame {
    pub data: Vec<u8>,
}

impl Frame {
    const WIDTH: usize = 256;
    const HEIGHT: usize = 240;

    pub fn new() -> Self {
        Self {
            data: vec![0; Frame::WIDTH * Frame::HEIGHT * 3],
        }
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
        let base = y * 3 * Frame::WIDTH + x * 3;
        if base + 2 < self.data.len() {
            self.data[base] = rgb.0;
            self.data[base + 1] = rgb.1;
            self.data[base + 2] = rgb.2
        }
    }

    // tile_n can be thought of as the offset in the pattern table  (CHR ROM)
    // pos (*8) relates to the value in the name table.
    pub fn draw_bg_tile(
        &mut self,
        chr_rom: &[u8],
        bank: usize,
        tile_n: usize,
        pos: (usize, usize),
        palette: [u8; 4],
    ) {
        assert!(bank <= 1);

        let tile_size_bytes = 16;
        let bank_size_bytes: usize = 4096;
        let tile_size = 8;

        let (x, y) = pos;
        for row in 0..tile_size {
            let first_byte_idx = bank * bank_size_bytes + tile_n * tile_size_bytes + row;
            let first_byte = chr_rom[first_byte_idx];
            let second_byte_idx = bank * bank_size_bytes + tile_n * tile_size_bytes + 8 + row;
            let second_byte = chr_rom[second_byte_idx];

            if x == 0 && y == 0 {
                log::info!(
                    "(x:{:2},y:{:2}) 1: [{:04x}] {:04x} , 2: [{:04x}] {:04x}",
                    x,
                    y,
                    first_byte_idx,
                    first_byte,
                    second_byte_idx,
                    second_byte
                );
            }

            for col in 0..tile_size {
                let which_bit = 1 << (7 - col);
                let lo_bit = first_byte & which_bit > 0;
                let hi_bit = second_byte & which_bit > 0;
                let palette_idx: u8 = ((hi_bit as u8) << 1) + (lo_bit as u8);
                assert!(palette_idx < 4, "palette_idx was {}", palette_idx);
                let color = SYSTEM_PALETTE[palette[palette_idx as usize] as usize];

                self.set_pixel((x * tile_size) + col, (y * tile_size) + row, color);
            }
        }
    }

    pub fn draw_sprite(&mut self, chr_rom: &[u8], sprite: &Sprite, palette: [u8; 4]) -> bool {
        if sprite.behind_background {
            return false;
        }

        let bank = if sprite.use_tile_bank_1 { 1 } else { 0 };
        let tile_n = sprite.tile_idx as usize;
        let x = sprite.x as usize;
        let y = sprite.y as usize;

        let tile_size_bytes = 16;
        let bank_size_bytes: usize = 4096;
        let tile_size = 8;

        let mut drew_something = false;
        for row in 0..tile_size {
            let first_byte_idx = bank * bank_size_bytes + tile_n * tile_size_bytes + row;
            let first_byte = chr_rom[first_byte_idx];
            // let second_byte_idx = bank * bank_size_bytes + tile_n * tile_size_bytes + row + 8;
            let second_byte = chr_rom[first_byte_idx + 8];

            for col in 0..tile_size {
                let which_bit = 1 << (7 - col);
                let lo_bit = first_byte & which_bit > 0;
                let hi_bit = second_byte & which_bit > 0;
                let palette_idx: u8 = ((hi_bit as u8) << 1) + (lo_bit as u8);
                assert!(palette_idx < 4);

                // 0 means transparent, for sprites
                if palette_idx > 0 {
                    drew_something = true;
                    let color = SYSTEM_PALETTE[palette[palette_idx as usize] as usize];
                    self.set_pixel(x + col, y + row, color);
                }
            }
        }

        drew_something
    }
}

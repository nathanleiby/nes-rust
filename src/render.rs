use crate::{
    palette::SYSTEM_PALETTE,
    ppu::{Sprite, PATTERN_TABLE_SIZE},
};

pub struct Frame {
    pub data: Vec<u8>,
}

/// Size of a single 8x8 Tile (in bytes).
/// 2 bytes for each row of 8 pixels.
const TILE_SIZE_BYTES: usize = 16;

/// Dimension of each side of the regular, 8x8 Tile.
const TILE_SIZE_PIXELS: usize = 8;

/// Get Tile
/// Does the "Rendering CHR ROM Tiles" logic from here
/// https://bugzmanov.github.io/nes_ebook/chapter_6_3.html
pub fn get_tile(pattern_table: &[u8], tile_n: usize) -> [[u8; 8]; 8] {
    let mut out = [[0; 8]; 8];

    for row in 0..TILE_SIZE_PIXELS {
        let first_byte_idx = tile_n * TILE_SIZE_BYTES + row;
        let first_byte = pattern_table[first_byte_idx];
        let second_byte = pattern_table[first_byte_idx + 8];

        for col in 0..TILE_SIZE_PIXELS {
            let which_bit = 1 << (7 - col);
            let lo_bit = first_byte & which_bit > 0;
            let hi_bit = second_byte & which_bit > 0;
            let palette_idx: u8 = ((hi_bit as u8) << 1) + (lo_bit as u8);
            assert!(palette_idx < 4, "palette_idx was {}", palette_idx);
            out[row][col] = palette_idx;
        }
    }

    out
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
        pattern_table: &[u8],
        tile_n: usize,
        pos: (usize, usize),
        palette: [u8; 4],
    ) {
        let tile = get_tile(&pattern_table, tile_n);
        let (tile_x, tile_y) = pos;
        for (row, row_data) in tile.iter().enumerate() {
            for (col, &palette_idx) in row_data.iter().enumerate() {
                let color = SYSTEM_PALETTE[palette[palette_idx as usize] as usize];
                self.set_pixel(
                    (tile_x * TILE_SIZE_PIXELS) + col,
                    (tile_y * TILE_SIZE_PIXELS) + row,
                    color,
                );
            }
        }
    }

    pub fn draw_sprite(&mut self, chr_rom: &[u8], sprite: &Sprite, palette: [u8; 4]) {
        if sprite.behind_background {
            return;
        }

        // Lookup the tile
        let tile_n = sprite.tile_idx as usize;
        let pt_idx = if sprite.use_pattern_table_1 { 1 } else { 0 };
        let pattern_table =
            &chr_rom[pt_idx * PATTERN_TABLE_SIZE..(pt_idx + 1) * PATTERN_TABLE_SIZE];
        let tile = get_tile(&pattern_table, tile_n);

        // Draw the tile
        let x = sprite.x as usize;
        let y = sprite.y as usize;
        let v_range = if sprite.flip_vertical {
            (0..TILE_SIZE_PIXELS).rev().collect::<Vec<usize>>()
        } else {
            (0..TILE_SIZE_PIXELS).collect::<Vec<usize>>()
        };
        for row in v_range {
            let h_range = if sprite.flip_horizontal {
                (0..TILE_SIZE_PIXELS).rev().collect::<Vec<usize>>()
            } else {
                (0..TILE_SIZE_PIXELS).collect::<Vec<usize>>()
            };
            for col in h_range {
                // 0 means transparent, for sprites
                let palette_idx = tile[row][col];
                if palette_idx > 0 {
                    let color = SYSTEM_PALETTE[palette[palette_idx as usize] as usize];
                    self.set_pixel(x + col, y + row, color);
                }
            }
        }
    }
}

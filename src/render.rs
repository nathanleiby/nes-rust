use crate::{
    palette::SYSTEM_PALETTE,
    ppu::{Sprite, PATTERN_TABLE_SIZE},
};

/// Debug flags
const DEBUG_DRAW_SPRITE_BOUNDING_BOX: bool = false;
// const DEBUG_DRAW_SPRITE_BOUNDING_BOX: bool = true;

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

    #[allow(clippy::needless_range_loop)]
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

pub struct Frame {
    pub data: Vec<u8>,
}

impl Frame {
    const WIDTH: usize = 256;
    const HEIGHT: usize = 240;

    pub fn new() -> Self {
        Self {
            // TODO: Consider a 'double-sized' frame with a viewport() function
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
        x_scroll: usize,
        y_scroll: usize,
        palette: [u8; 4],
    ) {
        let tile = get_tile(pattern_table, tile_n);
        let (tile_x, tile_y) = pos;
        for (row, row_data) in tile.iter().enumerate() {
            for (col, &palette_idx) in row_data.iter().enumerate() {
                let color = SYSTEM_PALETTE[palette[palette_idx as usize] as usize];
                let left_x = (tile_x * TILE_SIZE_PIXELS) + col;
                let top_y = (tile_y * TILE_SIZE_PIXELS) + row;
                // only draw pixels within the screen's viewport
                if x_scroll <= left_x
                    && left_x - x_scroll <= Frame::WIDTH
                    && y_scroll <= top_y
                    && top_y - y_scroll <= Frame::HEIGHT
                {
                    self.set_pixel(left_x - x_scroll, top_y - y_scroll, color);
                }
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
        let tile = get_tile(pattern_table, tile_n);

        // Draw the tile
        let x = sprite.x as usize;
        let y = sprite.y as usize;
        #[allow(clippy::needless_range_loop)]
        for row in 0..TILE_SIZE_PIXELS {
            for col in 0..TILE_SIZE_PIXELS {
                // 0 means transparent, for sprites
                let palette_idx = tile[row][col];
                if palette_idx > 0 {
                    let color = SYSTEM_PALETTE[palette[palette_idx as usize] as usize];

                    let y_offset = match sprite.flip_vertical {
                        true => TILE_SIZE_PIXELS - row - 1,
                        false => row,
                    };
                    let x_offset = match sprite.flip_horizontal {
                        true => TILE_SIZE_PIXELS - col - 1,
                        false => col,
                    };
                    self.set_pixel(x + x_offset, y + y_offset, color);
                }

                if DEBUG_DRAW_SPRITE_BOUNDING_BOX {
                    // Debug sprite by drawing a bounding box
                    let horiz_edge = row == 0 || row == TILE_SIZE_PIXELS - 1;
                    let vert_edge = col == 0 || col == TILE_SIZE_PIXELS - 1;
                    if horiz_edge || vert_edge {
                        // default = red
                        // flip v = yellow
                        // flip h = purple
                        // flip v+h = white
                        let color = (
                            255,
                            if sprite.flip_vertical && vert_edge {
                                255
                            } else {
                                0
                            },
                            if sprite.flip_horizontal && horiz_edge {
                                255
                            } else {
                                0
                            },
                        );
                        self.set_pixel(x + col, y + row, color);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_draw_bg_tile_default_color() {
        // draw a tile 0 in the top-left with default background color
        let pattern_table = [0; PATTERN_TABLE_SIZE];
        let tile_n = 0;
        let pos = (0, 0);
        let palette = [0, 0, 0, 0];

        let mut f = Frame::new();
        assert_eq!(f.data[0], 0);
        f.draw_bg_tile(&pattern_table, tile_n, pos, 0, 0, palette);
        assert_eq!(f.data[0], 128);
        assert_eq!(f.data[1], 128);
        assert_eq!(f.data[2], 128);
    }

    #[test]
    fn test_draw_bg_tile_single_color_lookup() {
        let mut pattern_table = [0; PATTERN_TABLE_SIZE];
        let tile_n = 1;
        for i in 0..8 {
            pattern_table[(tile_n * TILE_SIZE_BYTES) + i] = 0xff;
            pattern_table[(tile_n * TILE_SIZE_BYTES) + i + 8] = 0xff;
        }

        let pos = (3, 0);
        let palette = [65, 65, 65, 3]; // 65 should crash if read, since it's OOB the palette with 64 colors

        let mut f = Frame::new();
        f.draw_bg_tile(&pattern_table, tile_n, pos, 0, 0, palette);

        // verify we drew the entire tile in the one selected color
        for row in 0..8 {
            for col in 0..8 {
                let pixels_per_row = 32 * 8;
                let rows = pos.1 + row;
                let cols = pos.0 * 8 + col;
                let start = (pixels_per_row * rows + cols) * 3;

                let color = (0x44, 0x00, 0x96);
                assert_eq!(f.data[start], color.0);
                assert_eq!(f.data[start + 1], color.1);
                assert_eq!(f.data[start + 2], color.2);
            }
        }
    }

    #[test]
    fn test_draw_sprite() {
        let mut pattern_table = [0; PATTERN_TABLE_SIZE];
        let tile_n = 1;
        for i in 0..8 {
            pattern_table[(tile_n * TILE_SIZE_BYTES) + i] = 0xff;
            pattern_table[(tile_n * TILE_SIZE_BYTES) + i + 8] = 0xff;
        }

        let pos = (3, 0);
        let palette = [65, 65, 65, 3]; // 65 should crash if read, since it's OOB the palette with 64 colors

        let mut f = Frame::new();
        let sprite = Sprite {
            tile_idx: 1,
            // *8 to convert from an (x,y) to a (tile_x, tile_y)
            x: pos.0 * 8,
            y: pos.1 * 8,
            ..Default::default()
        };
        f.draw_sprite(&pattern_table, &sprite, palette);

        // verify we drew the entire tile in the one selected color
        for row in 0..8 {
            for col in 0..8 {
                let pixels_per_row = 32 * 8;
                let rows = (pos.1 as usize) + row;
                let cols = (pos.0 as usize) * 8 + col;
                let start = (pixels_per_row * rows + cols) * 3;

                let color = (0x44, 0x00, 0x96);
                assert_eq!(f.data[start], color.0);
                assert_eq!(f.data[start + 1], color.1);
                assert_eq!(f.data[start + 2], color.2);
            }
        }
    }

    #[test]
    fn test_draw_sprite_behind_background() {
        let mut pattern_table = [0; PATTERN_TABLE_SIZE];
        let tile_n = 1;
        for i in 0..8 {
            pattern_table[(tile_n * TILE_SIZE_BYTES) + i] = 0xff;
            pattern_table[(tile_n * TILE_SIZE_BYTES) + i + 8] = 0xff;
        }

        let palette = [65, 65, 65, 3]; // 65 should crash if read, since it's OOB the palette with 64 colors

        for behind_background in [true, false] {
            let mut f = Frame::new();
            let sprite = Sprite {
                tile_idx: 1,
                behind_background,
                ..Default::default()
            };
            f.draw_sprite(&pattern_table, &sprite, palette);

            // we don't draw anything if sprite is behind background
            // the actual desired behavior is more complex, with transparent  transparent tiles allowed
            assert_eq!(f.data.iter().all(|&x| x == 0), behind_background);
        }
    }
}
